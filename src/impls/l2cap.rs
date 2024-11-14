use core::fmt::Arguments;
use embassy_executor::{SpawnError, SpawnToken, Spawner};
use embassy_sync::blocking_mutex::raw::RawMutex;
use embassy_sync::mutex::Mutex;
use postcard_rpc::{
    header::{VarHeader, VarKey, VarKeyKind, VarSeq},
    server::{WireRx, WireRxErrorKind, WireSpawn, WireTx, WireTxErrorKind},
    standard_icd::LoggingTopic,
    Topic,
};
use serde::Serialize;
use trouble_host::prelude::*;

/// A collection of types and aliases useful for importing the correct types
pub mod dispatch_impl {
    #![allow(unused)]
    pub use super::embassy_spawn as spawn_fn;
    use super::{L2CapWireRx, L2CapWireSpawn, L2CapWireTx};
    use crate::impls::PBUFS;
    use embassy_sync::{
        blocking_mutex::raw::{RawMutex, ThreadModeRawMutex},
        mutex::Mutex,
    };
    use microbit_bsp::ble::SoftdeviceController;
    use postcard_rpc::server::{Dispatch, Server};
    use static_cell::{ConstStaticCell, StaticCell};
    use trouble_host::prelude::{L2capChannel, Stack};

    /// Type alias for `WireTx` impl
    pub type WireTxImpl<'d, M, C> = super::L2CapWireTx<'d, M, C>;
    /// Type alias for `WireRx` impl
    pub type WireRxImpl<'d, C> = super::L2CapWireRx<'d, C>;
    /// Type alias for `WireSpawn` impl
    pub type WireSpawnImpl = super::L2CapWireSpawn;
    /// Type alias for the receive buffer
    pub type WireRxBuf = &'static mut [u8];

    /// Create a new server using the [`Settings`] and [`Dispatch`] implementation
    pub fn new_server<'a, D>(
        dispatch: D,
        stack: &'static Stack<'static, SoftdeviceController<'static>>,
        chan: L2capChannel<'a>,
    ) -> Server<
        WireTxImpl<'a, ThreadModeRawMutex, SoftdeviceController<'static>>,
        WireRxImpl<'a, SoftdeviceController<'static>>,
        WireRxBuf,
        D,
    >
    where
        D: Dispatch<Tx = WireTxImpl<'a, ThreadModeRawMutex, SoftdeviceController<'static>>>,
    {
        let vkk = dispatch.min_key_len();
        let pbufs = PBUFS.take();
        let tx_impl_inner = super::L2CapWireTxInner {
            stack,
            log_seq: 0,
            tx_buf: pbufs.tx_buf.as_mut_slice(),
        };
        let tx_impl_inner_wtx = {
            static TX_IMPL_INNER: StaticCell<
                Mutex<ThreadModeRawMutex, super::L2CapWireTxInner<SoftdeviceController<'static>>>,
            > = StaticCell::new();
            TX_IMPL_INNER.init(Mutex::new(tx_impl_inner))
        };
        let tx_impl = super::L2CapWireTx {
            chan: chan.clone(),
            inner: tx_impl_inner_wtx,
        };
        let rx_impl = super::L2CapWireRx {
            stack,
            chan: chan.clone(),
        };
        let server = Server::new(
            &tx_impl,
            rx_impl,
            pbufs.rx_buf.as_mut_slice(),
            dispatch,
            vkk,
        );

        server
    }
}

//////////////////////////////////////////////////////////////////////////////
// TX
//////////////////////////////////////////////////////////////////////////////

/// Implementation detail, holding the endpoint and scratch buffer used for sending
pub struct L2CapWireTxInner<C: Controller + 'static> {
    stack: &'static Stack<'static, C>,
    log_seq: u16,
    tx_buf: &'static mut [u8],
}

/// A [`WireTx`] implementation for trouble l2cap channel.
// #[derive(Clone)]
pub struct L2CapWireTx<'d, M: RawMutex + 'static, C: Controller + 'static> {
    chan: L2capChannel<'d>,
    inner: &'static Mutex<M, L2CapWireTxInner<C>>,
}
impl<'d, M: RawMutex + 'static, C: Controller + 'static> Clone for L2CapWireTx<'d, M, C> {
    fn clone(&self) -> Self {
        L2CapWireTx {
            chan: self.chan.clone(),
            inner: self.inner,
        }
    }
}

impl<'d, M: RawMutex + 'static, C: Controller + 'static> WireTx for L2CapWireTx<'d, M, C> {
    type Error = WireTxErrorKind;

    async fn send<T: Serialize + ?Sized>(
        &self,
        hdr: VarHeader,
        msg: &T,
    ) -> Result<(), Self::Error> {
        let mut inner = self.inner.lock().await;
        let L2CapWireTxInner {
            stack,
            log_seq: _,
            tx_buf,
        }: &mut L2CapWireTxInner<C> = &mut inner;

        let (hdr_used, remain) = hdr.write_to_slice(tx_buf).ok_or(WireTxErrorKind::Other)?;
        let bdy_used = postcard::to_slice(msg, remain).map_err(|_| WireTxErrorKind::Other)?;
        let used_ttl = hdr_used.len() + bdy_used.len();

        if let Some(used) = tx_buf.get(..used_ttl) {
            send_all::<C>(stack, self.chan.clone(), used).await
        } else {
            Err(WireTxErrorKind::Other)
        }
    }

    async fn send_raw(&self, buf: &[u8]) -> Result<(), Self::Error> {
        let mut inner = self.inner.lock().await;
        let L2CapWireTxInner {
            stack, log_seq: _, ..
        }: &mut L2CapWireTxInner<C> = &mut inner;

        send_all::<C>(stack, self.chan.clone(), buf).await
    }

    async fn send_log_str(&self, kkind: VarKeyKind, s: &str) -> Result<(), Self::Error> {
        let mut inner = self.inner.lock().await;
        let L2CapWireTxInner {
            stack,
            log_seq,
            tx_buf,
        }: &mut L2CapWireTxInner<C> = &mut inner;

        let key = match kkind {
            VarKeyKind::Key1 => VarKey::Key1(LoggingTopic::TOPIC_KEY1),
            VarKeyKind::Key2 => VarKey::Key2(LoggingTopic::TOPIC_KEY2),
            VarKeyKind::Key4 => VarKey::Key4(LoggingTopic::TOPIC_KEY4),
            VarKeyKind::Key8 => VarKey::Key8(LoggingTopic::TOPIC_KEY),
        };
        let ctr = *log_seq;
        *log_seq = log_seq.wrapping_add(1);
        let wh = VarHeader {
            key,
            seq_no: VarSeq::Seq2(ctr),
        };

        let (hdr_used, remain) = wh.write_to_slice(tx_buf).ok_or(WireTxErrorKind::Other)?;
        let bdy_used = postcard::to_slice::<str>(s, remain).map_err(|_| WireTxErrorKind::Other)?;
        let used_ttl = hdr_used.len() + bdy_used.len();

        if let Some(used) = tx_buf.get(..used_ttl) {
            send_all::<C>(stack, self.chan.clone(), used).await
        } else {
            Err(WireTxErrorKind::Other)
        }
    }

    async fn send_log_fmt<'a>(
        &self,
        kkind: VarKeyKind,
        args: Arguments<'a>,
    ) -> Result<(), Self::Error> {
        let mut inner = self.inner.lock().await;
        let L2CapWireTxInner {
            stack,
            log_seq,
            tx_buf,
        }: &mut L2CapWireTxInner<C> = &mut inner;

        let ttl_len = tx_buf.len();

        let key = match kkind {
            VarKeyKind::Key1 => VarKey::Key1(LoggingTopic::TOPIC_KEY1),
            VarKeyKind::Key2 => VarKey::Key2(LoggingTopic::TOPIC_KEY2),
            VarKeyKind::Key4 => VarKey::Key4(LoggingTopic::TOPIC_KEY4),
            VarKeyKind::Key8 => VarKey::Key8(LoggingTopic::TOPIC_KEY),
        };
        let ctr = *log_seq;
        *log_seq = log_seq.wrapping_add(1);
        let wh = VarHeader {
            key,
            seq_no: VarSeq::Seq2(ctr),
        };
        let Some((_hdr, remaining)) = wh.write_to_slice(tx_buf) else {
            return Err(WireTxErrorKind::Other);
        };
        let max_log_len = actual_varint_max_len(remaining.len());

        // Then, reserve space for non-canonical length fields
        // We also set all but the last bytes to be "continuation"
        // bytes
        if remaining.len() < max_log_len {
            return Err(WireTxErrorKind::Other);
        }

        let (len_field, body) = remaining.split_at_mut(max_log_len);
        for b in len_field.iter_mut() {
            *b = 0x80;
        }
        if let Some(b) = len_field.last_mut() {
            *b = 0x00;
        }

        // Then, do the formatting
        let body_len = body.len();
        let mut sw = SliceWriter(body);
        let res = core::fmt::write(&mut sw, args);

        // Calculate the number of bytes used *for formatting*.
        let remain = sw.0.len();
        let used = body_len - remain;

        // If we had an error, that's probably because we ran out
        // of room. If we had an error, AND there is at least three
        // bytes, then replace those with '.'s like ...
        if res.is_err() && (body.len() >= 3) {
            let start = body.len() - 3;
            body[start..].iter_mut().for_each(|b| *b = b'.');
        }

        // then go back and fill in the len - we write the len
        // directly to the reserved bytes, and if we DIDN'T use
        // the full space, we mark the end of the real length as
        // a continuation field. This will result in a non-canonical
        // "extended" length in postcard, and will "spill into" the
        // bytes we wrote previously above
        let mut len_bytes = [0u8; varint_max::<usize>()];
        let len_used = varint_usize(used, &mut len_bytes);
        if len_used.len() != len_field.len() {
            if let Some(b) = len_used.last_mut() {
                *b |= 0x80;
            }
        }
        len_field[..len_used.len()].copy_from_slice(len_used);

        // Calculate the TOTAL amount
        let act_used = ttl_len - remain;

        send_all::<C>(stack, self.chan.clone(), &tx_buf[..act_used]).await
    }
}

#[inline]
async fn send_all<C: Controller>(
    stack: &Stack<'_, C>,
    mut chan: L2capChannel<'_>,
    out: &[u8],
) -> Result<(), WireTxErrorKind> {
    if out.is_empty() {
        return Ok(());
    }

    // write in segments of 64. The last chunk may
    // be 0 < len <= 64.
    for ch in out.chunks(64) {
        if chan.send::<_, 64>(*stack, &ch).await.is_err() {
            return Err(WireTxErrorKind::ConnectionClosed);
        }
    }
    Ok(())
}

struct SliceWriter<'a>(&'a mut [u8]);

impl<'a> core::fmt::Write for SliceWriter<'a> {
    fn write_str(&mut self, s: &str) -> Result<(), core::fmt::Error> {
        let sli = core::mem::take(&mut self.0);

        // If this write would overflow us, note that, but still take
        // as much as we possibly can here
        let bad = s.len() > sli.len();
        let to_write = s.len().min(sli.len());
        let (now, later) = sli.split_at_mut(to_write);
        now.copy_from_slice(s.as_bytes());
        self.0 = later;

        // Now, report whether we overflowed or not
        if bad {
            Err(core::fmt::Error)
        } else {
            Ok(())
        }
    }
}

/// Returns the maximum number of bytes required to encode T.
const fn varint_max<T: Sized>() -> usize {
    const BITS_PER_BYTE: usize = 8;
    const BITS_PER_VARINT_BYTE: usize = 7;

    // How many data bits do we need for this type?
    let bits = core::mem::size_of::<T>() * BITS_PER_BYTE;

    // We add (BITS_PER_VARINT_BYTE - 1), to ensure any integer divisions
    // with a remainder will always add exactly one full byte, but
    // an evenly divided number of bits will be the same
    let roundup_bits = bits + (BITS_PER_VARINT_BYTE - 1);

    // Apply division, using normal "round down" integer division
    roundup_bits / BITS_PER_VARINT_BYTE
}

#[inline]
fn varint_usize(n: usize, out: &mut [u8; varint_max::<usize>()]) -> &mut [u8] {
    let mut value = n;
    for i in 0..varint_max::<usize>() {
        out[i] = value.to_le_bytes()[0];
        if value < 128 {
            return &mut out[..=i];
        }

        out[i] |= 0x80;
        value >>= 7;
    }
    debug_assert_eq!(value, 0);
    &mut out[..]
}

fn actual_varint_max_len(largest: usize) -> usize {
    if largest < (2 << 7) {
        1
    } else if largest < (2 << 14) {
        2
    } else if largest < (2 << 21) {
        3
    } else if largest < (2 << 28) {
        4
    } else {
        varint_max::<usize>()
    }
}

//////////////////////////////////////////////////////////////////////////////
// RX
//////////////////////////////////////////////////////////////////////////////

/// A [`WireRx`] implementation for trouble l2cap channel.
pub struct L2CapWireRx<'d, C: Controller + 'static> {
    stack: &'d Stack<'d, C>,
    chan: L2capChannel<'d>,
}

impl<'d, C: Controller + 'static> WireRx for L2CapWireRx<'d, C> {
    type Error = WireRxErrorKind;

    async fn receive<'a>(&mut self, buf: &'a mut [u8]) -> Result<&'a mut [u8], Self::Error> {
        let buflen = buf.len();
        let mut window = &mut buf[..];
        while !window.is_empty() {
            let n = match self.chan.receive(*self.stack, &mut window).await {
                Ok(n) => n,
                Err(_) => return Err(WireRxErrorKind::ReceivedMessageTooLarge), // Err(EndpointError::Disabled) => {
                                                                                //     return Err(WireRxErrorKind::ConnectionClosed)
                                                                                // }
            };

            let (_now, later) = window.split_at_mut(n);
            window = later;
            if n != 64 {
                // We now have a full frame! Great!
                let wlen = window.len();
                let len = buflen - wlen;
                let frame = &mut buf[..len];

                return Ok(frame);
            }
        }

        // If we got here, we've run out of space. That's disappointing. Accumulate to the
        // end of this packet
        loop {
            match self.chan.receive(*self.stack, buf).await {
                Ok(64) => {}
                Ok(_) => return Err(WireRxErrorKind::ReceivedMessageTooLarge),
                Err(_) => return Err(WireRxErrorKind::ReceivedMessageTooLarge), // Err(EndpointError::Disabled) => {
                                                                                //     return Err(WireRxErrorKind::ConnectionClosed)
                                                                                // }
            };
        }
    }
}

//////////////////////////////////////////////////////////////////////////////
// SPAWN
//////////////////////////////////////////////////////////////////////////////

/// A [`WireSpawn`] impl using the embassy executor
#[derive(Clone)]
pub struct L2CapWireSpawn {
    /// The embassy-executor spawner
    pub spawner: Spawner,
}

impl From<Spawner> for L2CapWireSpawn {
    fn from(value: Spawner) -> Self {
        Self { spawner: value }
    }
}

impl WireSpawn for L2CapWireSpawn {
    type Error = SpawnError;

    type Info = Spawner;

    fn info(&self) -> &Self::Info {
        &self.spawner
    }
}

/// Attempt to spawn the given token
pub fn embassy_spawn<Sp, S: Sized>(sp: &Sp, tok: SpawnToken<S>) -> Result<(), Sp::Error>
where
    Sp: WireSpawn<Error = SpawnError, Info = Spawner>,
{
    let info = sp.info();
    info.spawn(tok)
}
