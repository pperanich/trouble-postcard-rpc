use static_cell::ConstStaticCell;

pub mod l2cap;

// Statics
pub static PBUFS: ConstStaticCell<BufStorage> = ConstStaticCell::new(BufStorage::new());
pub type BufStorage = PacketBuffers<1024, 1024>;

/// Static storage for generically sized input and output packet buffers
pub struct PacketBuffers<const TX: usize = 1024, const RX: usize = 1024> {
    /// the transmit buffer
    pub tx_buf: [u8; TX],
    /// thereceive buffer
    pub rx_buf: [u8; RX],
}

impl<const TX: usize, const RX: usize> PacketBuffers<TX, RX> {
    /// Create new empty buffers
    pub const fn new() -> Self {
        Self {
            tx_buf: [0u8; TX],
            rx_buf: [0u8; RX],
        }
    }
}
