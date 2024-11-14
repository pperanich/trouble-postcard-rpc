#![no_std]
#![no_main]
use defmt::{info, warn};
use embassy_executor::Spawner;
use embassy_futures::join::join;
use embassy_sync::blocking_mutex::raw::ThreadModeRawMutex;
use embassy_sync::mutex::Mutex;
use microbit_bsp::ble::{MultiprotocolServiceLayer, SoftdeviceController};
use microbit_bsp::Microbit;
use postcard_rpc::{
    define_dispatch,
    header::VarHeader,
    server::{Dispatch, Server, SpawnContext},
};
use static_cell::StaticCell;
use trouble_host::prelude::*;
use {defmt_rtt as _, panic_probe as _};

use trouble_postcard_rpc::{
    advertiser::AdvertiserBuilder,
    ble_task,
    icd::{PingEndpoint, ENDPOINT_LIST, TOPICS_IN_LIST, TOPICS_OUT_LIST},
    impls::l2cap::{
        dispatch_impl::{new_server, WireRxBuf, WireRxImpl, WireTxImpl},
        L2CapWireRx, L2CapWireSpawn, L2CapWireTx, L2CapWireTxInner,
    },
    mpsl_task, SdcResources,
};

// Postcard types
type MutexType = ThreadModeRawMutex;
type AppTx<'d> = WireTxImpl<'d, MutexType, AppDriver>;
type AppRx<'d> = WireRxImpl<'d, AppDriver>;
type AppServer<'d> = Server<AppTx<'d>, AppRx<'d>, WireRxBuf, MicrobitL2CapApp>;
type AppDriver = SoftdeviceController<'static>;

fn ping_handler(_context: &mut Context, _header: VarHeader, rqst: u32) -> u32 {
    info!("ping");
    rqst
}

pub struct Context {}

pub struct SpawnCtx {}

impl SpawnContext for Context {
    type SpawnCtxt = SpawnCtx;
    fn spawn_ctxt(&mut self) -> Self::SpawnCtxt {
        SpawnCtx {}
    }
}
define_dispatch! {
    app: MicrobitL2CapApp;
    spawn_fn: embassy_spawn;
    tx_impl: AppTx;
    spawn_impl: L2CapWireSpawn;
    context: Context;

    endpoints: {
        list: ENDPOINT_LIST;

        | EndpointTy                | kind      | handler                       |
        | ----------                | ----      | -------                       |
        | PingEndpoint              | blocking  | ping_handler                  |
    };
    topics_in: {
        list: TOPICS_IN_LIST;

        | TopicTy                   | kind      | handler                       |
        | ----------                | ----      | -------                       |
    };
    topics_out: {
        list: TOPICS_OUT_LIST;
    };
}

#[embassy_executor::task]
pub async fn run_l2cap_rpc(
    spawner: Spawner,
    sdc: SoftdeviceController<'static>,
    mpsl: &'static MultiprotocolServiceLayer<'static>,
    app_context: Context,
) {
    spawner.must_spawn(mpsl_task(mpsl));

    let pbufs = PBUFS.take();
    let address: Address = Address::random([0xff, 0x8f, 0x1a, 0x05, 0xe4, 0xff]);
    info!("Our address = {:?}", address);

    let resources = {
        static RESOURCES: StaticCell<SdcResources<'_>> = StaticCell::new();
        RESOURCES.init(SdcResources::new(PacketQos::None))
    };
    let (stack, mut peripheral, _, mut runner) = trouble_host::new(sdc, resources)
        .set_random_address(address)
        .build();
    spawner.must_spawn(ble_task(runner));
    let stack = {
        static STACK: StaticCell<Stack<'static, SoftdeviceController<'static>>> = StaticCell::new();
        STACK.init(stack)
    };

    let advertiser = AdvertiserBuilder::new("microbit-trouble", peripheral)
        .build()
        .unwrap();

    loop {
        let conn = advertiser.advertise().await.unwrap();

        info!("Connection established");

        let mut ch1 = L2capChannel::accept(*stack, &conn, &[0x2349], &Default::default())
            .await
            .unwrap();

        info!("L2CAP channel accepted");

        let dispatcher = MicrobitL2CapApp::new(app_context, spawner.into());

        let server = new_server(dispatcher, stack, ch1);

        server.run().await;

        warn!("Exiting run_ble_rpc!!")
    }
}

// Application main entry point. The spawner can be used to start async tasks.
#[embassy_executor::main]
async fn main(spawner: Spawner) {
    // First we initialize our board.
    let board = Microbit::new(Default::default());
    let app_context = Context {};
    let (sdc, mpsl) = board
        .ble
        .init(board.timer0, board.rng)
        .expect("Failed to initialize BLE.");

    spawner.must_spawn(run_l2cap_rpc(spawner, sdc, mpsl, app_context));
}
