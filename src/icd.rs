use postcard_rpc::{endpoints, topics, TopicDirection};

endpoints! {
    list = ENDPOINT_LIST;
    omit_std = true;
    | EndpointTy                | RequestTy     | ResponseTy            | Path              |
    | ----------                | ---------     | ----------            | ----              |
    | PingEndpoint              | u32           | u32                   | "ping"            |
}

topics! {
    list = TOPICS_IN_LIST;
    direction = TopicDirection::ToServer;
    | TopicTy                   | MessageTy     | Path              |
    | -------                   | ---------     | ----              |
}

topics! {
    list = TOPICS_OUT_LIST;
    direction = TopicDirection::ToClient;
    | TopicTy                   | MessageTy     | Path              | Cfg                           |
    | -------                   | ---------     | ----              | ---                           |
}
