-record(server_status, {
    id,
    connected,
    recover
}).

-record(client,  {
    type,
    currency,
    ml_call,
    ml_close,
    ml_intraday,
    ml_overnight,
    ml_restrict
}).

-record(market, {id, name}).
-record(candle, {id, period, name}).

-record(security, {
    secid,
    sectype,
    active,
    seccode,
    market,
    shortname,
    decimals,
    minstep,
    lotsize,
    point_cost,
    usecredit,
    bymarket,
    nosplit,
    immorcancel,
    cancelbalance
}).

-record(terminal_state, {
    server_status=#server_status{},
    client=#client{},
    markets=[],
    securities=[],
    candles=[]
}).