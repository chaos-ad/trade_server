-record(server_status, {
    id,
    connected,
    recover
}).

-record(client,  {
    id,
    type,
    currency,
    ml_call,
    ml_close,
    ml_intraday,
    ml_overnight,
    ml_restrict
}).

-record(market, {id, name}).
-record(candlekind, {id, period, name}).
-record(candle, {date, open, close, high, low, volume}).

-record(money_position, {
    asset,
    client,
    shortname,
    saldoin,
    bought,
    sold,
    saldo,
    ordbuy,
    ordbuycond,
    commission
}).

-record(sec_position, {
    client,
    secid,
    shortname,
    saldoin,
    saldomin,
    bought,
    sold,
    saldo,
    ordbuy,
    ordsell
}).

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

-record(trade, {
    secid,
    tradeno,
    orderno,
    board,
    client,
    buysell,
    time,
    brokerref,
    value,
    comission,
    price,
    quantity,
    yield,
    currentpos,
    accruedint,
    tradetype,
    settlecode
}).

-record(order, {
    transactionid,
    orderno,
    secid,
    board,
    client,
    status,
    buysell,
    time,
    accepttime,
    brokerref,
    value,
    accruedint,
    settlecode,
    balance,
    price,
    quantity,
    yield,
    withdrawtime,
    condition,
    conditionvalue,
    validafter,
    validbefore,
    maxcomission,
    result
}).


-record(terminal_state, {
    server_status=#server_status{},
    clients=[],
    markets=[],
    candlekinds=[],
    securities=[],
    positions=[],
    trades=[],
    orders=[],
    overnight
}).