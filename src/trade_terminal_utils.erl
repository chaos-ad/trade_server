-module(trade_terminal_utils).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("trade_terminal_state.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neworder_mode(buy) -> "B";
neworder_mode(sell) -> "S".

make_request(neworder, [Mode, SecurityID, ClientID, Amount]) ->
    Format =
    "<command id='neworder'>"
        "<secid>~B</secid>"
        "<client>~s</client>"
        "<quantity>~B</quantity>"
        "<buysell>~s</buysell>"
        "<unfilled>PutInQueue</unfilled>" %% CancelBalance, ImmOrCancel
        "<bymarket/>"
        "<nosplit/>"
    "</command>",
    io_lib:format(Format, [SecurityID, ClientID, Amount, neworder_mode(Mode)]);

make_request(cancelorder, Args) ->
    Format = "<command id='cancelorder'><transactionid>~B</transactionid></command>",
    io_lib:format(Format, Args);

make_request(login, Args) ->
    Format= "<command id='connect'>"
                "<login>~s</login><password>~s</password><host>~s</host><port>~B</port>"
                "<logsdir>./logs/</logsdir><loglevel>0</loglevel>"
                "<rqdelay>500</rqdelay><session_timeout>7</session_timeout><request_timeout>5</request_timeout>"
            "</command>",
    io_lib:format(Format, Args);

make_request(logout, _) ->
    "<command id='disconnect'/>";

make_request(server_status, _) ->
    "<command id='server_status'/>";

make_request(gethistorydata, Args) ->
    Format = "<command id='gethistorydata' secid='~B' period='~B' count='~B' reset='~p'/>",
    io_lib:format(Format, Args);

make_request(subscribe, Args) ->
    "<command id='subscribe'>" ++
        make_secids(quotes, Args) ++
        make_secids(alltrades, Args) ++
        make_secids(quotations, Args) ++
    "</command>";

make_request(unsubscribe, Args) ->
    "<command id='unsubscribe'>" ++
        make_secids(quotes, Args) ++
        make_secids(alltrades, Args) ++
        make_secids(quotations, Args) ++
    "</command>".

make_secid(ID) -> "<secid>" ++ integer_to_list(ID) ++ "</secid>".
make_secids(Mode, List) ->
    Secs = lists:map(fun({M, ID}) when M =:= Mode -> make_secid(ID); (_) -> [] end, List),
    lists:flatten(io_lib:format("<~p>~s</~p>", [Mode, Secs, Mode])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_record({candle,Attributes,[]}) ->
    #candle {
        date    = get_value(datetime, date, Attributes),
        open    = get_value(number, open, Attributes),
        close   = get_value(number, close, Attributes),
        high    = get_value(number, high, Attributes),
        low     = get_value(number, low, Attributes),
        volume  = get_value(number, volume, Attributes)
    };

make_record({order, Attributes, Args}) ->
    #order {
        transactionid   = get_value(integer, transactionid, Attributes),
        orderno         = get_value(integer, orderno, Args),
        secid           = get_value(integer, secid, Args),
        board           = get_value(board, Args),
        client          = get_value(client, Args),
        status          = get_value(status, Args),
        buysell         = get_value(buysell, Args),
        time            = get_value(time, Args),
        accepttime      = get_value(accepttime, Args),
        brokerref       = get_value(brokerref, Args),
        value           = get_value(number, value, Args),
        accruedint      = get_value(number, accruedint, Args),
        settlecode      = get_value(settlecode, Args),
        balance         = get_value(integer, balance, Args),
        price           = get_value(number, price, Args),
        quantity        = get_value(integer, quantity, Args),
        yield           = get_value(number, yield, Args),
        withdrawtime    = get_value(withdrawtime, Args),
        condition       = get_value(condition, Args),
        conditionvalue  = get_value(number, conditionvalue, Args),
        validafter      = get_value(validafter, Args),
        validbefore     = get_value(validbefore, Args),
        maxcomission    = get_value(integer, maxcomission, Args),
        result          = get_value(result, Args)
    };

make_record({trade, [], Args}) ->
    #trade{
        secid       = get_value(integer, secid, Args),
        tradeno     = get_value(integer, tradeno, Args),
        orderno     = get_value(integer, orderno, Args),
        board       = get_value(board, Args),
        client      = get_value(client, Args),
        buysell     = get_value(buysell, Args),
        time        = get_value(time, Args),
        brokerref   = get_value(brokerref, Args),
        value       = get_value(number, value, Args),
        comission   = get_value(number, comission, Args),
        price       = get_value(number, price, Args),
        quantity    = get_value(number, quantity, Args),
        yield       = get_value(number, yield, Args),
        currentpos  = get_value(number, currentpos, Args),
        accruedint  = get_value(number, accruedint, Args),
        tradetype   = get_value(tradetype, Args),
        settlecode  = get_value(settlecode, Args)
    };

make_record({money_position, [], Args}) ->
    #money_position{
        asset       = get_value(asset, Args),
        client      = get_value(client, Args),
        shortname   = get_value(shortname, Args),
        saldoin     = get_value(number, saldoin, Args),
        bought      = get_value(number, bought, Args),
        sold        = get_value(number, sold, Args),
        saldo       = get_value(number, saldo, Args),
        ordbuy      = get_value(number, ordbuy, Args),
        ordbuycond  = get_value(number, ordbuycond, Args),
        commission  = get_value(number, commission, Args)
    };

make_record({sec_position, [], Args}) ->
    #sec_position{
        client      = get_value(client, Args),
        secid       = get_value(integer, secid, Args),
        shortname   = get_value(shortname, Args),
        saldoin     = get_value(number, saldoin, Args),
        saldomin    = get_value(number, saldomin, Args),
        bought      = get_value(number, bought, Args),
        sold        = get_value(number, sold, Args),
        saldo       = get_value(number, saldo, Args),
        ordbuy      = get_value(number, ordbuy, Args),
        ordsell     = get_value(number, ordsell, Args)
    };

make_record({market,[{id,ID}],[Name]}) ->
    #market{id=list_to_integer(ID), name=Name};

make_record({kind,[],[{id,[],[ID]},{period,[],[Period]},{name,[],[Name]}]}) ->
    #candlekind{id=list_to_integer(ID), period=list_to_integer(Period), name=Name};

make_record({security, Attributes, Args}) ->

    {UseCredit, ByMarket, NoSplit, ImmOrCancel, CancelBalance} =
    case get_value(opmask, Args) of
        undefined            -> list_to_tuple(lists:duplicate(5, undefined));
        {opmask, OpMask, []} ->
            {get_value(usecredit,     OpMask) =:= "yes",
             get_value(bymarket,      OpMask) =:= "yes",
             get_value(nosplit,       OpMask) =:= "yes",
             get_value(immorcancel,   OpMask) =:= "yes",
             get_value(cancelbalance, OpMask) =:= "yes"}
    end,

    #security{  secid           = get_value(number, secid, Attributes),
                active          = get_value(atom, active, Attributes),
                sectype         = get_value(sectype, Args),
                seccode         = get_value(seccode, Args),
                market          = get_value(number, market, Args),
                shortname       = get_value(shortname, Args),
                decimals        = get_value(decimals, Args),
                minstep         = get_value(number, minstep, Args),
                lotsize         = get_value(number, lotsize, Args),
                point_cost      = get_value(number, point_cost, Args),
                usecredit       = UseCredit,
                bymarket        = ByMarket,
                nosplit         = NoSplit,
                immorcancel     = ImmOrCancel,
                cancelbalance   = CancelBalance
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_value(Name, Values) ->
    case lists:keyfind(Name, 1, Values) of
        false               -> undefined;
        {Name, Value}       -> Value;
        {Name, [], []}      -> undefined;
        {Name, [], [Value]} -> Value;
        {Name, [], [Value1, Value2]} -> Value1 ++ Value2;
        Other               -> Other
    end.

get_value(datetime, Name, Values) ->
    case get_value(Name, Values) of
        undefined -> undefined;
        Result    -> parse_datetime(Result)
    end;

get_value(atom, Name, Values) ->
    case get_value(Name, Values) of
        undefined -> undefined;
        Result    -> list_to_atom(Result)
    end;

get_value(integer, Name, Values) ->
    case get_value(Name, Values) of
        undefined -> undefined;
        Result    -> list_to_integer(Result)
    end;

get_value(number, Name, Values) ->
    case get_value(Name, Values) of
        undefined -> undefined;
        Result    -> list_to_number(Result)
    end.

list_to_number([C, $e | Value]) when C >= $0 andalso C =< $9 ->
    list_to_number([C, $., $0, $e | Value]);

list_to_number(Value) ->
    try list_to_float(Value)
    catch
        error:badarg -> list_to_integer(Value)
    end.

parse(Bin) when is_binary(Bin) ->
    case xmerl_scan:string(binary_to_list(Bin)) of
        {Result, []} -> xmerl_lib:simplify_element(Result);
        _            -> exit(parse_error)
    end.

parse_datetime(DateTime) ->
    {Date, Time} = lists:split(3, string:tokens(DateTime, [$., $:, $ ])),
    DateParsed = list_to_tuple(lists:reverse(lists:map(fun list_to_integer/1, Date))),
    TimeParsed = list_to_tuple(lists:map(fun list_to_integer/1, Time)),
    {DateParsed, TimeParsed}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
