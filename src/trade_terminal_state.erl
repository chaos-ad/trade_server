-module(trade_terminal_state).
-compile(export_all).

-include("trade_terminal_state.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new() ->
    new([]).

new(Options) ->
    Money = proplists:get_value(money, Options, 0.0),
    State = #terminal_state{securities=make_securities()},
    set_money_position(#money_position{saldo=Money}, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_money_position(Pos=#money_position{}, State=#terminal_state{positions=Positions}) ->
    State#terminal_state{positions=lists:keystore(money_position, 1, Positions, Pos)}.

get_money_position(#terminal_state{positions=Positions}) ->
    case lists:keyfind(money_position, 1, Positions) of
        false    -> exit(no_money_pos);
        Position -> Position
    end.

get_money(#money_position{saldo=Money}) ->
    Money;

get_money(State=#terminal_state{}) ->
    Pos = get_money_position(State),
    Pos#money_position.saldo.

add_money(Delta, State=#terminal_state{}) ->
    Pos = get_money_position(State),
    lager:debug("Adding ~p money to ~p...", [Delta, get_money(Pos)]),
    set_money_position(Pos#money_position{saldo=get_money(Pos) + Delta}, State).

del_money(Delta, State=#terminal_state{}) ->
    Pos = get_money_position(State),
    lager:debug("Deleting ~p money from ~p...", [Delta, get_money(Pos)]),
    case get_money(Pos) of
        Money when Money <  Delta -> error(no_money);
        Money when Money >= Delta ->
            set_money_position(Pos#money_position{saldo=get_money(Pos) - Delta}, State)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_positions(#terminal_state{positions=Positions}) ->
    Positions.

get_position(SecID, #terminal_state{positions=Positions}) ->
    false2undef( lists:keyfind(SecID, 3, Positions) ).

set_position(Pos=#sec_position{secid=SecID}, State=#terminal_state{positions=Positions}) ->
    State#terminal_state{positions=lists:keystore(SecID, 3, Positions, Pos)}.

del_position(#sec_position{secid=SecID}, State=#terminal_state{}) ->
    del_position(SecID, State);

del_position(SecID, State=#terminal_state{positions=Positions}) ->
    State#terminal_state{positions=lists:keydelete(SecID, 3, Positions)}.

get_position_lots(SecID, State=#terminal_state{}) ->
    case get_position(SecID, State) of
        undefined -> 0;
        #sec_position{saldo=Saldo} -> Saldo div get_lotsize(SecID, State)
    end.

add_position_lots(Lots, SecID, State=#terminal_state{}) ->
    Delta = round(Lots * get_lotsize(SecID, State)),
    case get_position(SecID, State) of
        undefined ->
            NewPos = #sec_position{secid=SecID, saldo=Delta},
            set_position(NewPos, State);
        Pos = #sec_position{saldo=Saldo} ->
            NewPos = Pos#sec_position{saldo=Saldo+Delta},
            set_position(NewPos, State)
    end.

del_position_lots(Lots, SecID, State=#terminal_state{}) ->
    Delta = round(Lots * get_lotsize(SecID, State)),
    case get_position(SecID, State) of
        undefined -> State;
        Pos = #sec_position{saldo=Delta} -> del_position(Pos, State);
        Pos = #sec_position{saldo=Saldo} ->
            NewPos = Pos#sec_position{saldo=Saldo-Delta},
            set_position(NewPos, State)
    end.

merge_positions(NewPos=#money_position{}, State=#terminal_state{}) ->
    set_money_position(NewPos, State);

merge_positions(NewPos=#sec_position{secid=SecID}, State=#terminal_state{}) ->
    case get_position(SecID, State) of
        undefined -> set_position(NewPos, State);
        OldPos ->
            List = lists:zip(tl(tuple_to_list(OldPos)), tl(tuple_to_list(NewPos))),
            Merged = lists:map(fun({X, undefined}) -> X; ({_, X}) -> X end, List),
            Result = list_to_tuple([sec_position|Merged]),
            set_position(Result, State)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_order(OrderID, #terminal_state{orders=Orders}) ->
    false2undef( lists:keyfind(OrderID, 2, Orders) ).

set_order(Order=#order{transactionid=OrderID}, State=#terminal_state{orders=Orders}) ->
    State#terminal_state{orders=lists:keystore(OrderID, 2, Orders, Order)}.

del_order(#order{transactionid=OrderID}, State=#terminal_state{}) ->
    del_order(OrderID, State);

del_order(OrderID, State=#terminal_state{orders=Orders}) ->
    State#terminal_state{orders=lists:keydelete(OrderID, 2, Orders)}.

merge_order(NewOrder=#order{transactionid=OrderID}, State=#terminal_state{}) ->
    case get_order(OrderID, State) of
        undefined -> set_order(NewOrder, State);
        OldOrder  ->
            List = lists:zip(tl(tuple_to_list(OldOrder)), tl(tuple_to_list(NewOrder))),
            Merged = lists:map(fun({X, undefined}) -> X; ({_, X}) -> X end, List),
            Result = list_to_tuple([order|Merged]),
            set_order(Result, State)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_trades(#terminal_state{trades=Trades}) ->
    Trades.

get_trade(SecID, #terminal_state{trades=Trades}) ->
    false2undef( lists:keyfind(SecID, 2, Trades) ).

set_trade(Trade=#trade{secid=SecID}, State=#terminal_state{trades=Trades}) ->
    State#terminal_state{trades=lists:keystore(SecID, 2, Trades, Trade)}.

del_trade(#trade{secid=SecID}, State=#terminal_state{}) ->
    del_trade(SecID, State);

del_trade(SecID, State=#terminal_state{trades=Trades}) ->
    State#terminal_state{trades=lists:keydelete(SecID, 2, Trades)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_security_id({Market, Security}, #terminal_state{securities=Securities}) ->
    get_security_id(Market, trade_utils:to_list(Security), Securities);

get_security_id(Security, #terminal_state{securities=Securities}) ->
    get_security_id(1, Security, Securities).

get_security_id(Market, Security, Securities) when is_list(Securities) ->
    case lists:keytake(Security, 5, Securities) of
        {value, #security{secid=ID, market=Market}, _} -> ID;
        {value, #security{}, Rest} -> get_security_id(Market, Security, Rest);
        false -> undefined
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_client_id(#terminal_state{clients=[#client{id=ID}]}) ->
    ID;

get_client_id(#terminal_state{}) ->
    undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_period_id(Period, #terminal_state{candlekinds=Candles}) ->
    case lists:keyfind(Period*60, 3, Candles) of
        false -> undefined;
        #candlekind{id=ID} -> ID
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_securities(#terminal_state{securities=Securities}) ->
    Securities.

get_security(SecID, #terminal_state{securities=Securities}) ->
    false2undef( lists:keyfind(SecID, 2, Securities) ).

set_security(Sec=#security{secid=SecID}, State=#terminal_state{securities=Securities}) ->
    State#terminal_state{securities=lists:keystore(SecID, 2, Securities, Sec)}.

del_security(#security{secid=SecID}, State=#terminal_state{}) ->
    del_security(SecID, State);

del_security(SecID, State=#terminal_state{securities=Securities}) ->
    State#terminal_state{securities=lists:keydelete(SecID, 2, Securities)}.

get_lotsize(SecID, State=#terminal_state{}) ->
    SecInfo = get_security(SecID, State),
    SecInfo#security.lotsize.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_orders(#terminal_state{orders=Orders}) ->
    Orders.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_client(ClientID, #terminal_state{clients=Clients}) ->
    false2undef( lists:keyfind(ClientID, 2, Clients) ).

set_client(Client=#client{id=ClientID}, State=#terminal_state{clients=Clients}) ->
    State#terminal_state{clients=lists:keystore(ClientID, 2, Clients, Client)}.

del_client(#client{id=ClientID}, State=#terminal_state{}) ->
    del_client(ClientID, State);

del_client(ClientID, State=#terminal_state{clients=Clients}) ->
    State#terminal_state{clients=lists:keydelete(ClientID, 2, Clients)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_status(#terminal_state{server_status=Status}) ->
    Status.

set_status(Status=#server_status{}, State=#terminal_state{}) ->
    State#terminal_state{server_status=Status}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_markets(#terminal_state{markets=Markets}) ->
    Markets.

set_markets(Markets, State=#terminal_state{}) ->
    State#terminal_state{markets=Markets}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_candlekinds(#terminal_state{candlekinds=CandleKinds}) ->
    CandleKinds.

set_candlekinds(CandleKinds, State=#terminal_state{}) ->
    State#terminal_state{candlekinds=CandleKinds}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_overnight(#terminal_state{overnight=Overnight}) ->
    Overnight.

set_overnight(Overnight, State=#terminal_state{}) ->
    State#terminal_state{overnight=Overnight}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

false2undef(false) -> undefined;
false2undef(Other) -> Other.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_securities() ->
    lists:map(fun make_security/1, trade_db:get_all_symbols_info()).

make_security({symbol, ID, Code, Name, Market}) ->
    #security{secid=ID, seccode=trade_utils:to_list(Code), market=Market, shortname=Name, lotsize=1}.
