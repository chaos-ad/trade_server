-module(trade_arg_utils).
-compile(export_all).

get_required(Args, Name) ->
    get_required(Args, Name, undefined).

get_required(Args, Name, Type) ->
    case get_default(Args, Name, Type, undefined) of
        undefined -> exit({absent_arg, Name});
        Value     -> Value
    end.

get_optional(Args, Name) ->
    get_optional(Args, Name, undefined).

get_optional(Args, Name, Type) ->
    get_default(Args, Name, Type, undefined).


get_default(Args, Name, Type, Default) ->
    case proplists:get_value(Name, Args) of
        undefined -> convert(Type, Default);
        Value     -> convert(Type, Value)
    end.

convert(_, undefined) -> undefined;
convert(undefined, Val) -> Val;
convert(atom, Val) when is_atom(Val) -> Val;
convert(atom, Val) when is_list(Val) -> list_to_atom(Val);
convert(integer, Val) when is_integer(Val) -> Val;
convert(integer, Val) when is_list(Val) -> list_to_integer(Val);
convert(date, Val) when is_list(Val) -> edate:string_to_date(Val).
