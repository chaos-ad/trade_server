-module(trade_terminal_mgr).
-behaviour(supervisor).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/0, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SERVER, {global, ?MODULE}).
-define(CHILD(I), {I, {I, start_link, []}, permanent, 60000, worker, [I]}).
-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 60000, worker, [I]}).
-define(CHILD(I, Args, Role), {I, {I, start_link, Options}, permanent, 60000, Role, [I]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    supervisor:start_link(?SERVER, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accounts() ->
    [ {Name, Pid} || {Name, Pid, _, _} <- supervisor:which_children(?SERVER) ].

account(Name) ->
    case lists:keyfind(Name, 1, accounts()) of
        {Name, Pid} -> Pid;
        undefined   -> undefined
    end.

stop_account(Account) ->
    supervisor:terminate_child(?SERVER, Account).

start_account(Account) ->
    supervisor:restart_child(?SERVER, Account).

add_account(Account) ->
    supervisor:start_child(?SERVER, child_spec(Account)).

del_account(Account) ->
    supervisor:delete_child(?SERVER, Account).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, Accounts} = application:get_env(accounts),
    RestartStrategy = {one_for_one, 2, 600},
    Children = lists:map(fun child_spec/1, Accounts),
    {ok, {RestartStrategy, Children}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

child_spec({Name, Options}) ->
    StartFunc = {trade_terminal, start_link, [Name, Options]},
    {Name, StartFunc, permanent, 60000, worker, [trade_terminal]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%