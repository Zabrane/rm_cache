-module(rm_test_backend).
-behaviour(gen_server).

-export([start/1, start/2, stop/1]).
-export([retrieve/2, dump_state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          data :: list(),
          calls :: list()
         }).

start(DataStructures) ->
    gen_server:start(?MODULE, [DataStructures], []).
start(Name, DataStructures) ->
    catch stop(Name),
    gen_server:start({local, Name}, ?MODULE, [DataStructures], []).

stop(Pid) -> gen_server:call(Pid, stop).

retrieve(Pid, Key) -> gen_server:call(Pid, {retrieve, Key}).

dump_state(Pid) -> gen_server:call(Pid, dump_state).

init([DataStructures]) ->
    {ok, #state{ data = DataStructures, calls = [] }}.

handle_call(stop, _, S) -> {stop, normal, S, S};
handle_call(dump_state, _, S) -> {reply, {S#state.data, S#state.calls}, S};
handle_call({retrieve, Key} = Req, From, S) ->
    Result = pfind(Key, S#state.data),
    S2 = update_calls(S, {Req, Result, From}),
    {reply, Result, S2}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% internal

update_calls(S, Call) ->
    S#state{ calls = [Call | S#state.calls ] }.

pfind(Key, Plist) ->
    case lists:keyfind(Key, 1, Plist) of
        {Key, Val} -> {ok, Val};
        false -> {error, {not_found, Key}} end.
