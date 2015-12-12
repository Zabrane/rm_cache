-module(rm_cache).
-export([new/3, lookup/2]).
-export([info/1, to_list/1]).
-define(META_MFA_KEY, 1).
-define(META_SIZE_KEY, 2).

new(Name, Size, {M,F,A}) ->
    true = new_data_table(Name),
    true = new_meta_table(Name, Size, {M,F,A}),
    ok.

lookup(Name, Key) ->
    case find_in_cache(Name, Key) of
        {ok, V} -> {ok, V};
        {error, not_found_in_cache} -> ask_backend(Name, Key)
    end.

info(Name) ->
    [MFA, Size] = [element(2,I) || I <- lists:sort(ets:tab2list(meta_table(Name)))],
    {ets:info(Name, name), Size, MFA}.

to_list(Name) ->
    ets:tab2list(Name).

%% Internal

ask_backend(Name, Key) ->
    {M,F,A} = meta_mfa(Name),
    case erlang:apply(M,F,A++[Key]) of
        {ok, V} -> insert_new_key(Name, Key,V),
                   {ok, V};
        Error -> Error end.

find_in_cache(Name, Key) ->
    case ets:lookup(Name, Key) of
        [{Key, Val, hot}] -> {ok, Val};
        [{Key, Val, cold}] -> ets:insert(Name, {Key, Val, hot}),
                              {ok, Val};
        _ -> {error, not_found_in_cache} end.

evict_cold(Name, Count) when Count > 0 ->
    case ets:match(Name, {'$1','_',cold}, Count) of
        {[[K]|_], _} -> ets:delete(Name, K);
        '$end_of_table' -> ets:delete_all_objects(Name) end.

insert_new_key(Name, Key, Value) ->
    case ets:info(Name, size) >= meta_size(Name) of
        false -> ets:insert(Name, {Key, Value, cold});
        true -> evict_cold(Name, 1),
                ets:insert(Name, {Key, Value, cold})
    end.

meta_mfa(Name) -> meta(Name, ?META_MFA_KEY).
meta_size(Name) ->meta(Name, ?META_SIZE_KEY).
meta_table(Name) -> list_to_atom(atom_to_list(Name) ++ "_meta").
meta(Name, MetaKey) -> [{MetaKey,V}] = ets:lookup(meta_table(Name),MetaKey), V.

new_data_table(Name) ->
    ets:new(Name, [named_table, public]),
    fixme_persist(Name).

new_meta_table(Name, Size, {_,_,_} = MFA) ->
    MetaName = meta_table(Name),
    MetaName = ets:new(MetaName, [named_table, public]),
    ets:insert(MetaName, {?META_MFA_KEY, MFA}),
    ets:insert(MetaName, {?META_SIZE_KEY, Size}),
    fixme_persist(MetaName).

fixme_persist(Ets) ->
    %% Ensure that ets tables have an heir that keeps them alive
    Owner = spawn(fun() -> timer:sleep(infinity) end),
    true = ets:give_away(Ets, Owner, []).

