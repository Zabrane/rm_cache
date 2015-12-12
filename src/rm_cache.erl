-module(rm_cache).
-export([new/2, lookup/2, info/1]).

new(Name, {M,F,A}) ->
    ok = new_data_table(Name),
    ok = new_meta_table(Name, {M,F,A}).

lookup(Name, Key) ->
    case ets:lookup(Name, Key) of
        [{Key,V}] -> {ok, V};
        [] -> ask_backend(Name, Key)
    end.

info(Name) ->
    [ets:info(Name, name) | ets:tab2list(meta_table(Name))].

meta_table(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_meta").

new_data_table(Name) ->
    ets:new(Name, [named_table, public]),
    ok.

new_meta_table(Name, {_,_,_} = MFA) ->
    MetaName = meta_table(Name),
    ets:new(MetaName, [named_table, public]),
    ets:insert(MetaName, {1, MFA}),
    ok.

ask_backend(Name, Key) ->
    {M,F,A} = get_mfa(Name),
    case erlang:apply(M,F,A++[Key]) of
        {ok, V} -> ets:insert(Name, {Key, V}),
                   {ok, V};
        Error -> Error
    end.

get_mfa(Name) ->
    [{1,MFA}] = ets:lookup(meta_table(Name),1),
    MFA.
