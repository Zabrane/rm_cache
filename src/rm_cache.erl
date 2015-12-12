-module(rm_cache).
-export([new/3, lookup/2, info/1]).
-define(MFA_KEY, 1).
-define(SIZE_KEY, 2).

new(Name, Size, {M,F,A}) ->
    ok = new_data_table(Name),
    ok = new_meta_table(Name, Size, {M,F,A}).

lookup(Name, Key) ->
    case ets:lookup(Name, Key) of
        [{Key,V}] -> {ok, V};
        [] -> ask_backend(Name, Key)
    end.

info(Name) ->
    [MFA, Size] = [element(2,I) || I <- lists:sort(ets:tab2list(meta_table(Name)))],
    {ets:info(Name, name), Size, MFA}.

meta_table(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_meta").

new_data_table(Name) ->
    ets:new(Name, [named_table, public]),
    ok.

new_meta_table(Name, Size, {_,_,_} = MFA) ->
    MetaName = meta_table(Name),
    ets:new(MetaName, [named_table, public]),
    ets:insert(MetaName, {?MFA_KEY, MFA}),
    ets:insert(MetaName, {?SIZE_KEY, Size}),
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
