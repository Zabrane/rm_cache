-module(rm_cache_SUITE).
-compile(export_all).
-include_lib("bddr/include/bddr.hrl").

all() ->
    [ it_is_created_with_a_name_and_backend,
      it_is_created_with_a_backend_fun,
      subsequent_lookup_does_not_hit_backend
    ].


it_is_created_with_a_name_and_backend(_) ->
  bddr:test
    (?Given() -> ok end,
     ?When(_) -> rm_cache:new(t0, {m, f, [a]}) end,
     ?Then(ok) -> [t0|_] = rm_cache:info(t0) end).

it_is_created_with_a_backend_fun(_) ->
  bddr:test
    (?Given() ->
            {ok, B} = rm_test_backend:start([{<<"1">>,<<"a">>}]),
            rm_cache:new(t2, {rm_test_backend, retrieve, [B]}) end,
     ?When(_) -> rm_cache:lookup(t2, <<"1">>) end,
     ?Then(Res) -> {ok, <<"a">>} = Res end).


subsequent_lookup_does_not_hit_backend(_) ->
  bddr:test
    (?Given() -> {ok, B} = rm_test_backend:start([{<<"10">>,<<"aa">>}]),
                 rm_cache:new(t3, {rm_test_backend, retrieve, [B]}),
                 {ok, <<"aa">>} = rm_cache:lookup(t3, <<"10">>),
                 B end,
     ?When(Backend) ->
            {ok, <<"aa">>} = rm_cache:lookup(t3, <<"10">>),
            Backend end,
     ?Then(Backend) ->
            {_Store, [_Call1]} = rm_test_backend:dump_state(Backend) end).

