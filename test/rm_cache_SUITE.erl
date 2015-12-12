-module(rm_cache_SUITE).
-compile(export_all).
-include_lib("bddr/include/bddr.hrl").

all() ->
    [ it_is_created_with_a_name_size_and_backend,
      it_is_created_with_a_backend_fun,
      subsequent_lookup_does_not_hit_backend,
      cold_keys_are_evicted_first,
      all_keys_are_evicted_if_table_fully_hot
    ].


it_is_created_with_a_name_size_and_backend(_) ->
  bddr:test
    (?Given() -> ok end,
     ?When(_) -> rm_cache:new(t0, 10, {m, f, [a]}) end,
     ?Then(ok) -> {t0, 10, {m,f,[a]}} = rm_cache:info(t0) end).

it_is_created_with_a_backend_fun(_) ->
  bddr:test
    (?Given() ->
            {ok, B} = rm_test_backend:start([{"1","a"}]),
            rm_cache:new(t2, 01, {rm_test_backend, retrieve, [B]}) end,
     ?When(_) ->
            rm_cache:lookup(t2, "1") end,
     ?Then(Res) ->
            {ok, "a"} = Res end).

subsequent_lookup_does_not_hit_backend(_) ->
  bddr:test
    (?Given() ->
            rm_test_backend:start(b3, [{"10","aa"}]),
            rm_cache:new(t3, 10, {rm_test_backend, retrieve, [b3]}),
            {ok, "aa"} = rm_cache:lookup(t3, "10") end,
     ?When(_) ->
            rm_cache:lookup(t3, "10") end,
     ?Then(Res) ->
            {ok, "aa"} = Res,
            {_, [_Call1]} = rm_test_backend:dump_state(b3) end).

cold_keys_are_evicted_first(_) ->
  bddr:test
    (?Given() ->
            rm_test_backend:start(rgb, [{"b","blue"}, {"g","green"}, {"r","red"}]),
            rm_cache:new(t4, 2, {rm_test_backend, retrieve, [rgb]}),
            {ok,"blue"} = rm_cache:lookup(t4, "b"),
            hot_key(t4, "r") end,
     ?When(_) ->
            {ok, "green"} = rm_cache:lookup(t4, "g") end,
     ?Then(_) ->
            [{"g", "green", cold}, {"r", "red", hot}] =
                lists:sort(rm_cache:to_list(t4)) end).


all_keys_are_evicted_if_table_fully_hot(_) ->
  bddr:test
    (?Given() ->
            rm_test_backend:start(rgb, [{"b","blue"}, {"g","green"}, {"r","red"}]),
            rm_cache:new(t5, 2, {rm_test_backend, retrieve, [rgb]}),
            hot_key(t5, "r"),
            hot_key(t5, "g") end,
     ?When(_) ->
            {ok, "blue"} = rm_cache:lookup(t5, "b") end,
     ?Then(_) ->
            [{"b", "blue", cold}] = rm_cache:to_list(t5) end).


hot_key(Cache, Key) ->
    {ok, V} = rm_cache:lookup(Cache, Key), % cold after first lookup
    {ok, V} = rm_cache:lookup(Cache, Key). % officially hot
