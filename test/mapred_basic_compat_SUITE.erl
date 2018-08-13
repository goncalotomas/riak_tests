-module(mapred_basic_compat_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    load_test_data/1
    ,empty_query/1
    ,reduce_zero_inputs/1
    ,keep_both/1
    ,keep_neither/1
    ,keep_first_only/1
    ,keep_second_only/1
    ,explicity_rereduce/1
    ,error_not_found_propagation/1
    ,basic_link/1
    ,link_not_found/1
    ,keydata/1
    ,key_filters/1
    ,map_output_with_btype/1
    ,modfun_generator1/1
    ,modfun_generator2/1
]).

%% exports required for this particular test that are NOT test cases
-export([inputs_gen_seq/3, inputs_gen_bkeys_1/3]).

-define(INTS_BUCKET, <<"foonum">>).
-define(LINK_BUCKET, <<"link bucket">>).
-define(BUCKET_TYPE, <<"mytype">>).

suite() ->
    [{timetrap, {minutes, 10}}].

init_per_suite(Config) ->
    rt_ct_util:start_node('mapred_basic_compat@127.0.0.1'),
    rt_ct_util:setup(),
    Nodes = rt:build_cluster(3),

    Node = hd(Nodes),
    %% create a new type
    ok = rt:create_activate_and_wait_for_bucket_type(Nodes, ?BUCKET_TYPE, [{n_val,3}]),
    % rt:create_and_activate_bucket_type(Nodes, ?BUCKET_TYPE, [{n_val, 3}]),
    rt:wait_until_bucket_type_status(?BUCKET_TYPE, active, Nodes),
    rt:wait_until_bucket_type_visible(Nodes, ?BUCKET_TYPE),

    rt:load_modules_on_nodes([?MODULE], Nodes),
    rt:wait_for_service(Node, riak_kv),

    [{node, Node}, {nodes, Nodes} | Config].

end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    [rt:stop_and_wait(Node) || Node <- Nodes],
    net_kernel:stop(),
    ok.

all() ->
    [
        load_test_data
        ,empty_query
        ,reduce_zero_inputs
        ,keep_both
        ,keep_neither
        ,keep_first_only
        ,explicity_rereduce
        ,error_not_found_propagation
        ,basic_link
        ,link_not_found
        ,keydata
        ,key_filters
        ,map_output_with_btype
        ,modfun_generator1
        ,modfun_generator2
    ].

%% @doc This will trigger a traversal of IntsBucket, but because the
%% query is empty, the MapReduce will traverse the bucket and send
%% BKeys down the pipe.
empty_query(Config) ->
    Node = ?config(node, Config),
    {ok, BKeys} = rpcmr(Node, ?INTS_BUCKET, []),
    ?assertEqual(5, length(BKeys)),
    ?assertEqual({?INTS_BUCKET, <<"bar1">>}, hd(lists:sort(BKeys))).

load_test_data(Config) ->
    Node = ?config(node, Config),
    %% creates foonum/1..5 - this is what populates ?INTS_BUCKET
    lager:info("Filling INTS_BUCKET (~s)", [?INTS_BUCKET]),
    ok = rpc:call(Node, riak_kv_mrc_pipe, example_setup, []),

    lager:info("Adding Link object"),
    Obj = riakc_obj:new(?LINK_BUCKET,
                        <<"yo">>,
                        <<"link val">>,
                        "text/plain"),
    MD = riakc_obj:add_link(
           riakc_obj:get_update_metadata(Obj),
           [{<<"link 1">>, [{?LINK_BUCKET, <<"nokey-1">>}]},
            {<<"link 2">>, [{?LINK_BUCKET, <<"nokey-2">>}]}]),

    C = rt:pbc(Node),
    ok = riakc_pb_socket:put(C, riakc_obj:update_metadata(Obj, MD)),

    %% Some bucket type entries {mytype,foonum}/bar{1..10}
    [begin
            K = list_to_binary("bar"++integer_to_list(N)),
            V = list_to_binary(integer_to_list(N)),
            O = riakc_obj:new({?BUCKET_TYPE, ?INTS_BUCKET}, K, V),
            riakc_pb_socket:put(C, O)
        end || N <- lists:seq(1,10)],
    riakc_pb_socket:stop(C).

%% @doc AZ 479: Reduce with zero inputs -> call reduce once w/empty list
reduce_zero_inputs(Config) ->
    Node = ?config(node, Config),
    Spec = [{reduce, {modfun, riak_kv_mapreduce, reduce_sum}, none, true}],
    ?assertEqual({ok, [0]}, rpcmr(Node, [], Spec)).

%% @doc Basic compatibility: keep both stages
keep_both(Config) ->
    Node = ?config(node, Config),
    Spec =  [{map, {modfun, riak_kv_mapreduce, map_object_value}, none, true},
             {reduce, {modfun, riak_kv_mapreduce, reduce_sum}, none, true}],
    {ok, [MapRs, ReduceRs]} = rpcmr(Node, ?INTS_BUCKET, Spec),
    ?assertEqual(5, length(MapRs)),
    ?assertEqual([15], ReduceRs).

%% @doc Basic compat: keep neither stages -> no output
keep_neither(Config) ->
    Node = ?config(node, Config),
    Spec = [{map, {modfun, riak_kv_mapreduce, map_object_value}, none, false},
            {reduce, {modfun, riak_kv_mapreduce, reduce_sum}, none, false}],
    %% "Crazy" semantics: if only 1 keeper stage, then
    %% return List instead of [List].
    ?assertEqual({ok, []}, rpcmr(Node, ?INTS_BUCKET, Spec)).

%% @doc Basic compat: keep first stage only, want 'crazy' result",
keep_first_only(Config) ->
    Node = ?config(node, Config),
    Spec = [{map, {modfun, riak_kv_mapreduce, map_object_value}, none, true},
            {reduce, {modfun, riak_kv_mapreduce, reduce_sum}, none, false}],
    %% "Crazy" semantics: if only 1 keeper stage, then
    %% return List instead of [List].
    {ok, MapRs} = rpcmr(Node, ?INTS_BUCKET, Spec),
    ?assertEqual(5, length(MapRs)).

%% @doc Basic compat: keep second stage only, want 'crazy' result
keep_second_only(Config) ->
    Node = ?config(node, Config),
    Spec = [{map, {modfun, riak_kv_mapreduce, map_object_value}, none, false},
            {reduce, {modfun, riak_kv_mapreduce, reduce_sum}, none, true}],
    %% "Crazy" semantics: if only 1 keeper stage, then
    %% return List instead of [List].
    ?assertEqual({ok, [15]}, rpcmr(Node, ?INTS_BUCKET, Spec)).

%% @doc Explicit rereduce
explicity_rereduce(Config) ->
    Node = ?config(node, Config),
    RedSpec = {reduce, {modfun, riak_kv_mapreduce, reduce_sum}, none, true},
    Spec = [{map, {modfun, riak_kv_mapreduce, map_object_value}, none, true}]
        ++ lists:duplicate(5, RedSpec),
    ?assertMatch({ok, [_, [15],[15],[15],[15],[15]]},
                 rpcmr(Node, ?INTS_BUCKET, Spec)).

%% @doc Make certain that {error, not_found} goes down the pipe from a
%% map phase.
error_not_found_propagation(Config) ->
    Node = ?config(node, Config),
    Inputs = [{<<"no-such-bucket">>, <<"no-such-key!">>}],
    Spec =  [{map, {modfun, riak_kv_mapreduce, map_object_value},
              {struct,[{<<"sub">>,[<<"0">>]}]}, false},
             {reduce, {modfun, riak_kv_mapreduce, reduce_string_to_integer},
              none,true}],
    ?assertEqual({ok, [0]}, rpcmr(Node, Inputs, Spec)),
    B = {?BUCKET_TYPE, ?INTS_BUCKET},
    Inputs2 = [{{B, <<"nokey">>}, undefined}],
    Spec2 =  [{map, {modfun, riak_kv_mapreduce, map_object_value},
              {struct,[{<<"sub">>,[<<"0">>]}]}, false},
             {reduce, {modfun, riak_kv_mapreduce, reduce_string_to_integer},
              none,true}],
    ?assertEqual({ok, [0]}, rpcmr(Node, Inputs2, Spec2)).

%% @doc A map phase outputting a 4 tuple can feed objects to another map phase
map_output_with_btype(Config) ->
    Node = ?config(node, Config),
    %% Translates from regular bucket to bucket type one
    Inputs = ?INTS_BUCKET,
    Spec = [{map, {jsanon, <<"function(o){return[[o.bucket,o.key,null,\"mytype\"]];}">>}, undefined, false},
            {map, {modfun, riak_kv_mapreduce, map_object_value}, undefined, false},
            {reduce, {modfun, riak_kv_mapreduce, reduce_string_to_integer}, undefined, false},
            {reduce, {modfun, riak_kv_mapreduce, reduce_sort}, undefined, true}
           ],
    ?assertEqual({{ok, lists:seq(1,5)}, {Inputs, Spec}},
                 {rpcmr(Node, Inputs, Spec), {Inputs, Spec}}).

%% @doc Basic link phase
basic_link(Config) ->
    Node = ?config(node, Config),
    Spec = [{link, '_', <<"link 1">>, true}],
    ?assertEqual({ok, [ [?LINK_BUCKET, <<"nokey-1">>, <<"link 1">>] ]},
                 rpcmr(Node, ?LINK_BUCKET, Spec)).

%% @doc Link phase + notfound
link_not_found(Config) ->
    Node = ?config(node, Config),
    Inputs = [{<<"no">>, K} || K <- [<<"no1">>, <<"no2">>]],
    Spec = [{link, '_', '_', true}],
    ?assertEqual({ok, []}, rpcmr(Node, Inputs, Spec)).

%% @doc KeyData
keydata(Config) ->
    Node = ?config(node, Config),
    UnMap = fun(O, undefined, _) ->
                    [{riak_object:bucket(O),
                      riak_object:key(O)}];
               (O, KeyData, _) ->
                    [{{riak_object:bucket(O),
                       riak_object:key(O)},
                      KeyData}]
            end,
    Normalize = fun({{B,K},D}) -> {{B,K},D};
                   ({B,K})     -> {B,K};
                   ([B,K])     -> {B,K};
                   ([B,K,D])   -> {{B,K},D}
                end,
    Spec = [{map, {qfun, UnMap}, none, true}],
    Inputs = [{?INTS_BUCKET, <<"bar1">>},
              {{?INTS_BUCKET, <<"bar2">>}, <<"keydata works">>},
              [?INTS_BUCKET, <<"bar3">>],
              [?INTS_BUCKET, <<"bar4">>, <<"keydata still works">>]],
    {ok, Results} = rpcmr(Node, Inputs, Spec),
    SortedNormal = lists:sort([ Normalize(I) || I <- Inputs ]),
    ?assertEqual(SortedNormal, lists:sort(Results)).

%% @doc Key Filters
key_filters(Config) ->
    Node = ?config(node, Config),
    %% filter sould match only "bar4" key
    Inputs = {?INTS_BUCKET, [[<<"ends_with">>, <<"r4">>]]},
    Spec = [{map, {modfun, riak_kv_mapreduce, map_object_value}, none, true}],
    ?assertEqual({ok, [4]}, rpcmr(Node, Inputs, Spec)).

%% @doc modfun for inputs generator
modfun_generator1(Config) ->
    Node = ?config(node, Config),
    Inputs = {modfun, ?MODULE, inputs_gen_seq, 6},
    Spec = [{reduce, {modfun, riak_kv_mapreduce, reduce_sum},none,true}],
    ?assertEqual({ok, [21]}, rpcmr(Node, Inputs, Spec)).

%% @doc modfun for inputs generator: make BKeys for conventional phases
modfun_generator2(Config) ->
    Node = ?config(node, Config),
    Inputs = {modfun, ?MODULE, inputs_gen_bkeys_1, {?INTS_BUCKET, 1, 5}},
    Spec = [{map, {modfun, riak_kv_mapreduce, map_object_value},
             none, false},
            {reduce, {modfun, riak_kv_mapreduce, reduce_string_to_integer},
             none,false},
            {reduce, {modfun, riak_kv_mapreduce, reduce_sum},
             none,true}],
    ?assertEqual({ok, [15]}, rpcmr(Node, Inputs, Spec)).

%% Auxiliary functions


rpcmr(Node, Inputs, Query) ->
    rpc:call(Node, riak_kv_mrc_pipe, mapred, [Inputs, Query]).

%% @doc runs on riak node
inputs_gen_seq(Pipe, Max, _Timeout) ->
    [riak_pipe:queue_work(Pipe, X) || X <- lists:seq(1, Max)],
    riak_pipe:eoi(Pipe),
    ok.

%% @doc runs on riak node
inputs_gen_bkeys_1(Pipe, {Bucket, Start, End}, _Timeout) ->
    BKeys = [{Bucket, list_to_binary("bar"++integer_to_list(X))} ||
                 X <- lists:seq(Start, End)],
    [riak_pipe:queue_work(Pipe, BK) || BK <- BKeys],
    riak_pipe:eoi(Pipe),
    ok.
