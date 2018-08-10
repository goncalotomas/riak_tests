-module(cuttlefish_configuration_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    verify_cuttlefish_params/1
]).

suite() ->
    [{timetrap, {minutes, 10}}].

init_per_suite(Config) ->
    rt_ct_util:start_node('cuttlefish_config@127.0.0.1'),
    rt_ct_util:setup(),

    CuttlefishConf = [
        {"ring_size", "8"},
        {"leveldb.sync_on_write", "on"}
    ],

    [Node] = rt:deploy_nodes(1, {cuttlefish, CuttlefishConf}),
    [{node, Node} | Config].

end_per_suite(Config) ->
    Node = ?config(node, Config),
    rt:stop(Node),
    ?assertEqual(ok, rt:wait_until_unpingable(Node)),
    net_kernel:stop(),
    ok.

all() ->
    [
        verify_cuttlefish_params
    ].

verify_cuttlefish_params(Config) ->
    Node = ?config(node, Config),
    {ok, RingSize} = rt:rpc_get_env(Node, [{riak_core, ring_creation_size}]),
    ?assertEqual(8, RingSize),

    %% test leveldb sync typo
    {ok, LevelDBSync} = rt:rpc_get_env(Node, [{eleveldb, sync}]),
    ?assertEqual(true, LevelDBSync),
    ok.
