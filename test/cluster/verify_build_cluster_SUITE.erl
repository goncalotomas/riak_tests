-module(verify_build_cluster_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Common Test exports
%%%-------------------------------------------------------------------
-export([
    suite/0,
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export ([setup_cluster/2]).

suite() ->
    [{timetrap, {minutes, 1}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
        start_nodes
        ,load_data_onto_node
        ,join_cluster
        ,takedown_nodes
        ,teardown_cluster
    ].

start_nodes(_Config) ->
    %% test requires allow_mult=false b/c of rt:systest_read
    rt:set_conf(all, [{"buckets.default.allow_mult", "false"}]),
    %% Deploy a set of new nodes
    lager:info("Deploying 4 nodes"),
    %% handoff_concurrency needs to be raised to make the leave operation faster.
    %% most clusters go up to 10, but this one is one louder, isn't it?
    [Node1, Node2, Node3, Node4] = Nodes = rt:deploy_nodes(4, [{riak_core, [{handoff_concurrency, 11}]}]),

    %% Ensure each node owns 100% of it's own ring
    lager:info("Ensure each nodes 100% of it's own ring"),

    [rt:wait_until_owners_according_to(Node, [Node]) || Node <- Nodes],
    ok.

load_data_onto_node(_Config) ->
    lager:info("Loading some data up in this cluster."),
    ?assertEqual([], rt:systest_write(Node1, 0, 1000, <<"verify_build_cluster">>, 2)),
    ok.

join_cluster(_Config) ->
    lager:info("joining Node 2 to the cluster... It takes two to make a thing go right"),
    rt:join(Node2, Node1),
    wait_and_validate([Node1, Node2]),

    lager:info("joining Node 3 to the cluster"),
    rt:join(Node3, Node1),
    wait_and_validate([Node1, Node2, Node3]),

    lager:info("joining Node 4 to the cluster"),
    rt:join(Node4, Node1),
    wait_and_validate(Nodes),
    ok.

takedown_nodes(_Config) ->
    lager:info("taking Node 1 down"),
    rt:stop(Node1),
    ?assertEqual(ok, rt:wait_until_unpingable(Node1)),
    wait_and_validate(Nodes, [Node2, Node3, Node4]),

    lager:info("taking Node 2 down"),
    rt:stop(Node2),
    ?assertEqual(ok, rt:wait_until_unpingable(Node2)),
    wait_and_validate(Nodes, [Node3, Node4]),

    lager:info("bringing Node 1 up"),
    rt:start(Node1),
    ok = rt:wait_until_pingable(Node1),
    wait_and_validate(Nodes, [Node1, Node3, Node4]),
    lager:info("bringing Node 2 up"),
    rt:start(Node2),
    ok = rt:wait_until_pingable(Node2),
    wait_and_validate(Nodes),
    ok.

teardown_cluster(_Config) ->
    % leave 1, 2, and 3
    lager:info("leaving Node 1"),
    rt:leave(Node1),
    ?assertEqual(ok, rt:wait_until_unpingable(Node1)),
    wait_and_validate([Node2, Node3, Node4]),

    lager:info("leaving Node 2"),
    rt:leave(Node2),
    ?assertEqual(ok, rt:wait_until_unpingable(Node2)),
    wait_and_validate([Node3, Node4]),

    lager:info("leaving Node 3"),
    rt:leave(Node3),
    ?assertEqual(ok, rt:wait_until_unpingable(Node3)),
    ok.

wait_and_validate(Nodes) -> wait_and_validate(Nodes, Nodes).
wait_and_validate(RingNodes, UpNodes) ->
    lager:info("Wait until all nodes are ready and there are no pending changes"),
    ?assertEqual(ok, rt:wait_until_nodes_ready(UpNodes)),
    ?assertEqual(ok, rt:wait_until_all_members(UpNodes)),
    ?assertEqual(ok, rt:wait_until_no_pending_changes(UpNodes)),
    lager:info("Ensure each node owns a portion of the ring"),
    [rt:wait_until_owners_according_to(Node, RingNodes) || Node <- UpNodes],
    [rt:wait_for_service(Node, riak_kv) || Node <- UpNodes],
    lager:info("Verify that you got much data... (this is how we do it)"),
    ?assertEqual([], rt:systest_read(hd(UpNodes), 0, 1000, <<"verify_build_cluster">>, 3)),
    done.
