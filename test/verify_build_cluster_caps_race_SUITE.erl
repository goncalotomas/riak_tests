-module(verify_build_cluster_caps_race_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    set_intercept/1
    ,staged_join_fail/1
]).

suite() ->
    [{timetrap, {minutes, 5}}].

init_per_suite(Config) ->
    rt_ct_util:start_node('verify_build_cluster_caps_race@127.0.0.1'),
    rt_ct_util:setup(),
    Nodes = rt:deploy_nodes(2),
    [{nodes, Nodes} | Config].

end_per_suite(Config) ->
    [Node1, Node2] = ?config(nodes, Config),
    % leave 1, 2, and 3
    lager:info("stopping Node 1"),
    rt:stop(Node1),
    ?assertEqual(ok, rt:wait_until_unpingable(Node1)),
    wait_and_validate([Node2]),

    lager:info("stopping Node 2"),
    rt:stop(Node2),
    ?assertEqual(ok, rt:wait_until_unpingable(Node2)),
    net_kernel:stop(),
    ok.

all() ->
    [
        set_intercept
        ,staged_join_fail
    ].

set_intercept(Config) ->
    [_Node1, Node2] = ?config(nodes, Config),
    configure_intercept(Node2),
    ok.

staged_join_fail(Config) ->
    [Node1, Node2] = ?config(nodes, Config),
    ?assertMatch({error, _}, staged_join(Node2, Node1)),
    ok.

staged_join(InitiatingNode, DestinationNode) ->
    rpc:call(InitiatingNode, riak_core, staged_join, [DestinationNode]).

%% init must return `starting' status for join to fail
configure_intercept(Node) ->
    lager:info("Doing unspeakably evil things to the VM"),
    rt_intercept:add(Node, {init, [{{get_status,0}, get_status}]}).

wait_and_validate(Nodes) -> wait_and_validate(Nodes, Nodes).
wait_and_validate(RingNodes, UpNodes) ->
    lager:info("Wait until all nodes are ready and there are no pending changes"),
    ?assertEqual(ok, rt:wait_until_nodes_ready(UpNodes)),
    ?assertEqual(ok, rt:wait_until_all_members(UpNodes)),
    ?assertEqual(ok, rt:wait_until_no_pending_changes(UpNodes)),
    lager:info("Ensure each node owns a portion of the ring"),
    [rt:wait_until_owners_according_to(Node, RingNodes) || Node <- UpNodes],
    [rt:wait_for_service(Node, riak_kv) || Node <- UpNodes],
    done.
