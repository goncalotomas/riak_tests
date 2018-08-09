-module(basic_command_line_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    verify_node_up_behaviour/1,
    stop_node/1
    ,verify_node_down_behaviour/1
]).

-define(PING_FAILURE_OUTPUT, "Node did not respond to ping!").

suite() ->
    [{timetrap, {minutes, 5}}].

init_per_suite(Config) ->
    rt_ct_util:start_node('basic_command_line@127.0.0.1'),
    rt_ct_util:setup(),
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    [{node, Node} | Config].

end_per_suite(_) ->
    net_kernel:stop(),
    ok.

all() ->
    [
        verify_node_up_behaviour
        ,stop_node
        ,verify_node_down_behaviour
    ].


verify_node_up_behaviour(Config) ->
    Node = ?config(node, Config),
    %% Verify node-up behavior
    ping_up_test(Node),
    attach_direct_up_test(Node),
    status_up_test(Node),
    console_up_test(Node),
    start_up_test(Node),
    getpid_up_test(Node),
    ok.

stop_node(Config) ->
    _Node = ?config(node, Config),
    ok.

verify_node_down_behaviour(Config) ->
    Node = ?config(node, Config),
    %% Stop the node, Verify node-down behavior
    stop_test(Node),
    ping_down_test(Node),
    attach_down_test(Node),
    attach_direct_down_test(Node),
    status_down_test(Node),
    console_test(Node),
    start_test(Node),
    getpid_down_test(Node),
    ok.

%% Auxiliary functions to test for node up behaviour

ping_up_test(Node) ->
    %% check /usr/sbin/riak ping
    lager:info("Testing riak ping on ~s", [Node]),
    %% ping / pong
    %% rt:start_and_wait(Node),
    lager:info("Node up, should ping"),
    {ok, PongOut} = rt:riak(Node, ["ping"]),
    ?assert(rt:str(PongOut, "pong")),
    ok.

attach_direct_up_test(Node) ->
    lager:info("Testing riak attach-direct"),
    rt:attach_direct(Node, [
        {expect, "\(^D to exit\)"},
        {send, "riak_core_ring_manager:get_my_ring()."},
        {expect, "dict,"},
        {send, [4]} %% 4 = Ctrl + D
    ]),
    ok.

status_up_test(Node) ->
    lager:info("Test riak-admin status on ~s", [Node]),
    {ok, {ExitCode, StatusOut}} = rt:admin(Node, ["status"], [return_exit_code]),
    io:format("Result of status: ~s", [StatusOut]),
    ?assertEqual(0, ExitCode),
    ?assert(rt:str(StatusOut, "1-minute stats")),
    ?assert(rt:str(StatusOut, "kernel_version")),
    ok.

console_up_test(Node) ->
    lager:info("Node is already up, `riak console` should fail"),
    {ok, ConsoleFail} = rt:riak(Node, ["console"]),
    ?assert(rt:str(ConsoleFail, "Node is already running")),
    ok.

start_up_test(Node) ->
    %% Try starting again and check you get the node is already running message
    lager:info("Testing riak start now will return 'already running'"),
    {ok, StartOut} = rt:riak(Node, ["start"]),
    ?assert(rt:str(StartOut, "Node is already running!")),
    ok.

getpid_up_test(Node) ->
    lager:info("Test riak getpid on ~s", [Node]),
    {ok, PidOut} = rt:riak(Node, ["getpid"]),
    ?assertNot(rt:str(PidOut, "")),
    ?assert(rt:str(PidOut, rpc:call(Node, os, getpid, []))),
    ok.

%% Auxiliary functions to test for node down behaviour

stop_test(Node) ->
    ?assert(rt:is_pingable(Node)),
    {ok, "ok\n"} = rt:riak(Node, "stop"),
    ?assertNot(rt:is_pingable(Node)),
    ok.

ping_down_test(Node) ->
    %% ping / pang
    lager:info("Node down, should pang"),
    {ok, PangOut} = rt:riak(Node, ["ping"]),
    ?assert(rt:str(PangOut, "not responding to pings")),
    ok.

attach_down_test(Node) ->
    lager:info("Testing riak attach while down"),
    {ok, AttachOut} = rt:riak(Node, ["attach"]),
    ?assert(rt:str(AttachOut, ?PING_FAILURE_OUTPUT)),
    ok.

attach_direct_down_test(Node) ->
    lager:info("Testing riak attach-direct while down"),
    {ok, AttachOut} = rt:riak(Node, ["attach-direct"]),
    ?assert(rt:str(AttachOut, ?PING_FAILURE_OUTPUT)),
    ok.

status_down_test(Node) ->
    lager:info("Test riak-admin status while down"),
    {ok, {ExitCode, StatusOut}} = rt:admin(Node, ["status"], [return_exit_code]),
    ?assertEqual(1, ExitCode),
    ?assert(rt:str(StatusOut, ?PING_FAILURE_OUTPUT)),
    ok.

console_test(Node) ->
    %% Make sure the cluster will start up with /usr/sbin/riak console, then quit
    lager:info("Testing riak console on ~s", [Node]),
    %% Stop node, to test console working
    rt:console(Node, [
        {expect, "\(abort with ^G\)"},
        {send, "riak_core_ring_manager:get_my_ring()."},
        {expect, "dict,"},
        {send, "q()."},
        {expect, "ok"}
    ]),
    rt:wait_until_unpingable(Node),
    ok.

start_test(Node) ->
    %% Test starting with /bin/riak start
    lager:info("Testing riak start works on ~s", [Node]),
    {ok, StartPass} = rt:riak(Node, ["start"]),
    lager:info("StartPass: ~p", [StartPass]),
    ?assert(StartPass =:= "" orelse string:str(StartPass, "WARNING") =/= 0),
    rt:stop_and_wait(Node),
    ok.

getpid_down_test(Node) ->
    lager:info("Test riak getpid fails on ~s", [Node]),
    {ok, PidOut} = rt:riak(Node, ["getpid"]),
    ?assert(rt:str_mult(PidOut, [?PING_FAILURE_OUTPUT, "not responding to pings"])),
    ok.
