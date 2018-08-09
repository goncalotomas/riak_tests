-module(bucket_props_roundtrip_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    set_get_and_check_props/1
]).

-define(BUCKET, <<"pbc_props_verify">>).
-define(COMMIT_HOOK, {struct, [{<<"mod">>, <<"foo">>}, {<<"fun">>, <<"bar">>}]}).
-define(CHASHFUN, {riak_core_util, chash_bucketonly_keyfun}).
-define(LINKFUN, {modfun, raw_link_walker, mapreduce_linkfun}).

suite() ->
    [{timetrap, {minutes, 10}}].

init_per_suite(Config) ->
    rt_ct_util:start_node('bucket_props_roundtrip@127.0.0.1'),
    rt_ct_util:setup(),
    [Node] = Nodes = rt:build_cluster(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready(Nodes)),
    Props = [
        {allow_mult, true, false},
        {backend, <<"custom">>, <<"other">>},
        {basic_quorum, true, false},
        {big_vclock, 100, 50},
        {chash_keyfun, ?CHASHFUN, {riak_core_util, chash_std_keyfun}},
        {dw, 0, quorum},
        {last_write_wins, true, false},
        {linkfun, ?LINKFUN, {modfun, riak_kv_wm_link_walker, mapreduce_linkfun}},
        {n_val, 2, 3},
        {notfound_ok, false, true},
        {old_vclock, 10000, 86400},
        {postcommit, [?COMMIT_HOOK], []},
        {pr, 2, 0},
        {precommit, [?COMMIT_HOOK], []},
        {pw, all, 0},
        {r, all, quorum},
        {repl, realtime, false},
        {rw, 1, quorum},
        {search, true, false},
        {small_vclock, 10, 50},
        {w, one, quorum},
        {young_vclock, 0, 20}
    ],
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(ibrowse),
    [{node, Node}, {props, Props} | Config].

end_per_suite(Config) ->
    Node = ?config(node, Config),
    rt:stop_and_wait(Node),
    ok = application:stop(ibrowse),
    ok = application:stop(crypto),
    net_kernel:stop(),
    ok.

all() ->
    [
        set_get_and_check_props
    ].

set_get_and_check_props(Config) ->
    Node = ?config(node, Config),
    Props = ?config(props, Config),
    lists:map(fun({Prop, FirstVal, SecondVal}) ->
        io:format("calling check_prop_set_and_get(~p, ~p, ~p, ~p)", [Node, Prop, FirstVal, SecondVal]),
        check_prop_set_and_get(Node, Prop, FirstVal, SecondVal)
    end, Props),
    ok.

check_prop_set_and_get(Node, Prop, One, Two) ->
    lager:info("-------- Testing roundtrip for property '~p' ---------", [Prop]),
    HTTP = rt:httpc(Node),
    PBC = rt:pbc(Node),
    lager:info("HTTP set = ~p", [One]),
    http_set_property(HTTP, Prop, One),
    lager:info("PBC get should == ~p", [One]),
    ?assertEqual(One, pbc_get_property(PBC, Prop)),

    lager:info("PBC set = ~p", [Two]),
    pbc_set_property(PBC, Prop, Two),
    lager:info("HTTP get should = ~p", [Two]),
    ?assertEqual(Two, http_get_property(HTTP, Prop)),
    ok.


http_set_property(Client, Prop, Value) ->
    rhc:set_bucket(Client, ?BUCKET, [{Prop, Value}]).

http_get_property(Client, Prop) ->
    {ok, Props} = rhc:get_bucket(Client, ?BUCKET),
    case lists:keyfind(Prop, 1, Props) of
        {Prop, Value} ->
            Value;
        _ -> undefined
    end.

pbc_set_property(Client, Prop, Value) ->
    riakc_pb_socket:set_bucket(Client, ?BUCKET, [{Prop, Value}]).

pbc_get_property(Client, Prop) ->
    {ok, Props} = riakc_pb_socket:get_bucket(Client, ?BUCKET),
    case lists:keyfind(Prop, 1, Props) of
        {Prop, Value} ->
            Value;
        _ -> undefined
    end.
