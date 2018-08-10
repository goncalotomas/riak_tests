-module(bucket_props_validation_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    verify_default_props/1
    ,set_invalid_props/1
    ,check_if_props_have_changed/1
    ,set_valid_props/1
]).

suite() ->
    [{timetrap, {minutes, 10}}].

init_per_suite(Config) ->
    rt_ct_util:start_node('bucket_props_roundtrip@127.0.0.1'),
    rt_ct_util:setup(),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(ibrowse),
    {ok, _} = application:ensure_all_started(druuid),
    {ok, _} = application:ensure_all_started(riak_pb),
    {ok, _} = application:ensure_all_started(riakc),
    {ok, _} = application:ensure_all_started(riakhttpc),
    [Node] = Nodes = rt:build_cluster(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready(Nodes)),
    Connections = get_connections(Node),
    Buckets = {druuid:v4_str(), druuid:v4_str()},
    [{node, Node}, {connections, Connections}, {buckets, Buckets} | Config].

end_per_suite(Config) ->
    Connections = ?config(connections, Config),
    close_connections(Connections),
    Node = ?config(node, Config),
    rt:stop_and_wait(Node),
    ok = application:stop(ibrowse),
    ok = application:stop(crypto),
    net_kernel:stop(),
    ok.

all() ->
    [
        verify_default_props
        ,set_invalid_props
        ,check_if_props_have_changed
        ,set_valid_props
    ].

verify_default_props(Config) ->
    %% Check we are starting in a default state
    Connections = ?config(connections, Config),
    Buckets = ?config(buckets, Config),
    DefaultProps = default_props(),
    verify_props(Connections, Buckets, DefaultProps),
    ok.

set_invalid_props(Config) ->
    %% Verify attempting to set invalid properties results in the expected errors
    Connections = ?config(connections, Config),
    Buckets = ?config(buckets, Config),
    InvalidProps = invalid_props(),
    verify_props_errors(set_props(Connections, Buckets, InvalidProps)),
    ok.

check_if_props_have_changed(Config) ->
    %% Verify no props were harmed in the making of this request
    verify_default_props(Config).

set_valid_props(Config) ->
    Connections = ?config(connections, Config),
    Buckets = ?config(buckets, Config),
    ValidProps = valid_props(),
    %% Set valid properties and verify they are present when
    %% retreiving the bucket properties
    ?assertEqual({ok, ok}, set_props(Connections, Buckets, ValidProps)),
    verify_props(Connections, Buckets, ValidProps),
    ok.

verify_props(Connections, Buckets, Expected) ->
    {HttpProps, PbcProps} = get_props(Connections, Buckets),
    ?assert(sets:is_subset(sets:from_list(Expected), sets:from_list(HttpProps))),
    ?assert(sets:is_subset(sets:from_list(Expected), sets:from_list(PbcProps))).

verify_props_errors({HttpResult, PBCResult}) ->
    verify_errors(http_errors(HttpResult)),
    ?assertEqual({error, function_clause}, PBCResult).

http_errors(Result) ->
    ?assertMatch({error, _}, Result),
    {error, {ok, "400", _H, Errors0}} = Result,
    {struct, Errors} = mochijson2:decode(Errors0),
    Errors.

verify_errors(Errors) ->
    ?assertEqual(13, length(Errors)),
    [?assert(verify_error(binary_to_existing_atom(Prop, latin1),
                          binary_to_atom(Message, latin1))) || {Prop, Message} <- Errors].

verify_error(allow_mult, not_boolean) ->
    true;
verify_error(basic_quorum, not_boolean) ->
    true;
verify_error(last_write_wins, not_boolean) ->
    true;
verify_error(notfound_ok, not_boolean) ->
    true;
verify_error(big_vclock, not_integer) ->
    true;
verify_error(n_val, not_integer) ->
    true;
verify_error(old_vclock, not_integer) ->
    true;
verify_error(small_vclock, not_integer) ->
    true;
verify_error(young_vclock, not_integer) ->
    true;
verify_error(Quorum, not_valid_quorum) when Quorum =:= dw;
                                            Quorum =:= pw;
                                            Quorum =:= pr;
                                            Quorum =:= r;
                                            Quorum =:= rw;
                                            Quorum =:= w ->
    true;
verify_error(_, _) ->
    false.

default_props() ->
    [{allow_mult,false},
     {basic_quorum,false},
     {big_vclock,50},
     {chash_keyfun,{riak_core_util,chash_std_keyfun}},
     {dw,quorum},
     {last_write_wins,false},
     {linkfun,{modfun,riak_kv_wm_link_walker,mapreduce_linkfun}},
     {n_val,3},
     {notfound_ok,true},
     {old_vclock,86400},
     {postcommit,[]},
     {pr,0},
     {precommit,[]},
     {pw,0},
     {r,quorum},
     {rw,quorum},
     {small_vclock,50},
     {w,quorum},
     {young_vclock,20}].


 invalid_props() ->
     [{allow_mult, elephant},
      {basic_quorum,fasle},
      {big_vclock, 89.90},
      {dw, qurum},
      {last_write_wins, 90},
      {n_val,<<"3">>},
      {notfound_ok,<<"truish">>},
      {old_vclock, -9890},
      {pr, -90},
      {pw, "seventeen"},
      {r, <<"florum">>},
      {rw, -9090929288989898398.9090093923232},
      {small_vclock, wibble},
      {w, "a horse a horse my kingdom"},
      {young_vclock, "12"}].

 valid_props() ->
     [{allow_mult, true},
      {basic_quorum, true},
      {big_vclock, 90},
      {dw, one},
      {last_write_wins, false},
      {n_val, 4},
      {notfound_ok, false},
      {old_vclock, 9090909},
      {pr, all},
      {pw, quorum},
      {r, 4},
      {rw, 1},
      {small_vclock, 22},
      {w, all},
      {young_vclock, 18}].

get_connections(Node) ->
    {rt:httpc(Node), rt:pbc(Node)}.

close_connections({_Http, PBC}) ->
    riakc_pb_socket:stop(PBC).

get_props({Http, PBC}, {HttpBucket, PbcBucket}) ->
    io:format("socket is_pid (get) = ~p~n", [is_pid(PBC)]),
    {ok, PbcProps} = riakc_pb_socket:get_bucket(PBC, PbcBucket),
    {ok, HttpProps} = rhc:get_bucket(Http, HttpBucket),
    {HttpProps, PbcProps}.

set_props({Http, PBC}, {HttpBucket, PbcBucket}, Props) ->
    io:format("socket is_pid (set) = ~p~n", [is_pid(PBC)]),
    HttpRes = rhc:set_bucket(Http, HttpBucket, Props),
    PbcRes = try riakc_pb_socket:set_bucket(PBC, PbcBucket, Props) of
                 NormalRes ->
                     NormalRes
             catch
                 Error:Reason ->
                     {Error, Reason}
             end,
     {HttpRes, PbcRes}.
