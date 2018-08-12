-module(rt_ct_util).

-include("riak_tests.hrl").

-export([start_node/1, setup/0, have_indexes/1]).

-define(RIAK_DEP_PATH, ?DEPS_DIR ++ "/riak").

start_node(Name) ->
    {ok, _} = net_kernel:start([Name]),
    true = erlang:set_cookie(Name, riak).

setup() ->
    ensure_clean_devrel(),
    configure_environment().

ensure_clean_devrel() ->
    lager:info("Running clean devrel on the riak dependency...~n"),
    os:cmd("cd " ++ ?PRIV_DIR ++ " && source ./clean_devrel riak && cd -"),
    ok.

configure_environment() ->
    application:set_env(riak_test, rt_harness, rtdev),
    %% TODO ensure that rt_config is able to read configuration below
    ListParams = [
        {rt_max_wait_time, 600000},
        {rt_retry_delay, 500},
        {rt_harness, rtdev}
    ],
    lists:map(fun({N, V}) ->
            application:set_env(riak_test, N, V)
        end, ListParams),
    io:format("Priv dir = ~p~n", [?PRIV_DIR]),
    io:format("Deps dir = ~p~n", [?DEPS_DIR]),
    io:format("Using ~p as the Riak dependency path~n", [?RIAK_DEP_PATH]),
    application:set_env(riak_test, rtdev_path, [
        {root,     ?RIAK_DEP_PATH},
        {current,  ?RIAK_DEP_PATH},
        {previous, ?RIAK_DEP_PATH},
        {legacy,   ?RIAK_DEP_PATH}
    ]).

have_indexes(Node) ->
    case rpc:call(Node, app_helper, get_env, [riak_kv, storage_backend]) of
        undefined -> false; %% default is da 'cask
        riak_kv_bitcask_backend -> false;
        _ -> true
    end.
