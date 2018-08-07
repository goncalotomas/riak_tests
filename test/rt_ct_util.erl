-module(rt_ct_util).

-export([start_node/1, setup/0]).

start_node(Name) ->
    {ok, _} = net_kernel:start([Name]),
    true = erlang:set_cookie(Name, riak).

setup() ->
    application:set_env(riak_test, rt_harness, rtdev),
    %% TODO ensure that rt_config is able to read configuration below
    ListParams = [
        {rt_max_wait_time, 600000},
        {rt_retry_delay, 1000},
        {rt_harness, rtdev}
    ],
    lists:map(fun({N, V}) ->
            application:set_env(riak_test, N, V)
        end, ListParams),
    %% TODO remove hardcoded paths
    application:set_env(riak_test, rtdev_path, [
        {root,     "/Users/goncalotomas/git/riak_tests/_build/default/lib/riak"},
       {current,  "/Users/goncalotomas/git/riak_tests/_build/default/lib/riak"},
       {previous, "/Users/goncalotomas/git/riak_tests/_build/default/lib/riak"},
       {legacy,   "/Users/goncalotomas/git/riak_tests/_build/default/lib/riak"}
    ]).
