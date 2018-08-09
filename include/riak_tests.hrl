-define(APP, riak_tests).
-define(PRIV_DIR, code:priv_dir(?APP)).
-define(DEPS_DIR,
    string:sub_string(?PRIV_DIR, 1, string:rstr(?PRIV_DIR, "riak_tests/priv") - 1)).
