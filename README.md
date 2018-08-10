# riak_test 2.0

Riak Test has historically been the singular code repository which concerns testing multiple deployment features of Riak.
Its purpose built test runner accumulated some technical debt and now presents a significant barrier of entry for new contributions. We propose to rework Riak Test by using mostly standard frameworks like Common Test to perform the tests. This implies an effort of converting the old test suites in a way that is compliant with Common Test's API, but also to provide very explicit failure context that was not available previously.

This is a Google Summer of Code 2018 project for the BEAM Community. The mentors of this project are [Gordon Guthrie][gordon], [Bryan Hunt][bryan] and [Russell Brown][russell].


## Setup
You will need Erlang 16 to run this code. There are still some missing pieces in the transition of Riak's codebase to
Erlang 21+, so a bundled version of [rebar3][2] is included that works with Erlang 16.  

## Running tests

You can run tests by running `./rebar3 ct`. In this process a release of the currently selected version of Riak (by
default the latest) will be generated, which is expected to take a while.

### Using a single test suite

If you're in a hurry or you just need to run a particular set of tests, you can take advantage of rebar3 itself to just
run a single suite:

```sh
./rebar3 ct --suite test/verify_build_cluster_SUITE.erl
```

### Failing test suites
Each test suites makes sure to generate a new devrel to make sure it is using a clean and deterministic test
environment. This is done in the `init_per_suite/1` function. If something goes wrong along the test suite, it
is possible that the test step that takes the nodes down is skipped, leaving you with some undead Riak nodes.
Be sure to take extra care after getting a failing test suite and run `killall beam.smp` or similar so as to
not propagate errors to other suites (because they will not be able to start up nodes with the same name).

#### Known issues
- `bucket_props_validation_SUITE` and `bucket_types_SUITE` have an issue with setting and getting bucket type properties via protocol buffers.
- Intercepts are still not working and cannot be added just yet.

### Cleaning devrel manually
If you are running a test suite at a time you may also find it useful to be able to clean the devrel inside your Riak dependency. To do so run the following from the project root:

```sh
./priv/clean_devrel <RIAK_DEP_NAME>
```

Where `<RIAK_DEP_NAME>` is the name under which the Riak dependency appears inside `_build/test/lib`. If you've not configured a different version of Riak, this should just be `riak`.


[1]: https://github.com/basho/riak_test
[2]: http://www.rebar3.org/
[bryan]: https://github.com/binarytemple
[gordon]: https://github.com/gordonguthrie
[russell]: https://github.com/russelldb
