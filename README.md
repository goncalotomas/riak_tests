# riak_test 2.0

Riak Test has historically been the singular code repository which concerns testing multiple deployment features of Riak.
Its purpose built test runner accumulated some technical debt and now presents a significant barrier of entry for new contributions.  
This is a Google Summer of Code 2018 project for the BEAM Community. The mentors of this project are Gordon Guthrie, Bryan Hunt and Russell Brown.


## Setup
You will need Erlang 16 to run this code. There are still some missing pieces in the transition of Riak's codebase to
Erlang 21+, so a bundled version of [rebar3][2] is included that works with Erlang 16.  

## Running tests

You can run tests by running `./rebar3 ct`. In this process a release of the currently selected version of Riak (by
default the latest) will be generated, which is expected to take a while.

[1]: https://github.com/basho/riak_test
[2]: http://www.rebar3.org/
