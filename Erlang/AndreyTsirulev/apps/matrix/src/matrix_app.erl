-module(matrix_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    {ok, SupPid} = matrix_sup:start_link(),
    N = application:get_env(matrix, workers, 50),
    persistent_term:put(workers, N),
    matrix_worker_sup:start_children(N),
    {ok, SupPid}.

stop(_State) ->
    ok.
