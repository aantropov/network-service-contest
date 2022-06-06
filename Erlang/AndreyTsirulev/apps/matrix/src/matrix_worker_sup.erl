-module(matrix_worker_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    start_children/1
]).

-export([
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_children(N) ->
    [ supervisor:start_child(?MODULE, []) || _ <- lists:seq(1, N) ],
    ok.

init([]) ->
    ChildSpec = #{id => worker, start => {matrix_worker, start_link, []}, restart => permanent},
    {ok, {#{strategy => simple_one_for_one}, [ChildSpec]}}.
