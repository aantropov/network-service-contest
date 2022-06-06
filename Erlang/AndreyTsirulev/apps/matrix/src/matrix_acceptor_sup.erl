-module(matrix_acceptor_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    start_children/2
]).

-export([
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_children(ListenSocket, N) ->
    [ supervisor:start_child(?MODULE, [ListenSocket]) || _ <- lists:seq(1, N) ],
    ok.

init([]) ->
    ChildSpec = #{id => acceptor, start => {matrix_acceptor, start_link, []}, restart => permanent},
    {ok, {#{strategy => simple_one_for_one}, [ChildSpec]}}.
