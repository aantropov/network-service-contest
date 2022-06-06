-module(matrix_sup).

-behaviour(supervisor).

-export([
    start_link/0
]).

-export([
    init/1
]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 3, period => 10},
    {ok, {SupFlags, [
        #{id => acceptor_sup, start => {matrix_acceptor_sup, start_link,[]}, type => supervisor},
        #{id => worker_sup, start => {matrix_worker_sup, start_link,[]}, type => supervisor},
        #{id => listen, start => {matrix_listen, start_link,[]}}
    ]}}.
