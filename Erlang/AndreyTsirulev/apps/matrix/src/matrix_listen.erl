-module(matrix_listen).

-behaviour(gen_server).

-export([
	start_link/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {socket}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Port = application:get_env(matrix, port, 5000),
    Acceptors = application:get_env(matrix, acceptors, 3),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}]),
    matrix_acceptor_sup:start_children(ListenSocket, Acceptors),
    {ok, #state{socket = ListenSocket}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
