-module(matrix_acceptor).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([
    start_link/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {listen_socket, acceptor}).

start_link(ListenSocket) -> gen_server:start_link(?MODULE, ListenSocket, []).

init(ListenSocket) ->
    {ok, Ref} =  prim_inet:async_accept(ListenSocket, -1),
    {ok, #state{listen_socket = ListenSocket, acceptor = Ref}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({inet_async, ListenSocket, Ref, {ok, ClientSocket}}, #state{listen_socket = ListenSocket, acceptor=Ref} = State) ->
    try
        inet_db:register_socket(ClientSocket, inet_tcp),
        prim_inet:setopts(ClientSocket, [binary, {packet, raw}, {active, false}]),
        ConnectionPid = erlang:spawn(matrix_connection, handle, [ClientSocket]),
        gen_tcp:controlling_process(ClientSocket, ConnectionPid),
        ConnectionPid ! start
    catch
        _:Reason ->
            ?LOG_ERROR(#{what => acceptor_init_client_error, error => Reason})
    end,
    {ok, NewRef} =  prim_inet:async_accept(ListenSocket, -1),
    {noreply, State#state{acceptor = NewRef}};
handle_info ({inet_async, _, _, Error}, State) ->
    ?LOG_ERROR(#{what => acceptor_error, error => Error}),
    {stop, Error, State};
handle_info (_Info, State) ->
    {noreply, State}.

