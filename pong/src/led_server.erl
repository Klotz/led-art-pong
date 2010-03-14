%%% -------------------------------------------------------------------
%%% Author  : hhv
%%% Description : LED server,simulates the real LED server.
%%%
%%% Created : 4 sep 2009
%%% -------------------------------------------------------------------
-module(led_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, add_socket/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {listen, socket}).

%% ====================================================================
%% External functions
%% ====================================================================
%%
%% Starts the server
%%
start_link(Port) -> 
	gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

add_socket(Socket) ->
	gen_server:call(?MODULE, {add_socket, Socket}),
	ok.

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Port) ->
%%	io:format("~p starting~n", [?MODULE]),
	{ok, Listen} = gen_tcp:listen(Port, [binary,{packet,0},
								  {reuseaddr, true}, {active, true}]),
	Server = self(),
	link(spawn(fun() ->
				  wait_for_connection(Listen, Server) end)),
    {ok, #state{listen=Listen}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({add_socket, Socket}, _From, State) ->
%%	io:format("~p: handle_call {add_socket,~p}~n", [?MODULE, Socket]),
    {reply, ok, State#state{socket=Socket}};
handle_call(_Request, _From, State) ->
	io:format("~p: handle_call Request=~p~n", [?MODULE, _Request]),
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, State) ->
	{ok, {Host, _Port}} = inet:peername(Socket),
	io:format("~p: receive{tcp, ~p, ~p}~n", [?MODULE, Host, Bin]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: wait_for_connection/1 ->
%% Description: Wait for socket connection
%% --------------------------------------------------------------------
wait_for_connection(Listen, Server) ->
	{ok, Socket} = gen_tcp:accept(Listen),
	gen_tcp:controlling_process(Socket, Server),
	{ok, {Host, _Port}} = inet:peername(Socket),
	io:format("~p: connection from ~p~n", [?MODULE, Host]),
	add_socket(Socket).