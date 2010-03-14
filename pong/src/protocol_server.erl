%% @author Huib Verweij <hhv@home.nl
%% @doc Sends low-level LED commands to the LED controller.
%% @copyright 2009 Led-Art.nl &amp; Huib Verweij.

-module(protocol_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("protocol.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, set_pixel/2, set_color/1, reconnect_servers/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% The state of the protocol_server.
% led_servers is a list containing tuples {Host, Port, void, <<>>} or {Host, Port, Socket, Bin}.
% data_received is a list containing tuples {Socket, Bin}. Bin is a binary of all data received through Socket.
-record(state, {led_servers = []}).

%% ====================================================================
%% External functions
%% ====================================================================
%
% Start the server.
start_link(LEDservers) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, LEDservers, []).


%
% Set a pixel to a RGB (={Red, Green, Blue}) value.
set_pixel(Pixel, Color) ->
	gen_server:call(?MODULE, {set_pixel, Pixel, Color}).


%
% Kleur de hele balk.
set_color(Color) ->
	gen_server:call(?MODULE, {set_color, Color}).


%
% (Re)connect the servers.
reconnect_servers() ->
	gen_server:call(?MODULE, {reconnect_servers}).



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
init(LEDservers) ->
	spawn(fun() ->
				  periodically_reconnect_servers(10000) end),
    {ok, #state{led_servers=LEDservers}}.

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
% Color one pixel
handle_call({set_pixel, Pixel, {Red, Green, Blue} = _Color}, _From, State) ->
	send_data_to_led_servers(State#state.led_servers, [?HEADER, ?CMD_SETSTRIPE, Pixel, Red, Green, Blue]),
    {reply, ok, State};

% Color one row
handle_call({set_color, {Red, Green, Blue} = _Color}, _From, State) ->
	send_data_to_led_servers(State#state.led_servers, [?HEADER, ?CMD_SETROW, Red, Green, Blue, 0]),
    {reply, ok, State};

% Yippie, we have a new connection!
% Replace {H, P, _, _} with {H, P, Socket, <<>>} in the LEDserver list.
handle_call({add_LEDserver, {Host, Port, Socket}}, _From, #state{led_servers = LEDservers} = State) ->
	NewLEDServers = lists:map(fun({H, P, S, B}) -> if (H == Host andalso P == Port) -> {H, P, Socket, <<>>}; true -> {H, P, S, B} end end, LEDservers),
	{reply, ok, State#state{led_servers = NewLEDServers}};

% Called periodically to try to connect to unconnected servers.
handle_call({reconnect_servers}, _From, #state{led_servers=LEDservers} = State) ->
	connect_to_led_servers(LEDservers),
    {reply, ok, State};

% Report any other call.
handle_call(Request, _From, State) ->
	error_logger:info_msg("~p: received unknown request ~w~n", [?MODULE, Request]),
    {reply, ok, State}.

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

% Received Bin from Socket, add process_input_from_led_server(Bin)
% to the binary data previously received through Socket.
handle_info({tcp, Socket, Bin}, #state{led_servers = LEDservers} = State) ->
	% error_logger:info_msg("received Bin=~p~n", [Bin]),
	NewLEDservers = lists:map(fun({H, P, S, B}) -> if (S == Socket) -> {H, P, S, process_input_from_led_server(list_to_binary([B, Bin]))}; true -> {H, P, S, B} end end, LEDservers),
    {noreply, State#state{led_servers = NewLEDservers}};
	
% A connection was lost, set Socket of lost connection to void.
handle_info({tcp_closed, Socket}, #state{led_servers=LEDservers} = State) ->
	[{Host, Port, _, _}] =[{H, P, S, B} || {H, P, S, B} <- LEDservers, S == Socket],
	error_logger:info_msg("~p: lost connection to server ~p:~p~n", [?MODULE, Host, Port]),
	{noreply, State#state{led_servers=lists:map(fun({H, P, S, B}) -> if (S == Socket) -> {H, P, void, <<>>}; true -> {H, P, S, B} end end, LEDservers)}};
	
% Generic handler, just report.
handle_info(_Info, State) ->
	error_logger:info_msg("~p: handle_info(~p, ~p)~n", [?MODULE, _Info, State]),
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Send data to all led_servers.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_data_to_led_servers(L, Data) -> 
	lists:map(fun({_H, _P, S, _B}) -> if S == void -> void; true -> send_data_to_socket(S, Data) end end, L).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Add checksum at end and send off to the Socket.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_data_to_socket(Socket, Data) ->
	gen_tcp:send(Socket, Data ++ <<(lists:sum(Data)):8>>).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Connect to LED servers
%% Parameter: LEDservers list
%% Returns:  void
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connect_to_led_servers(L) ->
	lists:map(fun({Host, Port, void, _}) -> Server = self(), spawn(fun() -> connect_to_led_server({Host, Port, Server}) end); (_) -> void end, L).

% Connect to one particular server (Host/Port) using gen_tcp module.
connect_to_led_server({Host, Port, Server}) ->
	% error_logger:info_msg("~p: trying to connect to server ~p:~p~n", [?MODULE, Host, Port]),
	case gen_tcp:connect(Host, Port, [binary, {packet,0}, inet], 3000) of
		{ok, Socket} ->
			gen_tcp:controlling_process(Socket, Server),
			error_logger:info_msg("~p: connected to server ~p:~p~n", [?MODULE, Host, Port]),
			gen_server:call(?MODULE, {add_LEDserver, {Host, Port, Socket}});
		{error, _Errorcode} ->
			% error_logger:info_msg("~p: failed to connect to server ~p:~p: ~p~n", [?MODULE, Host, Port, _Errorcode]),
			void
	end.

%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stuur elke T milliseconden een commando om (opnieuw) met de servers te verbinden.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
periodically_reconnect_servers(T) ->
	protocol_server:reconnect_servers(),
	receive	after T -> true	end,
	periodically_reconnect_servers(T).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process input from one led server.
% Return updated Bin (possible garbage and first n commands removed).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_input_from_led_server(Bin) ->
	% io:format("~p: process_input_from_led_server(~p)~n", [?MODULE, Bin]),
	Cleaned = remove_garbage(Bin),
	if 
		size(Cleaned) >= 7 ->
			{Command, Rest} = split_binary(Cleaned, 7),
			process_command(Command),
			process_input_from_led_server(Rest);
		true ->
			Cleaned
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Remove every byte that is not the ?HEADER from the front of Bin.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_garbage(<<?HEADER:8, _>> = Bin) -> Bin;
remove_garbage(<<_:8, Bin>>) -> remove_garbage(Bin);
remove_garbage(Any) -> Any.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process command from server.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check the checksum, ignore any commands that fail the checksum check.
process_command(<<Header:8, D1:8, D2:8, D3:8, D4:8, D5:8, Checksum:8>> = _Bin) ->
	<<CalculatedChecksum>> = <<(lists:sum([Header, D1, D2, D3, D4, D5])):8>>,
	if
		(CalculatedChecksum == Checksum) ->
			interpret_command(<<D1, D2, D3, D4, D5>>);
		true ->
			error_logger:info_msg("~p: bad checksum in ~p, Checksum=~p, CalculatedChecksum=~p, D1=~p~n", [?MODULE, _Bin, Checksum, CalculatedChecksum, D1]),
			void
	end.


% Interpret the bytes and execute the command.
interpret_command(<<?TX_BUTTONSTATE:8/integer, Button:8/integer, Force:8/integer, _Dummy:16/integer>> = _Bin) ->
	game_server:playerPushedButton(Button, Force);

% Ignore any other command.
interpret_command(<<_:40>> = _Bin) ->
	void.