%%% -------------------------------------------------------------------
%%% Author  : huiver
%%% Description :
%%%
%%% Created : 26 aug 2009
%%% -------------------------------------------------------------------
-module(executives).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("game.hrl").
-include("media.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, execute/1, test/1, demo/0, player_to_player/4, lost/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
% The State for the server. executive contains the Pid of the function that
% is running. Before a new executive is started, the old executive is sent
% a 'stop' message.
-record(state, {executive=void, mplayer=void}).



%% ====================================================================
%% External functions
%% ====================================================================
%%
%% Start the game server.
%%
start_link(Parameters) -> 
	gen_server:start_link({local,?MODULE}, ?MODULE, Parameters, []).

% Execute an 'executive' function
execute(Executive) ->
	gen_server:call(?MODULE, {execute, Executive}).

% Move ball from Player1 to Player2
player_to_player(Player1, Player2, TimeSpan, CallAfterwards) ->
	gen_server:call(?MODULE, {player_to_player, Player1, Player2, TimeSpan, CallAfterwards}).
	
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
init(Path) ->
%%	io:format("Path=~p~n", [Path]),
	io:format("os:type=~p, find_executable('mplayer')=~p~n", [os:type(), os:find_executable("mplayer", Path)]),
	MPlayer = os:find_executable("mplayer", Path),
    {ok, #state{executive=void, mplayer=MPlayer}}.

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
% Handle the executive() call here.
handle_call({execute, Executive}, _From, State) ->
	NewState = execute(State, Executive),
	{reply, ok, NewState};

handle_call({player_to_player, Player1, Player2, TimeSpan, CallAfterwards}, _From, State) ->
	player_to_player(State, Player1, Player2, TimeSpan, CallAfterwards),
	{reply, ok, State};
		
handle_call(Message, From, State) ->
	io:format("~p ignoring handle_call(~p, ~p, ~p)~n", [?MODULE, Message, From, State]),
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
handle_info(Info, State) ->
	io:format("~p ignoring handle_info(~p, ~p)~n",[?MODULE, Info, State]),
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

%% Start the new 'executive' function
% Spawn the new executive. Return new State.
execute(#state{executive = void} = State, Executive) ->
	State#state{executive = spawn(Executive)};
% Kill the previous executive and spawn the new executive. Return new State.
execute(#state{executive = OldExecutive} = State, Executive) ->
	OldExecutive ! stop,
	State#state{executive = spawn(Executive)}.

%%
% Demo mode executive
demo() ->
	demo_loop(1, 1, ?NR_OF_LEDS, 1).

% Demo mode executive loop
demo_loop(Pixel, MinPixel, MaxPixel, Next) ->
	receive
		stop ->
			void
		after 50 ->  %% Timeout 
			set_pixel(Pixel - (4 * Next), MinPixel, MaxPixel, {0, 0, 0}),
			set_pixel(Pixel - (3 * Next), MinPixel, MaxPixel, {120, 120, 120}),
			set_pixel(Pixel - (2 * Next), MinPixel, MaxPixel, {160, 160, 160}),
			set_pixel(Pixel - (1 * Next), MinPixel, MaxPixel, {200, 200, 200}),
			set_pixel(Pixel, MinPixel, MaxPixel, {255, 255, 255}),
			if
				(Pixel + Next > MaxPixel orelse Pixel + Next < 1) ->
					demo_loop(Pixel + -Next, MinPixel, MaxPixel, -Next);
				true ->
					demo_loop(Pixel + Next, MinPixel, MaxPixel, Next)
			end
	end.

% Start a background tune with mplayer, return Port
background_tune(File) ->
	_Port = open_port({spawn, "/usr/bin/afplay ./resources/button-29.mp3"}, []).

%% Ping pong
% Move ball from player P1 to player P2
player_to_player(#state{mplayer = MPlayer} = _State, 1, 2, TimeSpan, CallAfterwards) ->
	io:format("CallAfterwards=~p~n",[CallAfterwards]),
	_Port = open_port({spawn_executable, MPlayer}, [{args, [?MEDIA_BUTTON_PRESSED]}]),
	move_ball(1, ?NR_OF_LEDS, 1, 1, {255, 255, 0}, round(TimeSpan / ?NR_OF_LEDS)),
	spawn(CallAfterwards);
player_to_player(#state{mplayer = MPlayer} = _State, 2, 1, TimeSpan, CallAfterwards) ->
	_Port = open_port({spawn_executable, MPlayer}, [{args, [?MEDIA_BUTTON_PRESSED]}]),
	move_ball(1, ?NR_OF_LEDS, ?NR_OF_LEDS, -1, {255, 255, 0}, round(TimeSpan / ?NR_OF_LEDS)),
	spawn(CallAfterwards).

move_ball(From, To, Pixel, Next, Color, Pause) ->
	receive
		stop ->
			void
		after Pause ->
			set_pixel(Pixel, From, To, Color),
			set_pixel(Pixel - Next, From, To, {0, 0, 0}),
			if
				(Next == 1) and (Pixel + Next > To) ->
					void;
				(Next == -1) and (Pixel + Next < From) ->
					void;
				true ->
					move_ball(From, To, Pixel + Next, Next, Color, Pause + 2)
			end
	end.
	
% Set pixel if it is within range.
set_pixel(Pixel, MinPixel, MaxPixel, Color) ->
	if
		(Pixel =< MaxPixel andalso Pixel >= MinPixel) ->
			protocol_server:set_pixel(Pixel, Color);
		true -> void
	end.
	
%%
% Test the equipment.
test(Timeout) ->
	protocol_server:set_color({255, 255, 255}),
	receive after Timeout -> true end,
	protocol_server:set_color({255, 0, 0}),
	receive after Timeout -> true end,
	protocol_server:set_color({0, 255, 0}),
	receive after Timeout -> true end,
	protocol_server:set_color({0, 0, 255}),
	receive after Timeout -> true end,
	protocol_server:set_color({0, 0, 0}),
	void.

	
%%
% Player P lost.
lost(_P) ->
	protocol_server:set_color({255, 0, 0}),
	void.

