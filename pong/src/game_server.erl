%% @author Huib Verweij <hhv@home.nl
%% @doc Server that manages the gameplay.
%% The module is the central server that determines gameplay,
%% it issues high-level commands to be sent to the remote servers
%% and receives input from remote servers (e.g. button presses). 
%% @copyright 2009 Led-Art.nl &amp; Huib Verweij.

-module(game_server).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("game.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, playerPushedButton/2, check_player_response/0, demo/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% This is the record that keeps the State of the game_server.
% gamestate = ?UNINITIALISED, ?TEST, ?DEMO, ?PLAYING
% turn = player who's turn it is
% timespan = initial time used to move the ball from side to side
%	(actual time can be influenced by the function that moves the ball, f.i. it can slow the ball))
% last_time_button_pressed = timestamp of last button press of current user.
-record(state, {gamestate, turn, timespan, last_time_button_pressed}).



%% ====================================================================
%% External functions
%% ====================================================================
%%
% Start the game server.
start_link() -> 
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%%
% Report a pushed button.
playerPushedButton(Button, Force) ->
	gen_server:call(?MODULE, {pushedButton, Button, Force}).

%%
% It's time to check if player #state.turn returned the ball
check_player_response() ->
	gen_server:call(?MODULE, checkResponse).

%%
% Activate demo mode
demo() ->
	gen_server:call(?MODULE, demo).

%% @doc Move the ball from one side to the other.
%% 
%% @spec move_ball(list()) -> void 

-spec(move_ball(list()) -> void).

move_ball(Parameters) ->
	gen_server:call(?MODULE, {set_pixel, Parameters}).

%% @doc Move the ball from one side to the other.
%% 
%% @spec set_row(tuple()) -> void 

-spec(set_row(tuple()) -> void).

set_row(Color) ->
	gen_server:call(?MODULE, {set_row, Color}).




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
init([]) ->
	% The 1000ms timeout causes handle_info(timeout, State) to be called to start things up.
    {ok, #state{gamestate=?UNINITIALISED, turn=0, timespan=1000, last_time_button_pressed=0}, 1000}.



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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handle a pushed button
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handle any button press when in demo mode: start the game!
handle_call({pushedButton, Button, _Force}, _From, #state{gamestate = ?DEMO, timespan = Timespan} = State) ->
	% Roll the ball across for Timespan ms, check at the end if the other player hit it back (in another process).
	executives:execute(fun() -> executives:player_to_player(Button, next_turn(Button), Timespan, fun() -> game_server:check_player_response() end) end),
	% Switch to ?PLAYING state now and remember who's turn it is.
	{reply, ok, State#state{gamestate=?PLAYING, turn=next_turn(Button), last_time_button_pressed = 0}};
			
% Remember the timestamp of a button press when it's the players turn and the gamestate is ?PLAYING
handle_call({pushedButton, Button, _Force}, _From, #state{gamestate = ?PLAYING, turn = Button} = State) ->
	{reply, ok, State#state{last_time_button_pressed=now()}};

% ... otherwise ignore it ...
handle_call({pushedButton, _Button, _Force}, _From, State) ->
	{reply, ok, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check if player hit the ball back.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The player didn't hit the button at all (because "last_time_button_pressed = 0")! You loose, sucker!
handle_call(checkResponse, _From, #state{gamestate = ?PLAYING, turn = Turn, last_time_button_pressed = 0} = State) ->
	player_lost(Turn),
	{reply, ok, State#state{gamestate = ?LOST, turn = 0, last_time_button_pressed = 0}};
	
% Make sure that the player hit the button within the last N ms
handle_call(checkResponse, _From, #state{gamestate = ?PLAYING, turn = Turn, timespan = Timespan, last_time_button_pressed = LastTimeButtonPressed} = State) ->
	N = 300,
	MilliSecondsSinceButtonPressed = round(timer:now_diff(now(), LastTimeButtonPressed) / 1000),
	if
		(MilliSecondsSinceButtonPressed =< N) ->
			% Player responded in time, send ball the other way (in another process) and change State to record that it's the other player's turn.
			executives:execute(fun() -> executives:player_to_player(Turn, next_turn(Turn), Timespan, fun() -> game_server:check_player_response() end) end),
			{reply, ok, State#state{turn = next_turn(Turn), last_time_button_pressed = 0}};
		true ->
			player_lost(Turn),
			{reply, ok, State#state{gamestate = ?LOST, turn = 0, last_time_button_pressed = 0}}
	end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Others...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Activate demo mode.
handle_call(demo, _From, State) ->
	executives:execute(fun() -> executives:demo() end),
	{reply, ok, State#state{gamestate = ?DEMO, turn = 0, last_time_button_pressed = 0}};

% Record unknown message.
handle_call(Message, From, State) ->
	error_logger:info_msg("~p ignoring handle_call(~p, ~p, ~p)~n", [?MODULE, Message, From, State]),
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

% Start a test after a very short initial timeout from the init() function.
handle_info(timeout, #state{gamestate=?UNINITIALISED} = State) ->
	executives:test(100), % NOT a background function / other process !
	{noreply, State#state{gamestate=?TEST}, 1};
	
% After initial testing, go to demo mode
handle_info(timeout, #state{gamestate=?TEST} = State) ->
	executives:execute(fun() -> executives:demo() end),
    {noreply, State#state{gamestate=?DEMO}};

% Record unknown info.
handle_info(Info, State) ->
	error_logger:info_msg("~p ignoring handle_info(~p, ~p)~n",[?MODULE, Info, State]),
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

% Calculate who's next.
% E.g. when player 1 just hit the ball, it is now player 2's turn
next_turn(1) -> 2;
next_turn(2) -> 1.

%%
% Handle the case when a player lost, this is called from the handle_call functions.
player_lost(Turn) ->
	executives:execute(fun() -> executives:lost(Turn) end),
	timer:apply_after(1000, game_server, demo, []).
