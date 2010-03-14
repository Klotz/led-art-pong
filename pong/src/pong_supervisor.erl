%%% -------------------------------------------------------------------
%%% Author  : huiver
%%% Description :
%%%
%%% Created : 29 aug 2009
%%% -------------------------------------------------------------------
-module(pong_supervisor).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, start_link/0, start_in_shell_for_testing/0, start/0]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================

%%
%% Start the supervisor.
%%
start_link() -> 
	supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).
%%
%% Start the supervisor in the shell.
%%
start_in_shell_for_testing() -> 
	{ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
	unlink(Pid).

%%
%% Start the supervisor.
%%
start() -> 
	ok.


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	LEDserver = {tag1,
			{led_server, start_link, [5000]},
			permanent,
			10000,
			worker,
			[led_server]},
	Protocol = {tag2,
			{protocol_server, start_link, [[{"10.0.0.1", 3000, void, <<>>}, {"127.0.0.1", 55555, void, <<>>}]]},
			permanent,
			10000,
			worker,
			[protocol_server]},
	Executives = {tag4,
			{executives, start_link, ["/Applications/MPlayer\ OSX.app/Contents/Resources/External_Binaries/mplayer.app/Contents/MacOS/:/usr/bin/"]},
			permanent,
			10000,
			worker,
			[executives]},
    Game = {tag3,
			{game_server, start_link, []},
			permanent,
			10000,
			worker,
			[game_server]},
    {ok,{{one_for_one,0,1}, [LEDserver, Game, Protocol, Executives]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

