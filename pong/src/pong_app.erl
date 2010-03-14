%% Author: huiver
%% Created: 9 sep 2009
%% Description: LED-art.nl pong project
-module(pong_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([start/2, stop/1]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
%%
%% stop the pong app
%%
stop(_Arg0) -> 
	ok.
%%
%% Start the supervisor.
%%
start(_Type, _StartArgs) -> 
	pong_supervisor:start_link().
