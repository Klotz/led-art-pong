%% Application resource file for the pong app

{application, pong,
	[{description, "Led-art.nl Pong"},
	 {vsn, "0.1"},
	 {modules, [pong_app, pong_supervisor,
	 			led_server, protocol_server,
	 			executives, game_server]},
	 {registered, [protocol_server, game_server]},
	 {applications, [kernel, stdlib]},
	 {mod, {pong_app, []}},
	 {start_phases, []}
	]
}.