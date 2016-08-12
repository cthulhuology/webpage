-module(webpage_websocket_sup).
-behaviour(supervisor).
-export([start_link/0, client/3 ]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE,[]).

init([]) ->
	{ ok, { {one_for_one, 5, 10}, [] } }.


client(Socket,Req,Module) ->
	io:format("starting child ~p, ~p, ~p~n", [ Socket, Req, Module ]),
	supervisor:start_child(?MODULE, #{ 
		id => uuid:id(),
		start => { webpage_websocket, start_link, [ Socket, Req, Module ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			webpage_websocket
		]
	}).
