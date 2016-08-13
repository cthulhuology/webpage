-module(webpage_websocket_sup).
-behaviour(supervisor).
-export([start_link/0, client/1 ]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE,[]).

init([]) ->
	{ ok, { {one_for_one, 5, 10}, [] } }.


client(Request) ->
	io:format("starting child  ~p~n", [ Request ]),
	supervisor:start_child(?MODULE, #{ 
		id => uuid:id(),
		start => { webpage_websocket, start_link, [ Request ]},
		restart => temporary,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			webpage_websocket
		]
	}).
