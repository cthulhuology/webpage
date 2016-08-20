-module(webpage_sup).
-behaviour(supervisor).
-export([start_link/0, server/2]).
-export([init/1]).


-define(WEBPAGE_SERVER(P), list_to_atom("webpage_server_" ++ integer_to_list(P))).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ ok, Webpage } = application:get_env(webpage),
	Servers = [ #{ 
		id =>  Id,
		start => { Service, start_link, Args },
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules =>  Modules
		} || { Id, Service, Args, Modules } <- Webpage ], 
	{ok, { {one_for_one, 5, 10}, [
		#{ id => webpage_websocket_sup,
		start => { webpage_websocket_sup, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => supervisor,
		modules => [
			webpage_websocket_sup
		]},
		#{ id => webpage_rest,
		start => { webpage_rest, start_link, [] },
		restart => permanent,
		shutdown => brutal_kill,
		type => supervisor,
		modules => [
			webpage_rest
		]}
		| Servers ] }}.

server(Module,Port) ->
	supervisor:start_child(?MODULE, #{ 
		id =>  ?WEBPAGE_SERVER(Port),
		start => { webpage_server, start_link, [ Module, Port ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			webpage_server,
			webpage
		]}).
