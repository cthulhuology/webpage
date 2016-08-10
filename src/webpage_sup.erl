-module(webpage_sup).
-behaviour(supervisor).
-export([start_link/0, server/2]).
-export([init/1]).


-define(WEBPAGE_SERVER(P), list_to_atom("webpage_server_" ++ integer_to_list(P))).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, { {one_for_one, 5, 10}, [
		#{ 
			id => webpage_router,
			start => { webpage_router, start_link, []},
			restart => permanent,
			shutdown => brutal_kill,
			type => worker,
			modules => [ webpage_router ]
		},
		#{
			id => webpage_server,
			start => { webpage_server, start_link, [ webpage_router, 443 ]},
			restart => permanent,
			shutdown => brutal_kill,
			type => worker,
			modules => [ webpage_server, webpage_router, webpage ]
		}
	]}}.

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
