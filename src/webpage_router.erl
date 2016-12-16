-module(webpage_router).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).

%% router behavior
-export([ start_link/0, stop/0, add/2, remove/1, paths/0, route/1 ]).

%% http method mapping behavior
-export([ get/1, post/1, put/1, delete/1, options/1, head/1, trace/1, connect/1 ]).

%% gen_server behavior
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

%% import/export
-export([ json_to_route/1, route_to_json/1, load_route/1 ]).

-include("include/http.hrl").
-record(webpage_router, { paths = []}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

get(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ dispatch, get, Path, Req}).

post(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ dispatch, post, Path, Req}).

put(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ dispatch, put, Path, Req}).

delete(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ dispatch, delete, Path, Req}).

options(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ dispatch, options, Path, Req}).

head(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ dispatch, head, Path, Req}).

trace(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ dispatch, trace, Path, Req}).

connect(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ dispatch, connect, Path, Req}).

start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE,stop).

add(Path,Routes) ->
	gen_server:cast(?MODULE,{add,Path,Routes}).

remove(Path) ->
	gen_server:cast(?MODULE,{remove,Path}).

paths() ->
	gen_server:call(?MODULE, paths).

route(Path) ->
	gen_server:call(?MODULE, { route, Path }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([]) ->
	Paths = json:decode(webpage_rest:get("/routes")),
	error_logger:info_msg("Routes are: ~p~n", [ Paths ]),
	Routes = sort_routes([ load_route("/" ++ binary:bin_to_list(Path)) || Path <- Paths ]),
	{ ok, #webpage_router{ paths = lists:reverse(Routes) }}.

handle_call(paths,_From,State = #webpage_router{ paths = Paths }) ->
	{ reply, webpage_path:order(Paths), State };

handle_call({ route, Path }, _From, State = #webpage_router{ paths = Paths }) ->
	{ reply, webpage_path:scan(Path,Paths), State };

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call({dispatch,Method,Path,Request}, _From, State = #webpage_router{ paths = Paths }) ->
	case webpage_path:scan(Path,Paths) of
		Routes when is_list(Routes) ->
			Res = lists:foldl(fun(Route,R) -> route(Method,Route,R) end, Request, Routes),
			{ reply, Res, State };
		_ ->
			error_logger:info_msg("Route failed: ~p", [ Path ]),
			{ reply, #response{ status = 404, body= <<"Not Found">> }, State }
	end;

handle_call(Message,_From,State) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ reply, #response{ status = 405 }, State }.


handle_cast({ add, Path, Routes }, State = #webpage_router{ paths = Paths }) ->
	webpage_rest:post({ "/routes", json:encode( [
		{ <<"path">>, list_to_binary(Path) }, 
		{ <<"routes">>, Routes },
		{ <<"time">>, os:system_time() }
	])}),
	{ noreply, State#webpage_router{ paths = [ { Path, Routes } | proplists:delete(Path,Paths) ]}};	

handle_cast({ remove, Path }, State = #webpage_router{ paths = Paths }) ->
	webpage_rest:delete("/routes/path/" ++ url:encode(Path)),
	{ noreply, State#webpage_router{ paths = proplists:delete(Path,Paths) }};	

handle_cast(Message,State) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ noreply, State }.

code_change(_Old,_Extra,State) ->
	{ ok, State }.

terminate(_Reason,_State) ->
	ok.

route(_Method, [ Module, Function, Args ],R) ->		%% route defined function
	error_logger:info_msg("Route to ~p:~p(~p)", [ Module, Function, Args ]),
	Functions = Module:module_info(functions),
	case proplists:lookup(Function,Functions) of
		{ Fun, 1 } ->
			case R of 
				#request{} ->
					RQ = R#request{ module = Module, args = Args },
					Module:Fun(RQ);
				#response{} ->
					RS = R#response{ module = Module, args = Args },
					Module:Fun(RS)
			end;	
		_ ->
			#response{ status = 405 }
	end;
route(Method, [ Module, Args ], R) ->			%% route request supplied function
	error_logger:info_msg("Route to ~p:~p(~p)", [ Module, Method, Args ]),
	Functions = Module:module_info(functions),
	case proplists:lookup(Method,Functions) of
		{ Fun, 1 } ->
			case R of 
				#request{} ->
					RQ = R#request{ module = Module, args = Args },
					Module:Fun(RQ);
				#response{} ->
					RS = R#response{ module = Module, args = Args },
					Module:Fun(RS)
			end;	
		_ ->
			#response{ status = 405 }
	end;

route(_Method, _, _R) ->
	#response{ status = 405 }.

%% decode a json array into a route
json_to_route([]) ->
	[];
json_to_route(JSON) when is_binary(JSON) ->
	[ json_to_route(Route) || Route <- json:decode(JSON) ];
json_to_route([ Module, Args ]) ->
	[ list_to_atom(binary_to_list(Module)), json_to_route(Args) ];	
json_to_route([ Module, Function, Args ]) ->
	[ list_to_atom(binary_to_list(Module)), list_to_atom(binary_to_list(Function)), json_to_route(Args) ];
json_to_route([ X ]) when is_list(X) ->
	[ json_to_route(X) ].

%% encode a route into json
route_to_json(Route) ->
	json:encode(Route).

load_route(Path) ->
	error_logger:info_msg("Loading ~p", [ Path ]),
	JSON = webpage_rest:get(Path),
	error_logger:info_msg("JSON ~p", [ JSON ]),
	O = json:decode(JSON), 
	error_logger:info_msg("Term ~p", [ O ]),
	{ binary_to_list(proplists:get_value(<<"path">>,O)), json_to_route(proplists:get_value(<<"routes">>,O)), proplists:get_value(<<"time">>,O) }.

sort_routes(Routes) ->
	Sorted = lists:sort(fun({_,_,A },{ _,_, B }) -> A < B end, Routes),
	io:format("Sorted lists ~p~n", [ Sorted ]),
	[ { X, Y } || { X, Y, _Z } <- Sorted ].

