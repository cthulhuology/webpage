-module(webpage_rest).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/0, stop/0, install/1, get/1, put/1, post/1, delete/1 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).
-export([ filter_objects/2]).

-include("include/http.hrl").

-record(webpage_rest, {}).
-record(rest, { guid, bucket, object  }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE,stop).
get(#request{ path = Path}) ->
	gen_server:call(?MODULE, { get, Path });
get(Res = #response{}) ->
	Res;
get(Path) when is_list(Path) ->
	#response{ body = Object } = gen_server:call(?MODULE, { get, Path }),
	Object.

put(#request{ path = Path, body = Body }) ->
	gen_server:call(?MODULE, { put, Path, Body });
put(Res = #response{}) ->
	Res;
put({Path,Body}) when is_list(Path), is_binary(Body) ->
	#response{ body = Object } = gen_server:call(?MODULE, { put, Path, Body }),
	Object.

post(#request{ path = Path, body = Body }) ->
	gen_server:call(?MODULE, { post, Path, Body });
post(Res = #response{}) ->
	Res;
post({Path, Body }) ->
	#response{ body = Object } = gen_server:call(?MODULE, { post, Path, Body }),
	Object.	

delete(#request{ path = Path }) ->
	gen_server:call(?MODULE, { delete, Path });
delete(Res = #response{}) ->
	Res;
delete(Path) when is_list(Path) ->
	#response{ body = Object } = gen_server:call(?MODULE, { delete, Path }),
	Object.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([]) ->
	{ ok, #webpage_rest{}}.

handle_call({ get, Path }, _From, State) ->
	Res = case webpage_path:components(Path)  of
		[ Bucket ] ->
			F = fun() ->
				Objects = mnesia:match_object(#rest{ guid = '_', bucket = Bucket, object = '_' }),
				Guids = [ list_to_binary(Guid) || #rest{ guid = Guid } <- Objects ],
				JSON = json:encode(Guids),
				#response{ status = 200, headers = [ {<<"Content-Length">>, integer_to_binary(byte_size(JSON))}], body = JSON }
			end,
			mnesia:activity(transaction,F);
		[ Bucket, Guid ] ->
			F = fun() ->
				case mnesia:read(rest, Bucket ++ "/" ++ Guid, read) of
					[ #rest{ object = Body } ] ->
						#response{ status = 200, headers = [ {<<"Content-Length">>, integer_to_binary(byte_size(Body))}], body = Body };
					_ ->
						#response{ status = 404 } 
				end
			end,
			mnesia:activity(transaction,F);
		[ Bucket | Filters ] ->
			F = fun() ->
				Objects = mnesia:match_object(#rest{ guid = '_', bucket = Bucket, object = '_' }),
				JSON = json:encode(filter_objects(Objects, list_to_filters(Filters))),
				#response{ status = 200, headers = [{<<"Content-Length">>,integer_to_binary(byte_size(JSON)) }], body = JSON }
			end,
			mnesia:activity(transaction,F);
		_ ->
			#response{ status = 403 }
	end,	
	{ reply, Res, State };

handle_call({ post, Path, Body }, _From, State) ->
	Res = case webpage_path:components(Path) of
		[ Bucket ] -> 
			F = fun() ->
				Guid = uuid:to_string(uuid:v4()),
				ok = mnesia:write(#rest{ guid = Bucket ++ "/" ++ Guid, bucket = Bucket, object = Body }),
				JSON = json:encode(list_to_binary(Guid)),
				#response{
					status = 201, 
					headers = [ 
						{ <<"Location">>, list_to_binary("/" ++ Bucket ++ "/" ++ Guid) }, 
						{ <<"Content-Length">>, integer_to_binary(byte_size(JSON)) }
					], 
					body = JSON }
			end,
			mnesia:activity(transaction,F);
		_ ->
			#response{ status = 403 }
	end,
	{ reply, Res, State };

handle_call({ put, Path, Body }, _From, State) ->
	Res = case webpage_path:components(Path)of
		[ Bucket, Guid ] -> 
			F = fun() ->
				ok = mnesia:write(#rest{ guid = Bucket ++ "/" ++ Guid, bucket = Bucket, object = Body }),
				#response{ status = 204 }
			end,
			mnesia:activity(transaction,F);
		_ -> 
			#response{ status = 403 }
	end,
	{ reply, Res, State };

handle_call({ delete, Path }, _From, State) ->
	Res = case webpage_path:components(Path)of
		[ Bucket, Guid ] -> 
			F = fun() ->
				ok = mnesia:delete(rest, Bucket ++ "/" ++ Guid, write),
				#response{ status = 204 }
			end,
			mnesia:activity(transaction,F);
		[ Bucket | Filters ] ->
		_ -> 
			#response{ status = 403 }
	end,
	{ reply, Res, State };

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call(Message,_From,State) ->
	io:format("[webpage_rest] unknown message ~p~n", [ Message ]),
	{ reply, ok, State }.

handle_cast(Message,State) ->
	io:format("[webpage_rest] unknown message ~p~n", [ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	io:format("[webpage_rest] unknown message ~p~n", [ Message ]),
	{ noreply, State }.

code_change(_Old,_Extra,State) ->
	{ ok, State }.

terminate(_Reason,_State) ->
	ok.

install(Nodes) ->
	rpc:multicall(Nodes,application,start,[ mnesia ]),
	{ atomic, ok } = mnesia:create_table(rest, [
		{ attributes, record_info(fields,rest) },
		{ disc_copies, Nodes }]),
	rpc:multicall(Nodes,application,stop, [ mnesia ]).

filter_objects(Objects,Filters) ->
	lists:filter(fun(O) ->
		lists:foldl(fun ({K,V},B) ->
			B and (V =:= proplists:get_value(K,O))
		end, true, Filters)
	end,[ json:decode(Object) || #rest{ object = Object } <- Objects]).


url_decode([], Acc) ->
	lists:reverse(Acc);
url_decode([ $%, X, Y | T ], Acc) ->
	url_decode( T, [ list_to_integer([X,Y],16) | Acc ]);
url_decode([ $+ | T ], Acc) ->
	url_decode(T, [ 32 | Acc ]); 
url_decode([ X | T ], Acc ) ->
	url_decode(T, [ X | Acc ]).

to_json(Value) ->
	V2 = url_decode(Value,[]),
	json:decode(list_to_binary(V2)).

list_to_filters([],Acc) ->
	Acc;
list_to_filters([ K, V | T ], Acc) ->
	list_to_filters(T, [ { list_to_binary(K),to_json(V) } | Acc ]).

list_to_filters(List) ->
	list_to_filters(List,[]).

