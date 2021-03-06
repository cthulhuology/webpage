-module(webpage_rest).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/0, stop/0, install/1, get/1, put/1, post/1, delete/1 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).
-export([ filter_objects/2]).

-include("include/http.hrl").

-record(webpage_rest, { guid, bucket, object  }).

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
	{ ok, {}}.

handle_call({ get, Path }, _From, State) ->
	Res = case webpage_path:components(Path)  of
		[ Bucket ] ->
			io:format("searching bucket ~p~n", [ Bucket ]),
			F = fun() ->
				Objects = mnesia:match_object(#webpage_rest{ guid = '_', bucket = Bucket, object = '_' }),
				Guids = [ list_to_binary(Guid) || #webpage_rest{ guid = Guid } <- Objects ],
				JSON = json:encode(Guids),
				#response{ status = 200, headers = [ {<<"Content-Length">>, integer_to_binary(byte_size(JSON))}], body = JSON }
			end,
			mnesia:activity(transaction,F);
		[ Bucket, Guid ] ->
			io:format("searching file ~p/~p~n", [ Bucket, Guid ]),
			F = fun() ->
				case mnesia:read(webpage_rest, Bucket ++ "/" ++ Guid, read) of
					[ #webpage_rest{ object = Body } ] ->
						#response{ status = 200, headers = [ {<<"Content-Length">>, integer_to_binary(byte_size(Body))}], body = Body };
					_ ->
						#response{ status = 404 } 
				end
			end,
			mnesia:activity(transaction,F);
		[ Bucket | Filters ] ->
			io:format("searching bucket ~p with ~p", [ Bucket, Filters ]),
			F = fun() ->
				Objects = mnesia:match_object(#webpage_rest{ guid = '_', bucket = Bucket, object = '_' }),
				Matching = filter_objects(Objects, list_to_filters(Filters)),
				JSON = json:encode([ json:decode(O) || #webpage_rest{ object = O } <- Matching ]),
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
			io:format("Adding document to ~p with content ~p~n", [ Bucket, Body ] ),
			F = fun() ->
				Guid = uuid:to_string(uuid:v4()),
				io:format("Generating ~p~n", [ Guid ]),
				ok = mnesia:write(#webpage_rest{ guid = Bucket ++ "/" ++ Guid, bucket = Bucket, object = Body }),
				JSON = json:encode(list_to_binary(Guid)),
				#response{
					status = 201, 
					headers = [ 
						{ <<"Location">>, list_to_binary("/" ++ Bucket ++ "/" ++ Guid) }, 
						{ <<"Content-Length">>, integer_to_binary(byte_size(JSON)) }
					], 
					body = JSON }
			end,
			mnesia:activity(sync_transaction,F);
		_ ->
			#response{ status = 403 }
	end,
	{ reply, Res, State };

handle_call({ put, Path, Body }, _From, State) ->
	Res = case webpage_path:components(Path)of
		[ Bucket, Guid ] -> 
			F = fun() ->
				ok = mnesia:write(#webpage_rest{ guid = Bucket ++ "/" ++ Guid, bucket = Bucket, object = Body }),
				#response{ status = 204 }
			end,
			mnesia:activity(sync_transaction,F);
		_ -> 
			#response{ status = 403 }
	end,
	{ reply, Res, State };

handle_call({ delete, Path }, _From, State) ->
	Res = case webpage_path:components(Path)of
		[ Bucket, Guid ] -> 
			F = fun() ->
				ok = mnesia:delete(webpage_rest, Bucket ++ "/" ++ Guid, write),
				#response{ status = 204 }
			end,
			mnesia:activity(sync_transaction,F);
		[ Bucket | Filters ] ->
			F = fun() ->
				Objects = mnesia:match_object(#webpage_rest{ guid = '_', bucket = Bucket, object = '_' }),
				Matching = filter_objects(Objects, list_to_filters(Filters)),
				[ mnesia:delete(webpage_rest, Guid, write) || #webpage_rest{ guid = Guid } <- Matching ],
				#response{ status = 204 }
			end,
			mnesia:activity(sync_transaction,F);
		_ -> 
			#response{ status = 403 }
	end,
	{ reply, Res, State };

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call(Message,_From,State) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ reply, ok, State }.

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

install(Nodes) ->
	{ atomic, ok } = mnesia:create_table(webpage_rest, [
		{ attributes, record_info(fields,webpage_rest) },
		{ disc_copies, Nodes }]).

filter_objects(Objects,Filters) ->
	lists:filter(fun(#webpage_rest{ object = Object }) ->
		O = json:decode(Object),	
		lists:foldl(fun ({K,V},B) ->
			B and (V =:= proplists:get_value(K,O))
		end, true, Filters)
	end, Objects).

to_json(Value) ->
	V2 = url:decode(Value),
	json:decode(list_to_binary(V2)).

list_to_filters([],Acc) ->
	Acc;
list_to_filters([ K, V | T ], Acc) ->
	list_to_filters(T, [ { list_to_binary(K),to_json(V) } | Acc ]).

list_to_filters(List) ->
	list_to_filters(List,[]).

