-module(webpage).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start/0, start_link/2, stop/1, server/2 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-include("include/http.hrl").
-record(webpage, { socket, module, request = #request{} }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

%% create a webpage_server under supervision for the port
server(Module,Port) ->
	webpage_sup:server(Module,Port).


start() ->
	application:ensure_all_started(webpage).

start_link(Socket,Module) ->
	gen_server:start_link(?MODULE, #webpage{ 
		socket = Socket,
		module = Module,
		request = #request{}
	},[]).

stop(Webpage) ->
	gen_server:cast(Webpage,stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init(Webpage = #webpage{ socket = Listen }) ->
	case ssl:transport_accept(Listen) of
		{ ok, Socket } ->
			case ssl:ssl_accept(Socket,5000) of
				ok ->
					{ ok, Webpage#webpage{
						socket = Socket,
						request = #request{ socket = Socket }
					}};
				{ error, Reason } ->
					io:format("failed to make ssl connetion ~p~n", [ Reason ]),
					{ stop, Reason }
			end;
		{ error, Reason } ->
			io:format("failed to accept socket ~p~n", [ Reason ]),
			{ stop, Reason }
	end.
	
handle_call(Message,_From,Webpage) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ reply, ok, Webpage }.


handle_cast(Response = #response{},Webpage = #webpage{ socket = Socket }) ->
	Bin = http:response(Response),
	io:format("sending ~p~n", [ Bin ]),
	ssl:send(Socket,Bin),
	{ noreply, Webpage };

handle_cast(Message,Webpage) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ noreply, Webpage }.

handle_info({ ssl, Socket, Data }, Webpage = #webpage{ request = Req, module = Module }) ->
	Request = http:request(Req,Data),
	case Request#request.stage of
		done ->
			Response = erlang:apply(Module,Request#request.method,[ Request ]),
			Bin = http:response(Response),
			io:format("sending ~p~n", [ Bin ]),
			ssl:send(Socket,Bin),
			{ noreply, Webpage#webpage{ request = #request{ socket = Socket } }};
		_ ->
			{ noreply, Webpage#webpage{ request = Request }}
	end;

handle_info({ ssl_closed, _Socket}, Webpage = #webpage{}) ->
	io:format("connection closed~n"),
	{ stop, normal, Webpage };

handle_info(Message,Webpage) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ noreply, Webpage }.

terminate(_Reason,#webpage{ socket = Socket }) ->
	ssl:close(Socket),
	ok.

code_change( _Old, Webpage, _Extra) ->
	{ ok, Webpage }.

