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
			case ssl:ssl_accept(Socket,1000) of
				ok ->
					{ PeerIP, PeerPort } = inet:peername(Socket),
					error_logger:info_msg("Socket connection from ~p:~p", [ PeerIP, PeerPort ]),
					{ ok, Webpage#webpage{
						socket = Socket,
						request = #request{ socket = Socket }
					}};
				{ error, Reason } ->
					error_logger:error_msg("SSL connection failed: ~p", [ Reason ]),
					{ stop, Reason }
			end;
		{ error, Reason } ->
			error_logger:error_msg("Socket accept failed: ~p", [ Reason ]),
			{ stop, Reason }
	end.
	
handle_call(Message,_From,Webpage) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ reply, ok, Webpage }.


handle_cast(Response = #response{},Webpage = #webpage{ socket = Socket }) ->
	Bin = http:response(Response),
	ssl:send(Socket,Bin),
	{ noreply, Webpage };

handle_cast(Message,Webpage) ->
	error_loggger:error_msg("Unknown message: ~p", [ Message ]),
	{ noreply, Webpage }.

handle_info({ ssl, Socket, Data }, Webpage = #webpage{ request = Req, module = Module }) ->
	Request = http:request(Req,Data),
	case Request#request.stage of
		done ->
			Response = erlang:apply(Module,Request#request.method,[ Request ]),
			Bin = http:response(Response),
			ssl:send(Socket,Bin),
			{ noreply, Webpage#webpage{ request = #request{ socket = Socket } }};
		_ ->
			{ noreply, Webpage#webpage{ request = Request }}
	end;

handle_info({ ssl_closed, _Socket}, Webpage = #webpage{}) ->
	error_logger:info_msg("connection closed"),
	{ stop, normal, Webpage };

handle_info(Message,Webpage) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ noreply, Webpage }.

terminate(_Reason,#webpage{ socket = Socket }) ->
	ssl:close(Socket),
	ok.

code_change( _Old, Webpage, _Extra) ->
	{ ok, Webpage }.

