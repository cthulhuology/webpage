-module(webpage_websocket).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2012,2013 David J. Goehrig"/utf8>>).
-behavior(gen_server).

-export([ start_link/3, request/1, send/2, socket/1, headers/1, 
	path/1, stop/1, bind/3  ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-include("../include/http.hrl").

-record(websocket, { socket, path, headers, module, function, data, state }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%

%% Starts a websocket by accepting a connection form Listen port
start_link(Socket,Request = #request{},Module) ->
	gen_server:start_link(?MODULE, { Socket, Request, Module }, []).

%% Returns the raw TCP socket of the WebSocket
socket(WebSocket) ->
	gen_server:call(WebSocket,socket).

%% Returns the HTTP headers of the WebSocket
headers(WebSocket) ->
	gen_server:call(WebSocket,headers).

%% Returns the path of the WebSocket
path(WebSocket) ->
	gen_server:call(WebSocket,path).

%% Sends data to the websocket
send(WebSocket,Data) ->
	gen_server:cast(WebSocket,{ send, Data }).

%% Stops the WebSocket
stop(WebSocket) ->
	gen_server:cast(WebSocket,stop).

%% Binds a Module and Function to be called when a message is encountered
bind(WebSocket,Module,Function) ->
	gen_server:cast(WebSocket, { bind, Module, Function }).

%% returns true if the request is a websocket request
request(#request{ headers = Headers }) ->
	io:format("got headers ~p~n", [ Headers ]),
	case proplists:get_value(<<"Sec-WebSocket-Version">>,Headers) of
		<<"13">> -> 
			true;
		Any ->
			io:format("Protocol not supported ~p", [ Any ]),
			false
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server methods
init({ Socket, #request{ headers = Headers, path = Path, body = Data }, Module }) ->
	ok = ssl:controlling_process(Socket,self()),
	Handshake = handshake(Headers),
	case ssl:send(Socket,Handshake) of
		{ error, Reason } ->	
			io:format("Handshake failed ~p~n", [ Reason ]),
			{ stop, handshake_failed };
		ok -> 
			spawn(Module,recv,[self(),Path,connected]),
			{ ok, #websocket{ 
				socket = Socket, 
				headers = Headers, 
				path = Path, 
				module = Module,
				function = recv,
				state = { wait, Data, [] }}}
	end.
	
handle_call( socket, _From, WebSocket = #websocket{ socket = Socket }) ->
	{ reply, Socket, WebSocket };

handle_call( headers, _From, WebSocket = #websocket{ headers = Headers }) ->
	{ reply, Headers, WebSocket };

handle_call( path, _From, WebSocket = #websocket{ path = Path }) ->
	{ reply, Path, WebSocket }.

handle_cast({ bind, Module, Function }, WebSocket) ->
	{ noreply, WebSocket#websocket{ module = Module, function = Function }};

handle_cast( stop, WebSocket ) ->
	{ stop, normal, WebSocket };

handle_cast({ message, Data }, WebSocket = #websocket{ module = Module, function = Function, path = Path }) ->	
	spawn(Module,Function,[ self(), Path, Data ] ),
	{ noreply, WebSocket#websocket{ data = [] }};

handle_cast({ send, Data }, WebSocket = #websocket { socket = Socket }) ->
	ok = ssl:send(Socket,frame(Data)),
	{ noreply, WebSocket };

handle_cast(ping, WebSocket = #websocket{ socket = Socket }) -> 
	ssl:send(Socket,frame(<<"pong">>,10)),	%% send pong
	{ noreply, WebSocket };

handle_cast(pong,WebSocket) ->
	{ noreply, WebSocket };

handle_cast({ unknown, Any }, WebSocket) ->
	io:format("Unknown message ~p~n", [ Any ]),
	{ stop, unknown_message, WebSocket };

handle_cast(close, WebSocket) ->
	{ stop, normal, WebSocket }.

handle_info({ssl, _Socket, NewData}, WebSocket = #websocket{ state = { wait, Data, Payloads } }) ->
	State = unframe({ parse, <<Data/binary,NewData/binary>>, Payloads }),
	{ noreply, WebSocket#websocket{ state = State } };

handle_info({ssl, _Socket, NewData}, WebSocket = #websocket{ state = { Stage, F, Opcode, Mask, Length, Data, Payloads }}) ->
	State = unframe({ Stage, F, Opcode, Mask, Length, <<Data/binary,NewData/binary>>, Payloads }),
	{ noreply, WebSocket#websocket{ state = State } };

handle_info({ssl_closed, _Socket }, WebSocket) ->
	{ stop, normal, WebSocket };

handle_info(Message, WebSocket = #websocket{ socket = Socket}) ->
	ok = ssl:send(Socket,frame(Message)),
	{ noreply, WebSocket }.

terminate( normal, #websocket{ socket = Socket, module = Module, function = Function, path = Path }) ->
	spawn(Module,Function,[self(), Path, closed]),
	ssl:close(Socket),
	ok;

terminate( _Reason, #websocket{ socket = Socket, module = Module, function = Function, path = Path }) ->
	spawn(Module,Function,[self(), Path, closed]),
	ssl:close(Socket),
	ok.

code_change( _Old, WebSocket, _Extra ) ->
	{ ok, WebSocket }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods

frame(Data) when is_binary(Data) ->
	frame(Data,1).	%% 1 == text
	
frame(Data,Opcode) when is_binary(Data) ->
	frame(Data,Opcode, false ).		%% no masking

frame(Data,Opcode,Masked) when is_binary(Data) ->
	Len = iolist_size(Data),
	case Masked of	
		true -> 
			Mask = crypto:rand_bytes(4),
			framed(mask(Data,Mask), Opcode, Mask, Len);
		_ -> 
			framed(Data, Opcode, Len)
	end.

%% Calcuate an rfc6455 handshake
handshake(Headers) ->
	Key = proplists:get_value(<<"Sec-WebSocket-Key">>,Headers),
	Shake = <<Key/binary,"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>, %% 25.. is magic
	Crypt = crypto:hash(sha,Shake),
	Secret = base64:encode(Crypt),
	http:response(#response{
		status = 101,
		headers = [
			{ <<"Upgrade">>, <<"websocket">> },
			{ <<"Connection">>, <<"Upgrade">> },
			{ <<"Sec-WebSocket-Accept">>, Secret }]
		}).


%% Frame a Datagram with the appropriate Opcode, Length, and Mask

framed(Data,Opcode,Len) when Len < 126 ->
	Res = <<1:1,0:1,0:1,0:1,Opcode:4,0:1,Len:7,Data/binary>>,
	Res;
framed(Data,Opcode,Len) when Len < 65536 ->
	Res = <<1:1,0:1,0:1,0:1,Opcode:4,0:1,126:7,Len:16,Data/binary>>,
	Res;
framed(Data,Opcode,Len) ->
	Res = <<1:1,0:1,0:1,0:1,Opcode:4,0:1,127:7,Len:64,Data/binary>>,
	Res.

framed(Data,Opcode,Mask,Len) when Len < 126 ->
	Res = <<1:1,0:1,0:1,0:1,Opcode:4,1:1,Len:7,Mask:4/binary,Data/binary>>,
	Res;
framed(Data,Opcode,Mask,Len) when Len < 65536 ->
	Res = <<1:1,0:1,0:1,0:1,Opcode:4,1:1,126:7,Len:16,Mask:4/binary,Data/binary>>,
	Res;
framed(Data,Opcode,Mask,Len) ->
	Res = <<1:1,0:1,0:1,0:1,Opcode:4,1:1,127:7,Len:64,Mask:4/binary,Data/binary>>,
	Res.

%%  Mask the data as a client
mask(<<>>,_Mask,_I,Acc) ->
	binary:list_to_bin(lists:reverse(Acc));
mask(<<D:8,Data/binary>>,<<M1:8,M2:8,M3:8,M4:8>> = Mask, I, Acc) ->
	C = case I rem 4 of 
		0 -> D bxor M1;
		1 -> D bxor M2;
		2 -> D bxor M3;
		3 -> D bxor M4
	end,
	mask(Data,Mask,I+1, [ C | Acc]).

mask(Data,Mask) ->
	mask(Data,Mask,0,[]).

%% remove a mask form the data from the client
unmask(<<>>,_Mask,_I,Acc) ->
	lists:reverse(Acc);
unmask(<<D:8,Data/binary>>,<<M1:8,M2:8,M3:8,M4:8>> = Mask,I,Acc) ->
	C = case I rem 4 of
		0 -> [ D bxor M1 | Acc ];
		1 -> [ D bxor M2 | Acc ];
		2 -> [ D bxor M3 | Acc ];
		3 -> [ D bxor M4 | Acc ]
	end,
	unmask(Data,Mask,I+1,C).

unmask(Data,<<"">>) when is_binary(Data) ->
	Data;
unmask(Data,Mask) when is_binary(Data) ->
	unmask(Data,Mask,0,[]).


% wait for data
unframe({ parse, <<>>, Payloads}) ->
	{ wait, <<>>, Payloads};

% mask bit, and extract Finished bit, Opcode, and PayLen
unframe({ parse, <<F:1,_R1:1,_R2:1,_R3:1,Opcode:4,1:1,PayLen:7,Data/binary>>, Payloads }) ->
	unframe({ length, F, Opcode, masked, PayLen, Data, Payloads });

% unmaked bit and extract Finished bit, Opcode, and PayLen
unframe({ parse, <<F:1,_R1:1,_R2:1,_R3:1,Opcode:4,0:1,PayLen:7,Data/binary>>, Payloads }) ->
	unframe({ length, F, Opcode, unmasked, PayLen, Data, Payloads});

% wait for more data, because we can't parse a frame
unframe({ parse, Data,Payloads}) ->
	io:format("unframe error ~p~n", [ Data ]),
	{ wait, Data , Payloads };

% 16bit length for masked
unframe({ length, F, Opcode, masked, 126, <<Length:16,Remainder/binary>> = Data, Payloads }) when byte_size(Data) > 1 ->
	unframe({ unmask, F, Opcode, masked, Length, Remainder, Payloads });

% 64bit length for masked
unframe({ length, F, Opcode, masked, 127, <<Length:64,Remainder/binary>> = Data, Payloads }) when byte_size(Data) > 3 ->
	unframe({ unmask, F, Opcode, Length, Remainder, Payloads });

% 7bit length for masked
unframe({ length, F, Opcode, masked, Length, Data, Payloads }) ->
	unframe({ unmask, F, Opcode, masked, Length, Data, Payloads});

% 16bit length for unmasked
unframe({ length, F, Opcode, unmasked, 126, <<Length:16,Remainder/binary>> = Data, Payloads }) when byte_size(Data) > 1 ->
	unframe({ unpack, F, Opcode, unmasked, Length, Remainder, Payloads});

% 64bit length for unmasked
unframe({ length, F, Opcode, unmasked, 127, <<Length:64,Remainder/binary>> = Data, Payloads}) when byte_size(Data) > 3 ->
	unframe({ unpack, F, Opcode, unmasked, Length, Remainder, Payloads});

% 7bit length for unmasked
unframe({ length, F, Opcode, unmasked, Length, Data , Payloads}) ->
	unframe({ unpack, F, Opcode, unmasked, Length, Data, Payloads});

% extract 32bit mask
unframe({ unmask, F, Opcode, masked, Length, <<Mask:4/binary,Remainder/binary>> = Data, Payloads}) when byte_size(Data) > 3  ->
	unframe({ unpack, F, Opcode, Mask, Length, Remainder, Payloads });

% payload for unmasked
unframe({ unpack, F, Opcode, unmasked, Length, Data,Payloads}) when byte_size(Data) >= Length ->
	<<Payload:Length/binary,Remainder/binary>> = Data,
	unframe({ deliver, F, Opcode, unmasked, Length, Remainder, [ Payload | Payloads]});

% payload for masked,and decode
unframe({ unpack, F, Opcode, Mask, Length, Data, Payloads }) when byte_size(Data) >= Length ->
	<<Payload:Length/binary,Remainder/binary>> = Data,
	unframe({ deliver, F, Opcode, Mask, Length, Remainder, [ unmask(Payload,Mask) | Payloads ]});

% continutation frame
unframe({ deliver, 0, _Opcode, _Mask, _Length, Data, Payloads }) ->
	unframe({ parse, Data, Payloads });

% finished frame
unframe({ deliver, 1, Opcode, _Mask, _Length, Data, Payloads }) ->
	dispatch(Opcode,Payloads),
	unframe({ parse, Data,[]});

%% wait for new data, so we can advance to the next stage
unframe(State = { _Stage, _F, _Opcode, _Mask, _Length, _Data, _Payloads}) ->
	State.

%% handle protocol logic like ping/pong etc.	
dispatch(Opcode, Data ) ->
	Payload = iolist_to_binary(lists:reverse(Data)),
	case Opcode of
		0 -> gen_server:cast(self(),{ message, Payload });	%% continuation
		1 -> gen_server:cast(self(),{ message, Payload });	%% text
		2 -> gen_server:cast(self(),{ message, Payload });	%% binary
		8 -> gen_server:cast(self(),close);		 	%% close
		9 -> gen_server:cast(self(),ping);			%% ping
		10 -> gen_server:cast(self(),pong);			%% pong
		Any -> gen_server:cast(self(),{ unknown, Any })		%% unknown
	end.
