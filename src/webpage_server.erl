-module(webpage_server).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/2, stop/1, accept/1 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-define(SELF, list_to_atom(?MODULE_STRING ++ "_" ++ integer_to_list(Port))).
-record(webpage_server, { module, port, socket }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link(Module,Port) ->
	gen_server:start_link({ local, ?SELF }, ?MODULE, #webpage_server{
		module = Module, port = Port
	}, []).

stop(Port) ->
	gen_server:call(?SELF,stop).

accept(Port) ->
	gen_server:cast(?SELF,accept).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init(Server = #webpage_server{ port = Port }) ->
	CACert = code:priv_dir(webpage) ++ "/cacert.pem",
	Cert = code:priv_dir(webpage) ++ "/cert.pem",
	Key = code:priv_dir(webpage) ++ "/key.pem",
	case ssl:listen(Port,[
		binary, 
		{packet,0},
		{certfile, Cert}, 
		{keyfile, Key},
		{cacertfile, CACert},
		{reuseaddr, true},
		{verify, verify_none}, 
		{fail_if_no_peer_cert, false}
	%	{versions,['tlsv1.2']},
	%	{ciphers,[{rsa,aes_128_cbc,sha}]}
	]) of
		{ ok, Socket } ->
			accept(Port),
			{ ok, Server#webpage_server{ socket = Socket }};
		{ error, Reason } ->
			error_logger:error_msg("Socket listen failed on ~p because: ~p", [ Port, Reason ]),
			{ stop, Reason }
	end.

handle_call(stop,_From,Server) ->
	{ stop, stopped, Server };

handle_call(Message,_From,Server) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ reply, ok, Server }.

handle_cast(accept,Server = #webpage_server{ module = Module, socket = Socket, port = Port }) ->
	webpage:start_link(Socket,Module),
	accept(Port),
	{ noreply, Server };

handle_cast(Message,Server) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ noreply, Server }.

handle_info(Message,Server) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ noreply, Server }.

code_change(_Old,_Extra,Server) ->
	{ ok, Server }.

terminate(_Reason,_Server) ->
	ok.
