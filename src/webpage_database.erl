-module(webpage_database).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ initialize/0, start_link/0, stop/0 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-record(webpage_database, { nodes, tables }).


start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE,stop).

%% Initializes a blank db on all of the configured nodes
initialize() ->
	{ ok, Nodes } = application:get_env(webpage,nodes),
	{ ok, Tables } =  application:get_env(webpage,tables),
	rpc:multicall([ node() | Nodes ], mnesia,stop, []),
	mnesia:delete_schema([ node() | Nodes ]),
	mnesia:create_schema([ node() | Nodes ]),
	rpc:multicall([ node() |  Nodes ], mnesia ,start, []),
	[ Table:install([ node() | Nodes ]) || Table <- Tables ],
	rpc:multicall([ node() | Nodes ] , mnesia,stop, []),
	ok.

%% ensures all database tables are running before we use it
init([]) ->
	{ ok, Nodes } = application:get_env(webpage,nodes),
	{ ok, Tables } =  application:get_env(webpage,tables),
	{ ok, Timeout } = application:get_env(webpage,table_timeout),
	rpc:multicall([ node() | Nodes ] ,mnesia,start,[]),
	case mnesia:wait_for_tables(Tables,Timeout) of
		ok ->
			error_logger:info_msg("Database started on ~p", [ Nodes ]),
			{ ok, #webpage_database{ nodes = Nodes, tables = Tables }};
		{ timeout, Remaining } ->
			error_logger:error_msg("Database failed to load ~p", [ Remaining ]),		
			{ stop, timeout }
	end.

handle_call(stop,_From,State)  ->
	{ stop, stopped, State };
handle_call(Message,_From,State) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ reply, ok, State }.
	
handle_cast(Message,State) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ noreply, State }.
	
handle_info(Message,State) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ noreply, State }.

code_change(_Old,_Extra,State) ->
	{ ok, State }.


terminate(Reason,#webpage_database{ nodes = Nodes } = State) ->
	error_logger:info_msg("Terminating ~p with ~p", [ Reason, State ]),
	rpc:multicall(Nodes,application,stop, [ mnesia ]),
	ok.

