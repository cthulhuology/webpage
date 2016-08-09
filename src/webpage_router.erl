-module(webpage_router).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).

%% router behavior
-export([ start_link/0, stop/0, add/2, remove/1 ]).

%% http method mapping behavior
-export([ get/1, post/1, put/1, delete/1, options/1, head/1, trace/1, connect/1 ]).

%% gen_server behavior
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-include("include/http.hrl").
-record(webpage_router, { paths = []}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

get(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ get, Path, Req}).

post(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ post, Path, Req}).

put(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ put, Path, Req}).

delete(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ delete, Path, Req}).

options(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ options, Path, Req}).

head(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ head, Path, Req}).

trace(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ trace, Path, Req}).

connect(Req = #request{ path = Path }) ->
	gen_server:call(?MODULE,{ connect, Path, Req}).

start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE,stop).

add(Path,Module) ->
	gen_server:cast(?MODULE,{add,Path,Module}).

remove(Path) ->
	gen_server:cast(?MODULE,{remove,Path}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([]) ->
	{ ok, #webpage_router{ paths = [] }}.

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call({get,Path,Req}, _From, State = #webpage_router{ paths = Paths }) ->
	case proplists:get_value(Path,Paths) of
		undefined -> 
			{ reply, #response{ status = 404, body= <<"Not Found">> }, State };
		Module ->
			{ reply, erlang:apply(Module,get,[Req]), State }
	end;

handle_call({post,Path,Req}, _From, State = #webpage_router{ paths = Paths }) ->
	case proplists:get_value(Path,Paths) of
		undefined -> 
			{ reply, #response{ status = 404, body= <<"Not Found">> }, State };
		Module ->
			{ reply, erlang:apply(Module,post,[Req]), State }
	end;

handle_call({put,Path,Req}, _From, State = #webpage_router{ paths = Paths }) ->
	case proplists:get_value(Path,Paths) of
		undefined -> 
			{ reply, #response{ status = 404, body= <<"Not Found">> }, State };
		Module ->
			{ reply, erlang:apply(Module,put,[Req]), State }
	end;

handle_call({delete,Path,Req}, _From, State = #webpage_router{ paths = Paths }) ->
	case proplists:get_value(Path,Paths) of
		undefined -> 
			{ reply, #response{ status = 404, body= <<"Not Found">> }, State };
		Module ->
			{ reply, erlang:apply(Module,delete,[Req]), State }
	end;

handle_call({head,Path,Req}, _From, State = #webpage_router{ paths = Paths }) ->
	case proplists:get_value(Path,Paths) of
		undefined -> 
			{ reply, #response{ status = 404, body= <<"Not Found">> }, State };
		Module ->
			{ reply, erlang:apply(Module,head,[Req]), State }
	end;

handle_call({options,Path,Req}, _From, State = #webpage_router{ paths = Paths }) ->
	case proplists:get_value(Path,Paths) of
		undefined -> 
			{ reply, #response{ status = 404, body= <<"Not Found">> }, State };
		Module ->
			{ reply, erlang:apply(Module,options,[Req]), State }
	end;

handle_call({trace,Path,Req}, _From, State = #webpage_router{ paths = Paths }) ->
	case proplists:get_value(Path,Paths) of
		undefined -> 
			{ reply, #response{ status = 404, body= <<"Not Found">> }, State };
		Module ->
			{ reply, erlang:apply(Module,trace,[Req]), State }
	end;

handle_call({connect,Path,Req}, _From, State = #webpage_router{ paths = Paths }) ->
	case proplists:get_value(Path,Paths) of
		undefined -> 
			{ reply, #response{ status = 404, body= <<"Not Found">> }, State };
		Module ->
			{ reply, erlang:apply(Module,connect,[Req]), State }
	end;

handle_call(Message,_From,State) ->
	io:format("[webpage_router] unknown message ~p~n", [ Message ]),
	{ reply, #response{ status = 405 }, State }.


handle_cast({ add, Path, Module }, State = #webpage_router{ paths = Paths }) ->
	{ noreply, State#webpage_router{ paths = [ { Path, Module } | proplists:delete(Path,Paths) ]}};	

handle_cast({ remove, Path }, State = #webpage_router{ paths = Paths }) ->
	{ noreply, State#webpage_router{ paths = proplists:delete(Path,Paths) }};	

handle_cast(Message,State) ->
	io:format("[webpage_router] unknown message ~p~n", [ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	io:format("[webpage_router] unknown message ~p~n", [ Message ]),
	{ noreply, State }.

code_change(_Old,_Extra,State) ->
	{ ok, State }.

terminate(_Reason,_State) ->
	ok.
