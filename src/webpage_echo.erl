-module(webpage_echo).

-export([ recv/3 ]).

recv(Pid,Path,connected) ->
	io:format("From ~p at ~p connected~n", [ Pid, Path ]);
recv(Pid,Path,closed) ->
	io:format("From ~p at ~p closed~n", [ Pid, Path ]);
recv(Pid,Path,Data) ->
	io:format("From ~p at ~p got ~p~n", [ Pid, Path, Data ]),
	Pid ! Data.

