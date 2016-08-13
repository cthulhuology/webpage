-module(webpage_echo).

-export([ echo/3 ]).

echo(Pid,Path,connected) ->
	io:format("From ~p at ~p connected~n", [ Pid, Path ]);
echo(Pid,Path,closed) ->
	io:format("From ~p at ~p closed~n", [ Pid, Path ]);
echo(Pid,Path,Data) ->
	io:format("From ~p at ~p got ~p~n", [ Pid, Path, Data ]),
	Pid ! Data.

