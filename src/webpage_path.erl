-module(webpage_path).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).

-export([ match/2, scan/2, components/1, order/1 ]).

components(Path) ->
	string:tokens(Path,"/").

match(Path,Pattern) ->
	A = components(Path), 
	B = components(Pattern),
	compare(A,B).

%% a simple wildcard * match compare
compare([],[]) -> true;
compare([_HA|_TA],[]) -> false;
compare([],["*"|_TB]) -> true;
compare([],[_HB|_TB]) -> false;
compare([_HA|_TA],["*"|_TB]) -> true;
compare([HA|TA],[HB|TB]) ->
	case HA =:= HB of
		true -> compare(TA,TB);
		false -> false
	end.
		
%% searches a proplist for a path match
scan(_Path, []) ->
	none;
scan(Path, [ { K, V} | T ]) ->
	case match(Path,K) of
		true -> V;
		false -> scan(Path,T)
	end.

order([], Acc) ->
	lists:reverse(Acc);
order([ { K,V } | T ],Acc) ->
	order(T, [ K | Acc ]).
order(List) ->
	order(List,[]).
