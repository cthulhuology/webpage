-module(webpage_path).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).

-export([ match/2  ]).

match(Path,Pattern) ->
	A = string:tokens(Path,"/"),
	B = string:tokens(Pattern,"/"),
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
		
