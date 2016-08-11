-module(webpage_auth).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-export([ get/1 ]).

-include("include/http.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

get(Request = #request{ path = Path }) ->
	io:format("webpage_auth for ~p~n", [ Path ]),
	Request.
