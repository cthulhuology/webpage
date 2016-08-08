-module(webpage_logger).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).


-include("../../http/include/http.hrl").
-record(webpage_logger, {}).
-export([ get/1, post/1, put/1, delete/1, options/1, head/1, trace/1, connect/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

resp(Req) -> 
	Path = list_to_binary(Req#request.path),
	Length = integer_to_binary(6 + byte_size(Path)),
	#response{ 
		status = 200, 
		headers = [
			{ <<"Content-Length">>, Length },
			{ <<"Mime-Type">>, <<"text/html">> } ],
		body = <<"hello ", Path/binary>> 
	}.
	

get(Req) ->
	http:dump(Req),
	resp(Req).

post(Req) ->
	http:dump(Req),
	resp(Req).

put(Req) ->
	http:dump(Req),
	resp(Req).

delete(Req) ->
	http:dump(Req),
	resp(Req).

options(Req) ->
	http:dump(Req),
	resp(Req).

head(Req) ->
	http:dump(Req),
	resp(Req).

trace(Req) ->
	http:dump(Req),
	resp(Req).

connect(Req) ->
	http:dump(Req),
	resp(Req).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

