-module(webpage_file).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-export([ get/1, add/3, remove/1, install/1]).

-include("include/http.hrl").

-record(webpage_file, { path, filename, mime, time }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%
get(Path) when is_list(Path) ->
	get(#request{ path = Path });
get(#request{ path = Path }) ->
	F = fun() ->
		case mnesia:read(webpage_file,Path) of
			[ #webpage_file{ filename = Filename, mime = Mime } ] ->
				case file:read_file(Filename) of
					{ ok, Body }  ->
						#response{ status = 200, headers = [
							{ <<"Content-Length">>, integer_to_binary(byte_size(Body)) },
							{ <<"Content-Type">>, Mime }],
							body = Body };
					_ -> 
						#response{ status = 404, body = <<"File not Found">> }
				end;	
			_ ->
				#response{ status = 404, body = <<"File not Found">> }
		end
	end,
	mnesia:activity(transaction,F).

install(Nodes) ->
	{ atomic, ok } = mnesia:create_table(webpage_file, [
		{ attributes, record_info(fields,webpage_file) },
		{ disc_copies, Nodes }]).

add(Path,Filename,Mime) ->
	F = fun() ->
		mnesia:delete(webpage_file,Path,write),
		mnesia:write(#webpage_file{ path = Path, filename = Filename, mime = Mime, time = erlang:system_time() }),
		error_logger:info_msg("File ~p to ~p", [Path, Filename])
	end,
	ok = mnesia:activity(transaction,F).

remove(Path) ->
	F = fun() ->
		ok = mnesia:delete(webpage_file,Path,write),
		error_logger:info_msg("File removed ~p", [Path])
	end,
	mnesia:activity(transaction,F).
