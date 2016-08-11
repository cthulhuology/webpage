-module(webpage_file).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-export([ get/1, add/3, remove/1, install/1]).

-include("include/http.hrl").

-record(file_paths, { path, filename, mime }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

get(#request{ path = Path }) ->
	F = fun() ->
		case mnesia:read(file_paths,Path) of
			[ #file_paths{ filename = Filename, mime = Mime } ] ->
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
	application:set_env(mnesia,dir, code:priv_dir(webpage)),
	rpc:multicall(Nodes,application,start,[ mnesia ]),
	{ atomic, ok } = mnesia:create_table(file_paths, [
		{ attributes, record_info(fields,file_paths) },
		{ disc_copies, Nodes }]),
	rpc:multicall(Nodes,application,stop, [ mnesia ]).

add(Path,Filename,Mime) ->
	F = fun() ->
		mnesia:delete(file_paths,Path,write),
		mnesia:write(#file_paths{ path = Path, filename = Filename, mime = Mime })
	end,
	ok = mnesia:activity(transaction,F),
	webpage_router:add("/",webpage_file).

remove(Path) ->
	F = fun() ->
		mnesia:delete(file_paths,Path,write)
	end,
	mnesia:activity(transaction,F).
