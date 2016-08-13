-module(webpage_auth).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-export([ add/3, remove/2, install/1, grant/2, revoke/2]).

-include("include/http.hrl").

-record(user_auth, { token, user, email, active, paths = [] }).

-export([ auth/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

auth(Request = #request{ path = Path, headers = Headers }) ->
	io:format("auth headers are ~p~n",[ Headers ]),
	case proplists:get_value(<<"Authorization">>, Headers ) of
		undefined ->
			io:format("challenging~n"),
			#response{ status = 401, headers = [{  <<"WWW-Authenticate">>,<<"Basic realm=\"webpage\"">> }, {<<"Content-Length">>, <<"0">> }]};
		Authorization ->
			F = fun() ->
				{ ok, Salt } = application:get_env(webpage,salt),
				[ <<"Basic">>,Auth ] = binary:split(Authorization,<<" ">>),
				Token = crypto:hmac(sha256,Salt,Auth),
				case mnesia:read(user_auth,Token) of
					[ #user_auth{ user = User, email = Email, active = true, paths = Paths } ] ->
						case lists:foldl(fun(Pattern,Match) -> 
							webpage_path:match(Path,Pattern) or Match end, false, Paths) of
							true ->
								Request#request{ headers = [ { <<"User">>, User},{<<"Email">>,Email } | Headers ] };
							false ->
								#response{ status = 401, headers = [{  <<"WWW-Authenticate">>,<<"Basic realm=\"webpage\"">> }, {<<"Content-Length">>, <<"0">> }]}
						end;
					_ ->
						#response{ status = 401, headers = [{  <<"WWW-Authenticate">>,<<"Basic realm=\"webpage\"">> }, {<<"Content-Length">>, <<"0">> }]}
				end
			end,
			mnesia:activity(transaction,F)		
	end.		


add(User,Email,Password) ->
	{ ok, Salt } = application:get_env(webpage,salt),
	Auth = base64:encode(<< User/binary,":",Password/binary>>),
	Token = crypto:hmac(sha256,Salt,Auth),
	F = fun() ->
		remove(User,Email),
		mnesia:write(#user_auth{ token = Token, user = User, email = Email, active = true, paths = [] })
	end,
	mnesia:activity(transaction,F).

remove(User,Email) ->
	F = fun() ->
		case mnesia:match_object(#user_auth{ user = User, email = Email, token = '_', active = '_', paths = '_' }) of
			[] -> ok;
			Records ->
				[ mnesia:delete_object(Record) || Record <- Records ]
		end
	end,
	mnesia:activity(transaction,F).

grant(User,Pattern) ->
	F = fun() ->
		case mnesia:match_object(#user_auth{ user = User, email = '_', token = '_', active = true, paths = '_' }) of
			[] -> ok;
			[ Auth = #user_auth{ paths = Paths } ] ->
				mnesia:write(Auth#user_auth{ paths = [ Pattern | Paths ]})
		end
	end,
	mnesia:activity(transaction,F).

revoke(User,Pattern) ->
	F = fun() ->
		case mnesia:match_object(#user_auth{ user = User, email = '_', token = '_', active = true, paths = '_' }) of
			[] -> ok;
			[ Auth = #user_auth{ paths = Paths }] ->
				mnesia:write(Auth#user_auth{ paths = lists:delete(Pattern,Paths) })
		end
	end,
	mnesia:activity(transaction,F).

install(Nodes) ->
	Database = code:priv_dir(webpage),
	application:set_env(mnesia,dir, Database),
	rpc:multicall(Nodes,application,start,[ mnesia ]),
	{ atomic, ok } = mnesia:create_table(user_auth, [
		{ attributes, record_info(fields,user_auth) },
		{ disc_copies, Nodes }]),
	rpc:multicall(Nodes,application,stop, [ mnesia ]).
