-module(webpage_auth).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-export([ add/3, remove/2, install/1, grant/2, revoke/2, users/0, token/1]).

-include("include/http.hrl").

-record(webpage_auth, { token, user, email, active, paths = [] }).

-export([ auth/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

auth(Request = #request{ path = Path, headers = Headers }) ->
	case proplists:get_value(<<"Authorization">>, Headers ) of
		undefined ->
			#response{ status = 401, headers = [{  <<"WWW-Authenticate">>,<<"Basic realm=\"webpage\"">> }, {<<"Content-Length">>, <<"0">> }]};
		Authorization ->
			F = fun() ->
				{ ok, Salt } = application:get_env(webpage,salt),
				[ <<"Basic">>,Auth ] = binary:split(Authorization,<<" ">>),
				Token = crypto:hmac(sha256,Salt,Auth),
				case mnesia:read(webpage_auth,Token) of
					[ #webpage_auth{ user = User, email = Email, active = true, paths = Paths } ] ->
						case lists:foldl(fun(Pattern,Match) -> 
							webpage_path:match(Path,Pattern) or Match end, false, Paths) of
							true ->
								error_logger:info_msg("Allow ~p <~p> for ~p", [ User, Email, Path ]),
								Request#request{ headers = [ { <<"User">>, User},{<<"Email">>,Email } | Headers ] };
							false ->
								error_logger:info_msg("Deny ~p <~p> for ~p", [ User, Email, Path ]),
								#response{ status = 401, headers = [{  <<"WWW-Authenticate">>,<<"Basic realm=\"webpage\"">> }, {<<"Content-Length">>, <<"0">> }]}
						end;
					_ ->
						error_logger:info_msg("Deny token ~p for ~p", [ Auth, Path ]),
						#response{ status = 401, headers = [{  <<"WWW-Authenticate">>,<<"Basic realm=\"webpage\"">> }, {<<"Content-Length">>, <<"0">> }]}
				end
			end,
			mnesia:activity(transaction,F)		
	end.		

add(User,Email,Password) when is_list(User) ->
	add(list_to_binary(User),Email,Password);
add(User,Email,Password) when is_list(Email) ->
	add(User,list_to_binary(Email),Password);
add(User,Email,Password) when is_list(Password) ->
	add(User,Email,list_to_binary(Password));
add(User,Email,Password) ->
	{ ok, Salt } = application:get_env(webpage,salt),
	Auth = base64:encode(<< User/binary,":",Password/binary>>),
	Token = crypto:hmac(sha256,Salt,Auth),
	F = fun() ->
		remove(User,Email),
		ok = mnesia:write(#webpage_auth{ token = Token, user = User, email = Email, active = true, paths = [] }),
		error_logger:info_msg("Add User ~p <~p>", [ User, Email ])
	end,
	mnesia:activity(transaction,F).

remove(User,Email) when is_list(User) ->
	remove(list_to_binary(User),Email);
remove(User,Email) when is_list(Email) ->
	remove(User,list_to_binary(Email));
remove(User,Email) ->
	F = fun() ->
		case mnesia:match_object(#webpage_auth{ user = User, email = Email, token = '_', active = '_', paths = '_' }) of
			[] -> ok;
			Records ->
				error_logger:info_msg("Remove User ~p <~p>", [ User, Email ]),
				[ mnesia:delete_object(Record) || Record <- Records ]
		end
	end,
	mnesia:activity(transaction,F).

grant(User,Pattern) when is_list(User) ->
	grant(list_to_binary(User),Pattern);
grant(User,Pattern) ->
	F = fun() ->
		case mnesia:match_object(#webpage_auth{ user = User, email = '_', token = '_', active = true, paths = '_' }) of
			[] -> ok;
			[ Auth = #webpage_auth{ paths = Paths } ] ->
				ok = mnesia:write(Auth#webpage_auth{ paths = [ Pattern | Paths ]}),
				error_logger:info_msg("Granted ~p access to ~p", [ User, Pattern ])	
		end
	end,
	mnesia:activity(transaction,F).

revoke(User,Pattern) when is_list(User) ->
	revoke(list_to_binary(User), Pattern);
revoke(User,Pattern) ->
	F = fun() ->
		case mnesia:match_object(#webpage_auth{ user = User, email = '_', token = '_', active = true, paths = '_' }) of
			[] -> ok;
			[ Auth = #webpage_auth{ paths = Paths }] ->
				ok = mnesia:write(Auth#webpage_auth{ paths = lists:delete(Pattern,Paths) }),
				error_logger:info_msg("Revoked ~p access to ~p", [ User, Pattern ])
		end
	end,
	mnesia:activity(transaction,F).

users() ->
	F = fun() ->
		case mnesia:match_object(#webpage_auth{ user = '_', email = '_', token = '_', active = true, paths = '_' }) of
			Users when is_list(Users) ->
				[ User || #webpage_auth{ user = User } <- Users ];
			_ -> []	
		end
	end,
	mnesia:activity(transaction,F).

token(User) when is_list(User) ->
	token(list_to_binary(User));
token(User) ->
	F = fun() ->
		case mnesia:match_object(#webpage_auth{ user = User, email = '_', token = '_', active = true, paths = '_' }) of
			[ #webpage_auth{ token = Token }] ->
				Token;
			_ ->
				undefined
		end
	end,
	mnesia:activity(transaction,F).

install(Nodes) ->
	{ atomic, ok } = mnesia:create_table(webpage_auth, [
		{ attributes, record_info(fields,webpage_auth) },
		{ disc_copies, Nodes }]).
