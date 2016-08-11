-module(webpage_auth).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-export([ get/1, add/3, remove/2, install/1 ]).

-include("include/http.hrl").

-record(user_auth, { token, user, email, active }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

get(Request = #request{ path = Path, headers = Headers }) ->
	io:format("headers are ~p~n",[ Headers ]),
	case proplists:get_value(<<"Authorization">>, Headers ) of
		undefined ->
			io:format("challenging~n"),
			#response{ status = 401, headers = [{  <<"WWW-Authenticate">>,<<"Basic realm=\"webpage\"">> }]};
		Authorization ->
			F = fun() ->
				{ ok, Salt } = application:get_env(webpage,salt),
				[ <<"Basic">>,Auth ] = binary:split(Authorization,<<" ">>),
				Token = crypto:hmac(sha256,Salt,Auth),
				case mnesia:read(user_auth,Token) of
					[ #user_auth{ user = User, email = Email, active = true } ] ->
						Request#request{ headers = [ { <<"User">>, User},{<<"Email">>,Email } | Headers ] };
					_ ->
						#response{ status = 401, headers = [{  <<"WWW-Authenticate">>,<<"Basic realm=\"webpage\"">> }]}
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
		mnesia:write(#user_auth{ token = Token, user = User, email = Email, active = true })
	end,
	mnesia:activity(transaction,F).

remove(User,Email) ->
	F = fun() ->
		case mnesia:match_object(#user_auth{ user = User, email = Email, token = '_', active = '_' }) of
			[] -> ok;
			Records ->
				[ mnesia:delete_object(Record) || Record <- Records ]
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

