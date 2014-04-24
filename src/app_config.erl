-module(app_config).

-export([pg_config/0,redis_config/0,gevenv_default/2,redis_sub_config/0]).

pg_config() ->
  pg_config(gevenv_default("DB_URL","postgres://habitpop:pass@localhost:5432/habitpop")).
pg_config (UrlString) ->
  {ok,{_Scheme, UserInfo, Host, _Port, Database, _Query}} = http_uri:parse(UrlString),
  "/" ++ DatabaseFixed = Database,
  [User,Password] = re:split(UserInfo,":"),
  [
    Host,
    DatabaseFixed,
    binary_to_list(User),
    binary_to_list(Password)
  ].

redis_config () ->
  UrlString = gevenv_default("REDIS_URL","redis://localhost:6379"),
  {ok,{_Scheme, UserInfo, Host, Port, _Path, _Query}} = http_uri:parse(UrlString),
  Database = 0, % default
  case UserInfo of
    "" -> [Host,Port,Database];
    _String ->
      [_User,Password] = re:split(UserInfo,":"),
      [
        Host,
        Port,
        Database,
        binary_to_list(Password)
      ]
  end.

redis_sub_config () ->
  Conf = redis_config(),
  case Conf of
    [Host,Port,_Database] ->
      [Host,Port,""];
    [H,P,_,Pw] ->
      [H,P,Pw]
  end.

gevenv_default (Var,Def) ->
  case os:getenv(Var) of
    false -> Def;
    Value -> Value
  end.

