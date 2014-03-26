-module(docdb).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("../../../deps/epgsql/include/pgsql.hrl").

-export([start_link/0]).

-export([
    search/1,
    last/0,
    last/1,
    add/3,
    update/1
  ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

search(Project) ->
  gen_server:call(?SERVER, {search, Project}).

last() -> last(10).
last(N) ->
  gen_server:call(?SERVER, {last, N}).

add(Repo, Project, GitURL) ->
  gen_server:call(?SERVER, {add, Repo, Project, GitURL}).

update(ID) ->
  gen_server:call(?SERVER, {update, ID}).

init(_Args) ->
  DBHost = case application:get_env(docsrv, db_host) of
    {ok, Host} when is_list(Host) -> Host;
    {ok, Host} when is_binary(Host) -> binary_to_list(Host);
    _ -> "localhost"
  end,
  DBUser = case application:get_env(docsrv, db_username) of
    {ok, User} when is_list(User) -> User;
    {ok, User} when is_binary(User) -> binary_to_list(User);
    _ -> "erldoc"
  end,
  DBPass = case application:get_env(docsrv, db_password) of
    {ok, Pass} when is_list(Pass) -> Pass;
    {ok, Pass} when is_binary(Pass) -> binary_to_list(Pass);
    _ -> "erldoc"
  end,
  DBDatabase = case application:get_env(docsrv, db_database) of
    {ok, Database} when is_list(Database) -> Database;
    {ok, Database} when is_binary(Database) -> binary_to_list(Database);
    _ -> "erldoc"
  end,
  DBPort = case application:get_env(docsrv, db_port) of
    {ok, Port} when is_integer(Port) -> Port;
    _ -> 5432
  end,
  pgsql:connect(DBHost, DBUser, DBPass, [{database, DBDatabase}, {post, DBPort}]).

handle_call({search, Project}, _From, State) ->
  case pg_query(State, "select * from project where project like $1", ["%" ++ Project ++ "%"]) of
    {ok, Columns, Results} ->  {reply, {ok, format_results(Columns, Results)}, State};
    {error, #error{message = Message}} -> {reply, {error, binary_to_list(Message)}, State}
  end;
handle_call({last, Limit}, _From, State) ->
  case pg_query(State, "select * from project order by update_date desc limit $1", [Limit]) of 
    {ok, Columns, Results} ->  {reply, {ok, format_results(Columns, Results)}, State};
    {error, #error{message = Message}} -> {reply, {error, binary_to_list(Message)}, State}
  end;
handle_call({add, Repo, Project, GitURL}, _From, State) ->
  case pg_query(State, "insert into project (repo, project, giturl) values ($1, $2, $3)", [Repo, Project, GitURL]) of
    {error, #error{message = Message}} -> {reply, {error, binary_to_list(Message)}, State};
    OK -> {reply, OK, State}
  end;
handle_call({update, ID}, _From, State) ->
  case pg_query(State, "update project set update_date=$1 where id=$2", [erlang:date(), ID]) of
    {error, #error{message = Message}} -> {reply, {error, binary_to_list(Message)}, State};
    OK -> {reply, OK, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

format_results(Columns, Results) ->
  lists:foldl(fun(E, Acc) ->
        Acc ++ [[
          build_kv(lists:nth(I,Columns), element(I,E))
          || 
          I <- lists:seq(1,tuple_size(E))
        ]]
    end, [], Results).
build_kv({column, K, T, _, _, _}, V) -> 
  case T of
    date -> 
      {binary_to_atom(K, utf8), lists:flatten(io_lib:format("~B-~2.10.0B-~2.10.0B", tuple_to_list(V)))};
    _ -> 
      {binary_to_atom(K, utf8), V}
  end.

% TODO : pull request to https://github.com/epgsql/epgsql
pg_query(C, Sql, Parameters) ->
  Name = ["equery-", atom_to_list(node()), pid_to_list(self())],
  case pgsql:parse(C, Name, Sql, []) of
    {ok, #statement{types = Types} = S} ->
      Typed_Parameters = lists:zip(Types, Parameters),
      case gen_server:call(C, {equery, S, Typed_Parameters}, infinity) of
        {error, _} = Error -> pgsql:close(C, S), Error;
        OK -> OK
      end;
    Error ->
      Error
  end.

