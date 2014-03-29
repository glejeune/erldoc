-module(p).

-export([
    get/3
  ]).

-export([
    init/3,
    handle/3,
    info/3,
    terminate/3
  ]).

get(Req, User, Project) ->
  ProjectURL = "https://github.com/" ++ User ++ "/" ++ Project,
  case httpc:request(head, {ProjectURL, []}, [], []) of
    {ok,{{_, 200, "OK"}, _, _}} -> 
      {Host, _} = cowboy_req:host(Req),
      {Port, _} = cowboy_req:port(Req),
      paris_response:render_view(p, [{user, User}, {project, Project}, {host, Host}, {port, Port}]);
    _ -> 
      paris_response:redirect("/")
  end.

init(_TransportName, Req, State) ->
  lager:info("ws init"),
  paris_response:ws_ok(Req, State).

handle({text, Data}, Req, State) ->
  lager:info("workinfo ID = ~p", [Data]),
  [ID, User, Project|_] = string:tokens(binary_to_list(Data), "/"),
  PubSubID = list_to_atom(ID),
  gproc:reg({p, l, list_to_atom(ID)}),
  spawn(docgen, generate, [PubSubID, User, Project]),
  paris_response:ws_ok(Req, State);
handle(_, Req, State) ->
  lager:info("ws handle"),
  paris_response:ws_ok(Req, State).

info(Msg, Req, State) ->
  lager:info("ws info"),
  paris_response:ws_json(Req, State, Msg).

terminate(_, _, _) ->
  paris_response:ws_terminate().

