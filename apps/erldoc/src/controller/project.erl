-module(project).

-export([
    init/3,
    handle/3,
    info/3,
    terminate/3
  ]).

init(_TransportName, Req, State) ->
  paris_response:ws_ok(Req, State).

handle({text, Data}, Req, State) ->
  [ID, User, Project|_] = string:tokens(binary_to_list(Data), "/"),
  PubSubID = list_to_atom(ID),
  gproc:reg({p, l, list_to_atom(ID)}),
  spawn(docgen, generate, [PubSubID, User, Project]),
  paris_response:ws_ok(Req, State);
handle(_, Req, State) ->
  paris_response:ws_ok(Req, State).

info(Msg, Req, State) ->
  paris_response:ws_json(Req, State, Msg).

terminate(_, _, _) ->
  paris_response:ws_terminate().

