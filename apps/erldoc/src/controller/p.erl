-module(p).

-export([
    get/3
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

