-module(check).

-export([
  get/3
]).

get(_Request, User, Project) ->
  case httpc:request(head, {"https://github.com/" ++ User ++ "/" ++ Project, []}, [], []) of
    {ok,{{_, 200, "OK"}, _, _}} -> 
      paris_response:render_text("ok");
    _ -> 
      paris_response:http_error(404)
  end.
