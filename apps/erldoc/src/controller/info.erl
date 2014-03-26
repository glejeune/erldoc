-module(info).

-export([
  get/3
]).

get(_Request, User, Project) ->
  URL = "https://api.github.com/repos/" ++ User ++ "/" ++ Project,
  lager:info("---> ~p", [URL]),
  case httpc:request(get, {URL, [{"User-Agent", "ErlDoc/0.0.1"}]}, [], []) of
    {ok,{{_, 200, "OK"}, _, Body}} -> 
      JSon = jsx:decode(list_to_binary(Body)),
      lager:info("== ~p", [JSon]),
      paris_response:render_text("ok");
    E -> 
      lager:info("Github API => ~p", [E]),
      paris_response:redirect("/")
  end.
