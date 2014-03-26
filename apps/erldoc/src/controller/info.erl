-module(info).

-export([
  get/3
]).

get(_Request, User, Project) ->
  URL = "https://api.github.com/repos/" ++ User ++ "/" ++ Project,
  case httpc:request(get, {URL, [{"User-Agent", "ErlDoc/0.0.1"}]}, [], []) of
    {ok,{{_, 200, "OK"}, _, Body}} -> 
      JSon = jsx:decode(list_to_binary(Body)),
      Data = [] ++
      case lists:keyfind(<<"full_name">>, 1, JSon) of
        {<<"full_name">>, Fullname} -> [{full_name, binary_to_list(Fullname)}];
        _ -> []
      end ++
      case lists:keyfind(<<"html_url">>, 1, JSon) of 
        {<<"html_url">>, Url} -> [{html_url, binary_to_list(Url)}];
        _ -> []
      end ++
      case lists:keyfind(<<"description">>, 1, JSon) of
        {<<"description">>, Description} -> [{description, Description}];
        _ -> []
      end ++
      case lists:keyfind(<<"homepage">>, 1, JSon) of
        {<<"homepage">>, Homepage} -> [{homepage, Homepage}];
        _ -> []
      end,
      paris_response:render_view(info, Data);
    E -> 
      lager:info("Github API => ~p", [E]),
      paris_response:redirect("/")
  end.
