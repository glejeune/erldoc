-module(list).

-export([
  get/1,
  get/2
]).

get(_Request) ->
  Lasts = case docdb:last(25) of
    {ok, Projects} -> Projects;
    _ -> []
  end,
  paris_response:render_view(list, [{last_projects, Lasts}, {info, "- Recently Updated (" ++ integer_to_list(length(Lasts)) ++ ")"}]).

get(_Request, Term) ->
  Lasts = case docdb:search(Term) of
    {ok, Projects} -> Projects;
    _ -> []
  end,
  paris_response:render_view(list, [{last_projects, Lasts}, {info, "/"++Term++"/"}]).
