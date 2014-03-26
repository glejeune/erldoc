-module(index).

-export([
  get/1
]).

get(_Request) ->
  Lasts = case docdb:last() of
    {ok, Projects} -> Projects;
    _ -> []
  end,
  paris_response:render_view(index, [{last_projects, Lasts}]).
