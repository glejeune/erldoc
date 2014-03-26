-module(project).

-export([
  get/3
]).

get(_Request, User, Project) ->
  ProjectURL = "https://github.com/" ++ User ++ "/" ++ Project,
  UserDir = paris_helpers:static(User),
  ProjectDir = paris_helpers:static([User, Project]),
  _ = file:make_dir(UserDir),
  case file:make_dir(ProjectDir) of
    {error, Reason} when Reason =/= eexist -> 
      paris_response:render_view(project, [{user, User}, {project, Project}, {error, Reason}]);
    _ ->
      case httpc:request(head, {ProjectURL, []}, [], []) of
        {ok,{{_, 200, "OK"}, _, _}} -> 
          docsrv:add_project(ProjectURL, ProjectDir, User, Project),
          paris_response:render_view(project, [{user, User}, {project, Project}]);
        _ -> 
          paris_response:redirect("/")
      end
  end.
