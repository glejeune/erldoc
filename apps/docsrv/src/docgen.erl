-module(docgen).

-export([
    generate/3,
    doc_generator/1
  ]).

generate(PubSubID, User, Project) ->
  gproc:reg({p, l, PubSubID}),
  GenPID = spawn(?MODULE, doc_generator, [self()]),
  generate_loop(GenPID, make_dirs, PubSubID, {User, Project}).

generate_loop(GenPID, Action, PubSubID, Parameters) ->
  GenPID ! {Action, Parameters},
  receive
    {make_dirs_ok, Parameters1} ->
      gproc:send({p, l, PubSubID}, [
          {message, <<"Cloning project...">>},
          {status, running}
        ]),
      generate_loop(GenPID, clone, PubSubID, Parameters1);
    {clone_ok, Parameters1} ->
      gproc:send({p, l, PubSubID}, [
          {message, <<"Generating documentation...">>},
          {status, running}
        ]),
      generate_loop(GenPID, create_doc, PubSubID, Parameters1);
    {create_doc_ok, Parameters1} ->
      gproc:send({p, l, PubSubID}, [
          {message, <<"Updating style...">>},
          {status, running}
        ]),
      generate_loop(GenPID, update_style, PubSubID, Parameters1);
    {update_style_ok, Parameters1} ->
      gproc:send({p, l, PubSubID}, [
          {message, <<"Registering project...">>},
          {status, running}
        ]),
      generate_loop(GenPID, register, PubSubID, Parameters1);
    {register_ok, Parameters1} ->
      gproc:send({p, l, PubSubID}, [
          {message, <<"Cleaning...">>},
          {status, done}
        ]),
      GenPID ! {clean, Parameters1};
    {error, Parameters1} -> 
      gproc:send({p, l, PubSubID}, [
          {status, error}
        ]),
      lager:debug("error, ~p", [Parameters1]),
      GenPID ! {remove, Parameters1}
  end.

% --

doc_generator(CallerPID) ->
  receive
    {make_dirs, {User, Project}} ->
      lager:debug("make_dirs {~p, ~p}", [User, Project]),
      UserDir = paris:static(User),
      ProjectDir = paris:static([User, Project]),
      _ = file:make_dir(UserDir),
      case file:make_dir(ProjectDir) of
        {error, Reason} when Reason =/= eexist -> 
          CallerPID ! {error, {User, Project}};
        _ ->
          CallerPID ! {make_dirs_ok, {User, Project, ProjectDir}},
          doc_generator(CallerPID)
      end;
    {clone, {User, Project, ProjectDir}} -> 
      lager:debug("clone {~p, ~p, ~p}", [User, Project, ProjectDir]),
      CloneDir = tempdir(),
      ProjectURL = "https://github.com/" ++ User ++ "/" ++ Project,
      case file:make_dir(CloneDir) of
        ok ->
          case git:clone(ProjectURL, CloneDir) of
            {ok, _} -> 
              CallerPID ! {clone_ok, {User, Project, ProjectDir, CloneDir}},
              doc_generator(CallerPID);
            _ -> 
              CallerPID ! {error, {User, Project, ProjectDir, CloneDir}},
              doc_generator(CallerPID)
          end;
        _ ->
          CallerPID ! {error, {User, Project, ProjectDir}},
          doc_generator(CallerPID)
      end;
    {create_doc, {User, Project, ProjectDir, CloneDir}} ->
      lager:debug("create_doc {~p, ~p, ~p, ~p}", [User, Project, ProjectDir, CloneDir]),
      case build_doc(CloneDir, ProjectDir) of
        ok ->
          CallerPID ! {create_doc_ok, {User, Project, ProjectDir, CloneDir}},
          doc_generator(CallerPID);
        error ->
          CallerPID ! {error, {User, Project, ProjectDir, CloneDir}},
          doc_generator(CallerPID)
      end;
    {update_style, {User, Project, ProjectDir, CloneDir}} ->
      lager:debug("update_style {~p, ~p, ~p, ~p}", [User, Project, ProjectDir, CloneDir]),
      lists:foreach(fun(Extra) ->
            file:copy(
              paris:static(["_", "doc", Extra]), 
              filename:join(ProjectDir, Extra))
        end, ["stylesheet.css", "erldoc_header.html", "index.html"]),
      CallerPID ! {update_style_ok, {User, Project, ProjectDir, CloneDir}},
      doc_generator(CallerPID);
    {register, {User, Project, ProjectDir, CloneDir}} -> 
      lager:debug("register {~p, ~p, ~p, ~p}", [User, Project, ProjectDir, CloneDir]),
      ProjectURL = "https://github.com/" ++ User ++ "/" ++ Project,
      case docdb:find(User, Project) of
        {ok, []} -> 
          docdb:add(User, Project, ProjectURL),
          CallerPID ! {register_ok, {User, Project, ProjectDir, CloneDir}},
          doc_generator(CallerPID);
        {ok, [P|_]} -> 
          case lists:keyfind(id, 1, P) of
            {id, ID} -> 
              docdb:update(ID),
              CallerPID ! {register_ok, {User, Project, ProjectDir, CloneDir}},
              doc_generator(CallerPID);
            _ -> 
              CallerPID ! {error, {User, Project, ProjectDir, CloneDir}},
              doc_generator(CallerPID)
          end;
        _ -> 
          CallerPID ! {error, {User, Project, ProjectDir, CloneDir}},
          doc_generator(CallerPID)
      end;
    {clean, {User, Project, ProjectDir, CloneDir}} ->
      lager:debug("clean {~p, ~p, ~p, ~p}", [User, Project, ProjectDir, CloneDir]),
      del_dir(CloneDir);
    {remove, {User, Project}} ->
      lager:debug("remove {~p, ~p}", [User, Project]),
      ok;
    {remove, {User, Project, ProjectDir}} ->
      lager:debug("remove {~p, ~p, ~p}", [User, Project, ProjectDir]),
      case docdb:find(User, Project) of
        {ok, []} -> 
          del_dir(ProjectDir);
        {ok, [_|_]} -> 
          ok
      end;
    {remove, {User, Project, ProjectDir, CloneDir}} ->
      lager:debug("remove {~p, ~p, ~p, ~p}", [User, Project, ProjectDir, CloneDir]),
      del_dir(ProjectDir),
      del_dir(CloneDir),
      case docdb:find(User, Project) of
        {ok, []} -> ok;
        {ok, [P|_]} -> 
          case lists:keyfind(id, 1, P) of
            {id, ID} -> docdb:delete(ID);
            _ -> ok
          end
      end
  end.

% - Private -

tempdir() ->
  [TmpDir|_] = lists:dropwhile(fun(E) ->
        E =:= false orelse filelib:is_dir(E) =:= false
    end, [
      os:getenv("TMPDIR"),
      os:getenv("TMP"),
      os:getenv("TEMP"),
      "/tmp", "."]),
  {A,B,C}=now(),
  N=node(),
  filename:join(TmpDir, lists:flatten(io_lib:format("~p-~p.~p.~p",[N,A,B,C]))).

build_ex_doc(_Root, _OutDir) ->
  % 'Elixir.Mix'.start().
  % 'Elixir.Code':load_file(<<"deps/ex_doc/mix.exs">>).
  % 'Elixir.Mix':project(). % -> [{app, ex_doc}, {version,<<"0.1.0">>}, ...]
  % 'Elixir.ExDoc':generate_docs(<<"ex_doc">>, <<"0.1.0">>, [{source_root, <<"deps/ex_docs">>}, {output, <<"__exdocs_test">>}, {source_beam, <<"deps/ex_doc/_build/dev/lib/ex_doc/ebin">>}]).
  ok.

build_doc(Root, OutDir) ->
  % @TODO add hrl files
  process_flag(trap_exit, true),
  Files = filelib:wildcard(filename:join([Root, "src", "**", "*.erl"])) ++
          filelib:wildcard(filename:join([Root, "apps", "*", "src", "**", "*.erl"])),
  Opts = [{dir, OutDir}] ++
  case filelib:wildcard(filename:join([Root, "doc", "**", "overview.edoc"])) ++
       filelib:wildcard(filename:join([Root, "_doc", "**", "overview.edoc"])) of
    [] -> [];
    [Overview|_] -> [{overview, Overview}]
  end,
  NBFiles = length(Files) + length(Opts),
  lager:info("~p files.... ~p, ~p ", [NBFiles, Files, Opts]),
  if
    NBFiles =< 1 -> error;
    true ->
      spawn_link(edoc, files, [Files, Opts]),
      receive
        {'EXIT', _, error} -> 
          lager:debug("build_doc error"),
          error;
        R -> 
          lager:debug("build_doc ok (~p)", [R]),
          ok
      end
  end.

del_dir(Dir) ->
  lists:foreach(fun(D) ->
        ok = file:del_dir(D)
    end, del_all_files([Dir], [])).
del_all_files([], EmptyDirs) ->
  EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
  {ok, FilesInDir} = file:list_dir(Dir),
  {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
          Path = Dir ++ "/" ++ F,
          case filelib:is_dir(Path) of
            true ->
              {Fs, [Path | Ds]};
            false ->
              {[Path | Fs], Ds}
          end
      end, {[],[]}, FilesInDir),
  lists:foreach(fun(F) ->
        ok = file:delete(F)
    end, Files),
  del_all_files(T ++ Dirs, [Dir | EmptyDirs]).
