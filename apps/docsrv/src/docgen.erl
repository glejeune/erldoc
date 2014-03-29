-module(docgen).

-export([
    generate/3,
    doc_generator/1
  ]).

generate(PubSubID, User, Project) ->
  gproc:reg({p, l, PubSubID}),
  process_flag(trap_exit, true),
  GenPID = spawn(docgen, doc_generator, [self()]),
  generate_loop(GenPID, make_dirs, PubSubID, {User, Project}).

generate_loop(GenPID, Action, PubSubID, Parameters) ->
  GenPID ! {Action, Parameters},
  receive
    {make_dirs_ok, Parameters1} ->
      gproc:send({p, l, PubSubID}, [
          {message, <<"Directories created">>},
          {status, running}
        ]),
      generate_loop(GenPID, clone, PubSubID, Parameters1);
    {clone_ok, Parameters1} ->
      gproc:send({p, l, PubSubID}, [
          {message, <<"Project cloned">>},
          {status, running}
        ]),
      generate_loop(GenPID, create_doc, PubSubID, Parameters1);
    {create_doc_ok, Parameters1} ->
      gproc:send({p, l, PubSubID}, [
          {message, <<"Documentation generated">>},
          {status, running}
        ]),
      generate_loop(GenPID, update_style, PubSubID, Parameters1);
    {update_style_ok, Parameters1} ->
      gproc:send({p, l, PubSubID}, [
          {message, <<"Terminate doc creation">>},
          {status, running}
        ]),
      generate_loop(GenPID, register, PubSubID, Parameters1);
    {register_ok, Parameters1} ->
      gproc:send({p, l, PubSubID}, [
          {message, <<"Project registered">>},
          {status, done}
        ]),
      GenPID ! {clean, Parameters1};
    {'EXIT', _, _Status} -> 
      gproc:send({p, l, PubSubID}, [
          {message, <<"Internal error">>},
          {status, error}
        ]); 
    {error, Err} -> 
      gproc:send({p, l, PubSubID}, [
          {error, Err},
          {status, error}
        ]) 
  end.

% --

doc_generator(CallerPID) ->
  receive
    {make_dirs, {User, Project}} ->
      UserDir = paris_helpers:static(User),
      ProjectDir = paris_helpers:static([User, Project]),
      _ = file:make_dir(UserDir),
      case file:make_dir(ProjectDir) of
        {error, Reason} when Reason =/= eexist -> 
          CallerPID ! {error, <<"Can't create project directory">>};
        _ ->
          CallerPID ! {make_dirs_ok, {User, Project, ProjectDir}},
          doc_generator(CallerPID)
      end;
    {clone, {User, Project, ProjectDir}} -> 
      CloneDir = tempdir(),
      ProjectURL = "https://github.com/" ++ User ++ "/" ++ Project,
      case file:make_dir(CloneDir) of
        ok ->
          case git:clone(ProjectURL, CloneDir) of
            {ok, _} -> 
              CallerPID ! {clone_ok, {User, Project, ProjectDir, CloneDir}},
              doc_generator(CallerPID);
            _ -> 
              CallerPID ! {error, <<"Can't clone project.">>}
          end;
        _ ->
          CallerPID ! {error, <<"Can't create temp directory.">>}
      end;
    {create_doc, {User, Project, ProjectDir, CloneDir}} ->
      build_doc(CloneDir, ProjectDir),
      CallerPID ! {create_doc_ok, {User, Project, ProjectDir, CloneDir}},
      doc_generator(CallerPID);
    {update_style, {User, Project, ProjectDir, CloneDir}} ->
      lists:foreach(fun(Extra) ->
            file:copy(
              paris_helpers:static(["_", "doc", Extra]), 
              filename:join(ProjectDir, Extra))
        end, ["stylesheet.css", "erldoc_header.html", "index.html"]),
      CallerPID ! {update_style_ok, {User, Project, ProjectDir, CloneDir}},
      doc_generator(CallerPID);
    {register, {User, Project, ProjectDir, CloneDir}} -> 
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
              CallerPID ! {error, <<"Can't update project in database.">>}
          end;
        _ -> 
          CallerPID ! {error, <<"Database error.">>}
      end;
    {clean, {_User, _Project, _ProjectDir, CloneDir}} ->
      del_dir(CloneDir)
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

build_doc(Root, OutDir) ->
  Files = filelib:wildcard(filename:join([Root, "src", "**", "*.erl"])) ++
          filelib:wildcard(filename:join([Root, "apps", "*", "src", "**", "*.erl"])),
  Opts = [{dir, OutDir}] ++
  case filelib:wildcard(filename:join([Root, "doc", "**", "overview.edoc"])) ++
       filelib:wildcard(filename:join([Root, "_doc", "**", "overview.edoc"])) of
    [] -> [];
    [Overview|_] -> [{overview, Overview}]
  end,
  edoc:files(Files, Opts).

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
