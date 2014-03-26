-module(docsrv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([add_project/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_project(ProjectURL, ProjectDir, User, Project) ->
  gen_server:cast(?SERVER, {add, ProjectURL, ProjectDir, User, Project}).

init(Args) ->
  {ok, Args}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({add, ProjectURL, ProjectDir, User, Project}, State) ->
  CloneDir = tempdir(),
  case file:make_dir(CloneDir) of
    ok ->
      lager:info("Clone ~p to ~p", [ProjectURL, CloneDir]),
      case git:clone(ProjectURL, CloneDir) of
        {ok, _} ->
          doc(CloneDir, ProjectDir),
          lists:foreach(fun(Extra) ->
                file:copy(
                  paris_helpers:static(["_", "doc", Extra]), 
                  filename:join(ProjectDir, Extra))
            end, ["stylesheet.css", "erldoc_header.html", "index.html"]),
          del_dir(CloneDir),
          docdb:add(User, Project, ProjectURL);
        E ->
          lager:error("Error ~p", [E])
      end;
    _ -> 
      lager:error("Can't create tempfile ~p for projetc ~p/~p", [CloneDir, User, Project])
  end,
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

doc(Root, OutDir) ->
  Files = filelib:wildcard(filename:join([Root, "src", "**", "*.erl"])) ++
          filelib:wildcard(filename:join([Root, "apps", "*", "src", "**", "*.erl"])),
  %        filelib:wildcard(filename:join([Root, "doc", "**", "*.edoc"])) ++
  %        filelib:wildcard(filename:join([Root, "_doc", "**", "*.edoc"])),
  edoc:files(Files, [{dir, OutDir}]).

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
