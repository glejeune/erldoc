-module(erldoc).

-export([start/0]).

start() ->
  {ok, _} = application:ensure_all_started(lager),
  {ok, _} = application:ensure_all_started(cowboy),
  {ok, _} = application:ensure_all_started(ssl),
  {ok, _} = application:ensure_all_started(inets),
  ok = application:start(mimetypes),
  ok = application:start(docsrv),
  ok = application:start(erldoc).
