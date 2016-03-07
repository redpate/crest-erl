-module(crest_erl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  inets:start(),
  ssl:start(),
  crest_erl_sup:start_link().

stop(_State) ->
  ok.
