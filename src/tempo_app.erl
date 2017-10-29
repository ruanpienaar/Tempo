-module(tempo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("tempo.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    tempo_sup:start_link().

stop(_State) ->
    ok.
