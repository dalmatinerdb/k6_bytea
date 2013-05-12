-module(k6_bytea_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    k6_bytea_sup:start_link().

stop(_State) ->
    ok.
