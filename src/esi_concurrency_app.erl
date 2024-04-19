%%%-------------------------------------------------------------------
%% @doc esi_concurrency public API
%% @end
%%%-------------------------------------------------------------------

-module(esi_concurrency_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    esi_concurrency_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
