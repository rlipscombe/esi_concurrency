-module(esi_module).
-export([
    hello/3,
    pause/3
]).

-include_lib("eunit/include/eunit.hrl").

hello(SessionID, _Env, _Input) ->
    mod_esi:deliver(SessionID, "Content-Type: text/plain\r\n\r\n"),
    mod_esi:deliver(SessionID, "OK"),
    ok.

pause(SessionID, _Env, Input) when is_list(Input) ->
    ?debugMsg("enter pause"),
    mod_esi:deliver(SessionID, "Content-Type: text/plain\r\n\r\n"),
    ?debugMsg("sleeping"),
    timer:sleep(500),
    ?debugMsg("ok"),
    mod_esi:deliver(SessionID, "OK"),
    ok.
