-module(pause_h).
-export([init/2, info/3]).

-include_lib("eunit/include/eunit.hrl").

init(Req, _Opts) ->
    Ms = 500,
    timer:send_after(Ms, reply),
    State = no_state,
    ?debugMsg("enter pause"),
    {cowboy_loop, Req, State, hibernate}.

info(reply, Req, State) ->
    ?debugMsg("ok"),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"OK\r\n">>, Req),
    {stop, Req2, State};
info(_Msg, Req, State) ->
    {ok, Req, State, hibernate}.
