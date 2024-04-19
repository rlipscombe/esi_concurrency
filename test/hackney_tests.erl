-module(hackney_tests).
-include_lib("eunit/include/eunit.hrl").

httpd_test_() ->
    {setup, fun setup/0, fun cleanup/1,
        {with, [
            fun pause/1,
            fun concurrent_requests/1
        ]}}.

setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    test_helper:start_httpd().

cleanup(Config) ->
    test_helper:stop_httpd(Config).

pause(_Config = #{base_url := BaseUrl}) ->
    Url = BaseUrl ++ "esi/esi_module:pause",
    ?debugMsg(Url),

    {ok, 200, _, _} = hackney:get(Url, [], <<>>, [with_body]),
    ok.

concurrent_requests(_Config = #{base_url := BaseUrl}) ->
    Url = BaseUrl ++ "esi/esi_module:pause",
    ?debugMsg(Url),

    % Start up a number of sessions in parallel.
    Parent = self(),
    Requests = lists:map(
        fun(_N) ->
            spawn_link(fun() ->
                % TODO: These complete sequentially. When I point them at a cowboy server, they complete in parallel;
                % this suggests that it's a problem with httpd or mod_esi. I wonder whether the mod_esi callback is blocked by the sleep.
                ?debugMsg("making request"),
                Result = hackney:get(Url, [], <<>>, [with_body]),
                Parent ! {self(), Result}
            end)
        end,
        lists:seq(1, 5)
    ),

    assert_requests(Requests),
    ok.

assert_requests([]) ->
    ok;
assert_requests(Requests) ->
    receive
        {Pid, Result} when is_pid(Pid) ->
            ?debugMsg("completed"),
            {ok, 200, _, _} = Result,
            assert_requests(lists:delete(Pid, Requests))
    end,
    ok.
