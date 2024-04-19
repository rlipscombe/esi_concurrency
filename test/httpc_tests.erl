-module(httpc_tests).
-include_lib("eunit/include/eunit.hrl").

httpd_test_() ->
    {setup, fun setup/0, fun cleanup/1,
        {with, [
            fun pause/1,
            fun concurrent_requests/1
        ]}}.

-define(PROFILE, ?MODULE).

setup() ->
    {ok, _} = inets:start(httpc, [{profile, ?PROFILE}]),
    ok = httpc:set_options([{max_sessions, 5}], ?PROFILE),
    start_httpd().

cleanup(Config) ->
    stop_httpd(Config).

pause(_Config = #{base_url := BaseUrl}) ->
    Url = BaseUrl ++ "esi/esi_module:pause",
    ?debugMsg(Url),

    {ok, {{_, 200, _}, _, _}} = httpc:request(get, {Url, []}, [], [], ?PROFILE),
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
                Result = httpc:request(get, {Url, []}, [], [], ?PROFILE),
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
            {ok, {{_, 200, _}, _, _}} = Result,
            assert_requests(lists:delete(Pid, Requests))
    end,
    ok.

start_httpd() ->
    % rebar3 runs tests with the current directory set to the project root; use that.
    {ok, Root} = file:get_cwd(),
    HttpdConfig = [
        {port, 0},
        {server_name, "Erlang-httpd"},
        {server_root, Root},
        {document_root, Root},

        % erl_script_alias takes a list of allowed modules.
        % /esi/Module:Fun or /esi/Module/Fun
        {erl_script_alias, {"/esi", [esi_module]}},
        {erl_script_nocache, true}
    ],
    {ok, Pid} = inets:start(httpd, HttpdConfig),
    {port, Port} = lists:keyfind(port, 1, httpd:info(Pid)),
    Url = uri_string:recompose(#{scheme => "http", host => "localhost", path => "/", port => Port}),
    #{httpd => Pid, base_url => Url}.

stop_httpd(#{httpd := Pid}) ->
    ok = inets:stop(httpd, Pid).

% cowboy_test_() ->
%     {setup, fun start_cowboy/0, fun stop_cowboy/1, {with, []}}.
