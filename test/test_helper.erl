-module(test_helper).
-export([
    start_httpd/0,
    stop_httpd/1,

    start_cowboy/0,
    stop_cowboy/1
]).

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

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", home_h, []},
            {"/pause", pause_h, []}
        ]}
    ]),
    {ok, Pid} =
        cowboy:start_clear(?MODULE, [{port, 0}], #{env => #{dispatch => Dispatch}}),
    #{port := Port} = ranch:info(?MODULE),
    BaseUrl = uri_string:recompose(#{scheme => "http", host => "localhost", port => Port, path => "/"}),
    #{cowboy_pid => Pid, base_url => BaseUrl}.

stop_cowboy(_Config) ->
    ok.
