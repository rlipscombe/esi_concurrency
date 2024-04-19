# esi_concurrency

Attempt to reproduce an issue with concurrency in mod_esi.

Instead, it appears that it's an issue with concurrency in httpc.

## Reproduction

Run the tests with `rebar3 eunit`.

Look in `test/httpc_tests.erl`. In this bit:

```erlang
httpd_test_() ->
    {setup, fun setup/0, fun cleanup/1,
        {with, [
            fun pause/1,
            fun concurrent_requests/1
        ]}}.
```

If `pause/1` is included, then the 5 concurrent requests in `concurrent_requests/1` aren't run concurrently.

Change it to this:

```erlang
httpd_test_() ->
    {setup, fun setup/0, fun cleanup/1,
        {with, [
            %fun pause/1,
            fun concurrent_requests/1
        ]}}.
```

If `pause/1` is NOT run, then the 5 concurrent requests in `concurrent_requests/1` ARE run concurrently.
