-module(rosalind).

-export([run/1, run_sample/1]).

-include_lib("stdlib/include/assert.hrl").

read_file(Directory, Module) ->
    Path = join_path(Directory, ["rosalind_", Module, ".txt"]),
    {ok, Data} = file:read_file(Path),
    Data.

write_file(Directory, Module, Data) ->
    Path = join_path(Directory, ["rosalind_", Module, ".txt"]),
    ok = file:write_file(Path, Data).

purge(Module) ->
    code:purge(Module).

compile(Module) ->
    Path = join_path("src/", [Module, ".erl"]),
    Opts = [verbose, report_errors, report_warnings, {outdir, "ebin/"}],
    {ok, Module} = compile:file(Path, Opts),
    ok.

load(Module) ->
    {module, Module} = code:load_file(Module).

run_sample(Module) ->
    purge(Module),
    compile(Module),
    load(Module),
    Input = read_file("sample/input/", Module),
    Expected = read_file("sample/output/", Module),
    Actual = ensure_binary(erlang:apply(Module, run, [Input])),
    ?assertEqual(Expected, Actual),
    io:format(standard_error, "~s~n", [Actual]).

run(Module) ->
    purge(Module),
    compile(Module),
    load(Module),
    Input = read_file("actual/input/", Module),
    Actual = ensure_binary(erlang:apply(Module, run, [Input])),
    io:format("-----BEGIN ANSWER-----~n", []),
    io:format("~s~n", [Actual]),
    io:format("-----END ANSWER-----~n", []),
    ok = write_file("actual/output/", Module, Actual).

join_path(Dir, Parts) ->
    _Parts = lists:map(fun ensure_string/1, Parts),
    filename:join(Dir, lists:append(_Parts)).

ensure_string(S) when erlang:is_list(S) -> S;
ensure_string(S) when erlang:is_atom(S) -> erlang:atom_to_list(S).

ensure_binary(B) when erlang:is_binary(B) -> B;
ensure_binary(B) when erlang:is_list(B)   -> erlang:list_to_binary(B).
