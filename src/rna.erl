-module(rna).

-export([run/1]).

run(Input) ->
    binary:replace(Input, <<"T">>, <<"U">>, [global]).
