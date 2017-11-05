-module(revc).

-export([run/1]).

run(Input) ->
    compute(Input, <<>>).

compute(<<>>, Output) ->
    Output;
compute(<<X, Xs/binary>>, Output) ->
    case X of
        $A -> compute(Xs, <<"T", Output/binary>>);
        $T -> compute(Xs, <<"A", Output/binary>>);
        $G -> compute(Xs, <<"C", Output/binary>>);
        $C -> compute(Xs, <<"G", Output/binary>>);
        _  -> Output
    end.
