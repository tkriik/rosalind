-module(dna).

-export([run/1]).

run(Input) ->
    {As, Cs, Gs, Ts} = compute(Input, {0, 0, 0, 0}),
    io_lib:format("~p ~p ~p ~p", [As, Cs, Gs, Ts]).

compute(<<>>, Acc) -> Acc;
compute(<<X, Xs/binary>>, {As, Cs, Gs, Ts} = Acc) ->
    case X of
        $A -> compute(Xs, {As + 1, Cs,     Gs,     Ts});
        $C -> compute(Xs, {As,     Cs + 1, Gs,     Ts});
        $G -> compute(Xs, {As,     Cs,     Gs + 1, Ts});
        $T -> compute(Xs, {As,     Cs,     Gs,     Ts + 1});
        _  -> compute(Xs, Acc)
    end.
