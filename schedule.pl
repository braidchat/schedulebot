:- module(schedule, [viable_time/2]).
:- use_module(library(clpfd)).
:- use_module(library(julian)).

viable_time(Constraints, Dt) :-
    form_time(Constraints, Dt),
    julian:datetime(Dt, MJD, Nanos),
    labeling([leftmost,up,bisect], [MJD]),
    labeling([leftmost,up,bisect], [Nanos]).

all_viable_times(Constraints, Ds) :-
    findall(D, viable_time(Constraints, D), Ds).

rfc_time(D, S) :-
    form_time(rfc3339(RfcCodes), D),
    string_codes(S, RfcCodes).

?- all_viable_times([after(2018-02-21), before(2018-02-28), _H:00:00], Ds),
   maplist(schedule:rfc_time, Ds, Rfcs),
   [D1, D2, D3|_] = Rfcs,
   format('first times ~w, ~w, ~w ~n', [D1, D2, D3]).
