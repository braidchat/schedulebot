:- module(schedule, [viable_time/2]).
:- use_module(library(clpfd)).
:- use_module(library(delay), [delay/1]).
:- use_module(library(julian), [form_time/2, datetime/3]).
:- use_module(library(julian/util), [dow_number/2]).
:- use_module(library(list_util), [xfy_list/3]).

:- multifile julian:form_time/2.
julian:form_time(not(dow(Day)), Dt) :-
    ground(Day),
    datetime(Dt, MJD, _),
    dow_number(Day, DayNumber),
    (MJD+2) mod 7 #\= DayNumber.
julian:form_time(not(Times), Dt) :-
    form_time(Times, NotDt),
    datetime(Dt, MJD, Ns),
    datetime(NotDt, NotMJD, NotNs),
    fd_dom(NotMJD, NotMJDDom),
    fd_dom(NotNs, NotNSDom),
    MJD in NotMJDDom #\ Ns in NotNSDom.

julian:form_time(day_hours(D, Hs), Dt) :-
    form_time([D, H:_:_], Dt),
    xfy_list(\/, Domain, Hs),
    H in Domains.

viable_time(Constraints, Dt) :-
    form_time(Constraints, Dt),
    form_time(H:_:_, Dt),
    H in 9..18,
    julian:datetime(Dt, MJD, Nanos),
    labeling([leftmost,up,bisect], [MJD]),
    labeling([leftmost,up,bisect], [Nanos]).

all_viable_times(Constraints, Ds) :-
    findall(D, viable_time(Constraints, D), Ds).

rfc_time(D, S) :-
    form_time(rfc3339(RfcCodes), D),
    string_codes(S, RfcCodes).

print_list([]).
print_list([A|As]) :-
    format('~w~n', [A]), print_list(As).

?- H in 10..13,
   all_viable_times([after(2018-02-21), before(2018-02-24), H:00:00,
                     %% day_hours(true, [11])
                     %% not(day_hours(2018-02-22, [10, 12])),
                     %% not(dow(friday)),
                     true
                    ],
                    Ds),
   maplist(schedule:rfc_time, Ds, Rfcs),
   format('~n', []),
   print_list(Rfcs).

%% ?- A in 0..3, B in 1..4, clpfd:all_distinct([A, B]).
