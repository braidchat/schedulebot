:- module(schedule, [all_viable_times/2]).
:- use_module(library(clpfd)).
:- use_module(library(julian), [form_time/2, datetime/3]).
:- use_module(library(julian/util), [dow_number/2]).
:- use_module(library(list_util), [xfy_list/3]).

:- multifile julian:form_time/2.
julian:form_time(not(dow(Day)), Dt) :-
    ground(Day),
    datetime(Dt, MJD, _),
    dow_number(Day, DayNumber),
    (MJD+2) mod 7 #\= DayNumber.
julian:form_time(not(Y-M-D), Dt) :-
    form_time(Y-M-D, NotDt),
    datetime(Dt, MJD, _),
    datetime(NotDt, NotMJD, _),
    fd_dom(NotMJD, NotMJDDom),
    #\ MJD in NotMJDDom.
julian:form_time(not(hours(Hs)), Dt) :-
    form_time(H:_:_, Dt),
    xfy_list(\/, Domain, Hs),
    #\ H in Domain.
% XXX: to work with general `not` clauses, the date part needs to be
% concrete -- e.g., 2018-02-23, not dow(thursday)
julian:form_time(not(Ts), Dt) :-
    is_list(Ts),
    form_time(Ts, NotDt),
    datetime(Dt, MJD, Ns),
    datetime(NotDt, NotMJD, NotNs),
    fd_dom(NotMJD, NotMJDDom),
    fd_dom(NotNs, NotNsDom),
    MJD in NotMJDDom #==> #\ Ns in NotNsDom,
    Ns in NotNsDom #==> #\ MJD in NotMJDDom.

julian:form_time(hours(Hs), Dt) :-
    form_time(H:_:_, Dt),
    xfy_list(\/, Domain, Hs),
    H in Domain.

viable_time(Constraints, Dt) :-
    form_time(Constraints, Dt),
    form_time(_:00:00, Dt),
    julian:datetime(Dt, MJD, Nanos),
    labeling([leftmost,up,bisect], [MJD]),
    labeling([leftmost,up,bisect], [Nanos]).

all_viable_times(Constraints, Ds) :-
    findall(D, viable_time(Constraints, D), Ds).

rfc_time(D, S) :-
    form_time(rfc3339(RfcCodes), D),
    string_codes(S, RfcCodes).

%:- use_module(library(plunit)).
%?- load_test_files([]), run_tests.
