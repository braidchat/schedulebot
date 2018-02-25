:- module(schedule, [all_viable_times/2]).
:- use_module(library(clpfd)).
:- use_module(library(julian), [form_time/2, datetime/3]).
:- use_module(library(julian/util), [dow_number/2]).
:- use_module(library(list_util), [xfy_list/3]).

% Extending Julian time forms so we can specify both specific hour
% spans and times that *don't* work
:- multifile julian:form_time/2.

% Specify a range of hours that work
% could be, e.g. hours([11, 15]) or hours([11..13, 14..16])
julian:form_time(hours(Hs), Dt) :-
    form_time(H:_:_, Dt),
    xfy_list(\/, Domain, Hs),
    H in Domain.
% Negative time specifiers
julian:form_time(not(dow(Day)), Dt) :-
    atom(Day), !,
    datetime(Dt, MJD, _),
    dow_number(Day, DayNumber),
    (MJD+2) mod 7 #\= DayNumber.
julian:form_time(not(dow(Ds)), Dt) :-
    is_list(Ds), !,
    datetime(Dt, MJD, _),
    maplist(dow_number, Ds, DayNumbers),
    xfy_list(\/, Domain, DayNumbers),
    #\ DayNumber in Domain,
    (MJD + 2) mod 7 #= DayNumber.
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
julian:form_time(not(Ts), Dt) :-
    is_list(Ts), memberchk(dow(_), Ts), !,
    datetime(Dt, MJD, Ns),
    % get just the dow(_) terms...
    findall(D, member(dow(D), Ts), Dows_),
    flatten(Dows_, Dows),
    maplist(dow_number, Dows, DayNumbers),
    xfy_list(\/, Domain, DayNumbers),
    % and the other terms, which we assume are times (not dates)
    subtract(Ts, Dows_, OtherTs),
    form_time(OtherTs, NotDt),
    datetime(NotDt, _, NotNs), fd_dom(NotNs, NotNsDom),

    DayNumber in Domain #/\ (MJD + 2) mod 7 #= DayNumber #==> #\ Ns in NotNsDom,
    Ns in NotNsDom #==> #\ DayNumber in Domain #/\ (MJD + 2) mod 7 #= DayNumber.
julian:form_time(not(Ts), Dt) :-
    is_list(Ts),
    form_time(Ts, NotDt),
    datetime(Dt, MJD, Ns),
    datetime(NotDt, NotMJD, NotNs),
    fd_dom(NotMJD, NotMJDDom),
    fd_dom(NotNs, NotNsDom),
    MJD in NotMJDDom #==> #\ Ns in NotNsDom,
    Ns in NotNsDom #==> #\ MJD in NotMJDDom.

% Building the schedule

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
