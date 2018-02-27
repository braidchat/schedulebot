:- module(schedule, [viable_time/2, all_viable_times/2, rfc_time/2]).
:- use_module(library(clpfd)).
:- use_module(library(julian), [form_time/2, datetime/3]).
:- use_module(library(julian/util), [dow_number/2]).
:- use_module(library(list_util), [xfy_list/3]).

ensure_list(X, X) :- is_list(X).
ensure_list(X, [X]).

% Extending Julian time forms so we can specify both specific hour
% spans and times that *don't* work
:- multifile julian:form_time/2.

% Specify a range of hours that work
% could be, e.g. hours([11, 15]) or hours([11..13, 14..16])
julian:form_time(hours(Hs), Dt) :-
    form_time(H:_:_, Dt),
    xfy_list(\/, Domain, Hs),
    H in Domain.
% specify a time on a day
julian:form_time(day_at(dow(Days_), Time), Dt) :-
    datetime(Dt, MJD, Ns),
    ensure_list(Days_, Days),
    maplist(dow_number, Days, DayNumbers),
    xfy_list(\/, DayDomain, DayNumbers),
    DayNumber in DayDomain,

    form_time(Time, TimeDt),
    datetime(TimeDt, _, TimeNs),
    fd_dom(TimeNs, NSDom),
    (MJD + 2) mod 7 #= DayNumber #==> Ns in NSDom, !.
julian:form_time(day_at(Date, Time), Dt) :-
    debug(schedule, 'day_at(~w, ~w)', [Date, Time]),
    datetime(Dt, MJD, Ns),
    form_time(Date, DayDt),
    form_time(Time, TimeDt),
    datetime(DayDt, DayMJD, _),
    datetime(TimeDt, _, TimeNs),
    fd_dom(DayMJD, MJDDom),
    fd_dom(TimeNs, NSDom),
    debug(schedule, 'time range ~w', [NSDom]),
    MJD in MJDDom #==> Ns in NSDom.
julian:form_time(Forms, Dt) :-
    % Non-empty list of just day_at forms
    is_list(Forms), =(Forms, [_|_]),
    forall(member(F, Forms), =(F, day_at(_, _))), !,
    debug(schedule, 'forms ~w', [Forms]),
    % Assert all the days_at..
    foreach(member(day_at(Da, T), Forms),
            form_time(day_at(Da, T), Dt)),
    % But then also assert that the day must be one of those days
    findall(Da, member(day_at(Da, _), Forms), Days),
    debug(schedule, 'day limit ~w', [Days]),
    form_time(Days, Dt).
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
    % dow(_) in not(_) requires special handling
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
    string_codes(S, RfcCodes), !.

%?- run_tests.
