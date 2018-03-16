:- module(schedule, [viable_time/2, all_viable_times/2, datetimes_format/2]).
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

% Any of the given apply
julian:form_time(any([]), _Dt) :- false.
julian:form_time(any([F|Fs]), Dt) :-
    form_time(F, Dt) ; form_time(any(Fs), Dt).

% specify a time on a day
julian:form_time(day_at(Date, Time), Dt) :-
    datetime(Dt, MJD, Ns),
    form_time(Date, DayDt),
    form_time(Time, TimeDt),
    datetime(DayDt, MJD, _),
    datetime(TimeDt, _, Ns).
julian:form_time(day_at(Date, _Time), Dt) :-
    form_time(not(Date), Dt).

julian:form_time(one_of(Forms), Dt) :-
    % Non-empty list of just day_at forms
    is_list(Forms), =(Forms, [_|_]),
    forall(member(F, Forms), =(F, day_at(_, _))), !,
    % also assert that the day must be one of those days
    findall(Da, member(day_at(Da, _), Forms), Days),
    FormsPlus = [any(Days)|Forms],
    form_time(FormsPlus, Dt).

% Negative time specifiers
julian:form_time(not(Ts), Dt) :-
    datetime(Dt, MJD, Ns),
    form_time(Ts, NotDt),
    datetime(NotDt, NotMJD, NotNs),
    copy_term(NotMJD, MJD, MJDGs),
    copy_term(NotNs, Ns, NsGs),
    andTerms(MJDGs, DayG-_DayOther),
    andTerms(NsGs, TimeG-_TimeOther),
    G1 = clpfd:( (DayG) #==> #\ (TimeG) ),
    G2 = clpfd:( (TimeG) #==> #\ (DayG) ),
    call(G1),
    call(G2),
    !.

andTerm(clpfd:(T), V0-O, V-O) :-
    clpfd:(V) = clpfd:(T #/\ V0).
andTerm(T, V0-O, V0-[T|O]).

andTerms(Ts, G-Other) :- foldl(andTerm, Ts, (A #\= A)-[], G-Other).

% Building the schedule

viable_time(Constraints, Dt) :-
    form_time(Constraints, Dt),
    form_time(_:00:00, Dt),
    julian:datetime(Dt, MJD, Nanos),
    labeling([leftmost,up,bisect], [MJD]),
    labeling([leftmost,up,bisect], [Nanos]).

all_viable_times(Constraints, Ds) :-
    findall(D, viable_time(Constraints, D), Ds_),
    sort(Ds_, Ds).

rfc_time(D, S) :-
    form_time(rfc3339(RfcCodes), D),
    string_codes(S, RfcCodes), !.

%%  format_range(+Ds, -Fds) is det.
% When Ds is a sorted list of datetimes, Fds is a list of nicely-formatted
% range strings
datetimes_format(Ds, Fds) :-
    % Cut to just get the most-collapsed ranges
    phrase(collapsed(Colled), Ds), !,
    maplist(format_range, Colled, Fds).

% TODO: if Ns1 = Ns2, just say time
format_range(range(MJD, Ns1, Ns2), S) :-
    datetime(StartDt, MJD, Ns1),
    datetime(EndDt, MJD, Ns2),
    form_time(unix(EpochStart), StartDt),
    form_time(unix(EpochEnd), EndDt),
    stamp_date_time(EpochStart, DateTimeStart, 'UTC'),
    stamp_date_time(EpochEnd, DateTimeEnd, 'UTC'),
    format_time(string(Start), '%a %b %d, %H:00', DateTimeStart),
    format_time(string(End), '-%H:00', DateTimeEnd),
    string_concat(Start, End, S).

adjacent_dt(MJD-Ns1, Ns2) -->
    [datetime(MJD, Ns2)],
    { Ns2 #= Ns1 + 60 * 60 * 1_000_000_000 }.

adjacent_seq(MJD-Ns1, NsFinal) -->
    adjacent_dt(MJD-Ns1, Ns2),
    adjacent_seq(MJD-Ns2, NsFinal).
adjacent_seq(_-Ns1, Ns1) --> [].

collapsed([]) --> [].
collapsed([datetime(MJD, Ns)]) --> [range(MJD, Ns, Ns)].
collapsed([range(MJD, Ns1, Ns2)|Rs]) -->
    [datetime(MJD, Ns1)],
    adjacent_seq(MJD-Ns1, Ns2),
    collapsed(Rs).
