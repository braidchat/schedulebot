:- module(english_dates, [datetime_range//1,
                          datetime_ranges//1,
                          availability//1]).
% very heavily inspired by julian_lang_en; just rolling my own because
% I want to do some things a little differently, but see
% https://github.com/mndrix/julian_lang_en/blob/master/prolog/julian/lang/en.pl
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics), [integer//1, string//1]).
:- use_module(library(julian/calendar/gregorian), [gregorian/3, month_number/2]).
:- use_module(library(julian/util), [dow_number/2]).

dow_alternate(mon, monday).
dow_alternate(tue, tuesday).
dow_alternate(wed, wednesday).
dow_alternate(thu, thursday).
dow_alternate(thur, thursday).
dow_alternate(fri, friday).
dow_alternate(sat, saturday).
dow_alternate(sun, sunday).
dow_alternate(mondays, monday).
dow_alternate(tuesdays, tuesday).
dow_alternate(wednesdays, wednesday).
dow_alternate(thursdays, thursday).
dow_alternate(thursdays, thursday).
dow_alternate(fridays, friday).
dow_alternate(saturdays, saturday).
dow_alternate(sundays, sunday).
dow_alternate(X, X).

codes_dow(Codes, Day) :-
    atom_codes(Atom, Codes),
    downcase_atom(Atom, Day_),
    dow_alternate(Day_, Day),
    dow_number(Day, _).

month_shortform(jan, january).
month_shortform(feb, february).
month_shortform(mar, march).
month_shortform(apr, april).
month_shortform(jun, june).
month_shortform(jul, july).
month_shortform(aug, august).
month_shortform(sep, september).
month_shortform(sept, september).
month_shortform(oct, october).
month_shortform(nov, november).
month_shortform(dec, december).
month_shortform(X, X).

codes_month(Codes, M) :-
    atom_codes(Atom, Codes),
    downcase_atom(Atom, Month_),
    month_shortform(Month_, Month),
    month_number(Month, M).

maybe(S) --> S.
maybe(_) --> "".

star(S) --> S, star(S).
star(_) --> "".

comma --> " and ".
comma --> " or ".
comma --> ", ".
comma --> ",".
comma --> " ".

range --> "-".
range --> " to ".

% TODO: need to have the ability to specify a date
day(dow(Day)) -->
    string(Word),
    { codes_dow(Word, Day) }.

month(month(M)) -->
    string(Word),
    { codes_month(Word, M) }.

% don't allow empty case -- needs to be at least one day
days(dow([D])) --> day(dow(D)).
days(dow([D|Ds])) -->
    day(dow(D)), star(comma), days(dow(Ds)).

date(dow(D)) --> days(dow(D)).
date(gregorian(_,M, D)) -->
    maybe(days(dow(_))), maybe(comma),
    month(month(M)),
    comma, maybe("the"), maybe(comma),
    integer(D), maybe("th"), maybe("st").

next_hour([]) --> "".
next_hour(Hs) --> star(comma), hours(hours(Hs)).

hour_range(Start, End_, Rng) :-
    End_ =< Start,
    End is End_ + 12,
    Rng = Start..End.
hour_range(Start, End, Start..End).

hour(H) -->
    integer(H), maybe(" "), "am".
hour(H) -->
    integer(N), maybe(" "), "pm",
    { H is N + 12 }.
hour(H) -->
    integer(H), ":00".
hour(H) -->
    integer(H).

hours(hours([H|Hs])) -->
    hour(A),
    star(" "), range, star(" "),
    hour(B),
    { hour_range(A, B, Rng),
      H = Rng },
    !, next_hour(Hs).
hours(hours([H|Hs])) -->
    hour(H), next_hour(Hs).

datetime_range(day_at(true, hours(Hs))) -->
    "any day", maybe(comma), hours(hours(Hs)).
datetime_range(day_at(Date, hours(Hs))) -->
    date(Date),
    comma, maybe("at"), maybe(comma),
    hours(hours(Hs)).
datetime_range(day_at(dow(Days), true)) -->
    maybe("any time "),
    days(dow(Days)), maybe(" any time"), maybe(" whenever").

datetime_ranges(one_of([])) --> "".
datetime_ranges(one_of([Dr|Drs])) -->
    datetime_range(Dr),
    star(comma),
    datetime_ranges(one_of(Drs)).

availability(NotTs) -->
    "can't ", maybe("do "), datetime_ranges(one_of(Ts)),
    { maplist([T, Nt]>> =(Nt, not(T)), Ts, NotTs) }.
availability(Ts) -->
    maybe("can "), maybe("do "), datetime_ranges(Ts).
