:- module(english_dates, [datetime_range//1,
                          datetime_ranges//1,
                          availability//1]).
% very heavily inspired by julian_lang_en; just rolling my own because
% I want to do some things a little differently, but see
% https://github.com/mndrix/julian_lang_en/blob/master/prolog/julian/lang/en.pl
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics), [integer//1, string//1]).
:- use_module(library(julian/util), [dow_number/2]).

% True if Day is an atom representing the day of week named in Codes
codes_dow(Codes, Day) :-
    atom_codes(Atom, Codes),
    downcase_atom(Atom, Day),
    dow_number(Day, _).

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

% don't allow empty case -- needs to be at least one day
days(dow([D])) --> day(dow(D)).
days(dow([D|Ds])) -->
    day(dow(D)), star(comma), days(dow(Ds)).

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
datetime_range(day_at(dow(Day), hours(Hs))) -->
    days(dow(Day)),
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
    "can ", maybe("do "), datetime_ranges(Ts).
