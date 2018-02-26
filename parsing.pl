:- module(parsing, [english_form//1]).
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

end_of_content([], []).

comma --> " and ".
comma --> " or ".
comma --> ", ".
comma --> ",".
comma --> " ".

range --> "-".
range --> " to ".

day(dow(Day)) -->
    string(Word),
    { codes_dow(Word, Day) }.

next_hour([]) --> end_of_content.
next_hour(Hs) --> star(comma), hours(hours(Hs)).

hour_range(Start, End_, Rng) :-
    End_ =< Start,
    End is End_ + 12,
    Rng = Start..End.
hour_range(Start, End, Start..End).

hours(hours([H|Hs])) -->
    integer(H),
    maybe(" "), "am",
    !, next_hour(Hs).
hours(hours([H|Hs])) -->
    integer(N),
    maybe(" "), "pm",
    { H is N + 12 },
    !, next_hour(Hs).
hours(hours([H|Hs])) -->
    integer(H), ":00",
    !, next_hour(Hs).
hours(hours([H|Hs])) -->
    integer(A),
    star(" "), range, star(" "),
    integer(B),
    { hour_range(A, B, Rng),
      H = Rng },
    !, next_hour(Hs).

english_form(true) -->
    "any day".
english_form([dow(Day), hours(Hs)]) -->
    day(dow(Day)),
    comma, maybe("at"), maybe(comma),
    hours(hours(Hs)).
%% english_form([Ds]) -->
%%     true.
%% english_form(not(Ds)) -->
%%     english_form(Ds).


%% ?- run_tests.
