:- use_module(library(plunit)).
:- use_module(library(clpfd)).
:- use_module(parsing).

:- begin_tests(parsing).

test(parse_normal_daytime_1) :-
    string_codes("Monday at 14:00", Cs),
    phrase(english_form(F), Cs),
    !, F = [dow(monday), hours([14])].

test(parse_normal_daytime_2) :-
    string_codes("Monday at 2pm", Cs),
    phrase(english_form(F), Cs),
    !, F = [dow(monday), hours([14])].

test(parse_normal_daytime_3) :-
    string_codes("Monday at 7 am", Cs),
    phrase(english_form(F), Cs),
    !, F = [dow(monday), hours([7])].

test(multiple_hours) :-
    string_codes("Monday at 7am, 10am, or 6pm", Cs),
    phrase(english_form(F), Cs),
    !, F = [dow(monday), hours([7, 10, 18])].

test(hour_range) :-
    string_codes("wednesday 10-16", Cs),
    phrase(english_form(F), Cs),
    !, F =[dow(wednesday), hours([10..16])].

test(hour_range_am) :-
    string_codes("wednesday 9 to 3", Cs),
    phrase(english_form(F), Cs),
    !, F =[dow(wednesday), hours([9..15])].

:- end_tests(parsing).
