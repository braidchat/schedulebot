:- use_module(library(plunit)).
:- use_module(library(clpfd)).
:- use_module(english_dates).

:- begin_tests(english_dates).

% Single datetime
test(parse_normal_daytime_1) :-
    string_codes("Monday at 14:00", Cs),
    phrase(datetime_range(F), Cs),
    !, F = [dow([monday]), hours([14])].

test(parse_normal_daytime_2) :-
    string_codes("Monday at 2pm", Cs),
    phrase(datetime_range(F), Cs),
    !, F = [dow([monday]), hours([14])].

test(parse_normal_daytime_3) :-
    string_codes("Monday at 7 am", Cs),
    phrase(datetime_range(F), Cs),
    !, F = [dow([monday]), hours([7])].

test(multiple_hours) :-
    string_codes("Monday at 7am, 10am, or 6pm", Cs),
    phrase(datetime_range(F), Cs),
    !, F = [dow([monday]), hours([7, 10, 18])].

test(hour_range) :-
    string_codes("wednesday 10-16", Cs),
    phrase(datetime_range(F), Cs),
    !, F = [dow([wednesday]), hours([10..16])].

test(hour_range_am) :-
    string_codes("wednesday 9 to 3", Cs),
    phrase(datetime_range(F), Cs),
    !, F = [dow([wednesday]), hours([9..15])].

% multiple dates & times

test(multiple_days_and_hours) :-
    string_codes("Monday at 7am, 10am, or 6pm, Tuesday 9-11", Cs),
    phrase(datetime_ranges(F), Cs),
    !, F = [[dow([monday]), hours([7, 10, 18])],
            [dow([tuesday]), hours([9..11])]].

test(multiple_days_group_and_hours) :-
    string_codes("Monday or wednesday at 7am, 10am, or 6pm, Tuesday 9-11", Cs),
    phrase(datetime_ranges(F), Cs),
    !, F = [[dow([monday,wednesday]), hours([7, 10, 18])],
            [dow([tuesday]), hours([9..11])]].

% testing overall availability

test(can_do_times) :-
    string_codes("can do Monday or wednesday at 7am, 10am, or 6pm, Tuesday 9-11", Cs),
    phrase(availability(F), Cs),
    !, F = [[dow([monday,wednesday]), hours([7, 10, 18])],
            [dow([tuesday]), hours([9..11])]].

test(cant_do_times) :-
    string_codes("can't do Monday or wednesday at 7am, 10am, or 6pm, Tuesday 9-11", Cs),
    phrase(availability(F), Cs),
    !, F = not([[dow([monday,wednesday]), hours([7, 10, 18])],
                [dow([tuesday]), hours([9..11])]]).

:- end_tests(english_dates).
