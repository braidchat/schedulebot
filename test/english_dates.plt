:- use_module(library(plunit)).
:- use_module(library(clpfd)).
:- use_module(english_dates).

:- begin_tests(english_dates).

% Single datetime
test(parse_normal_daytime_1) :-
    string_codes("Monday at 14:00", Cs),
    phrase(datetime_range(F), Cs),
    !, F = day_at(dow([monday]), hours([14])).

test(parse_normal_daytime_2) :-
    string_codes("Monday at 2pm", Cs),
    phrase(datetime_range(F), Cs),
    !, F = day_at(dow([monday]), hours([14])).

test(parse_normal_daytime_3) :-
    string_codes("Monday at 7 am", Cs),
    phrase(datetime_range(F), Cs),
    !, F = day_at(dow([monday]), hours([7])).

test(multiple_hours) :-
    string_codes("Monday at 7am, 10am, or 6pm", Cs),
    phrase(datetime_range(F), Cs),
    !, F = day_at(dow([monday]), hours([7, 10, 18])).

test(hour_range) :-
    string_codes("wednesday 10-16", Cs),
    phrase(datetime_range(F), Cs),
    !, F = day_at(dow([wednesday]), hours([10..16])).

test(hour_range_am) :-
    string_codes("wednesday 9 to 3", Cs),
    phrase(datetime_range(F), Cs),
    !, F = day_at(dow([wednesday]), hours([9..15])).

test(time_range) :-
    string_codes("monday, wednesday, or friday 10:00-18:00", Cs),
    phrase(datetime_range(F), Cs),
    !, F = day_at(dow([monday, wednesday, friday]), hours([10..18])).

test(any_time_day) :-
    string_codes("any time friday", Cs),
    phrase(datetime_range(F), Cs),
    !, F = day_at(dow([friday]), true).

test(time_any_day) :-
    string_codes("any day 9am to 3pm", Cs),
    phrase(datetime_range(F), Cs),
    !, F = day_at(true, hours([9..15])).

test(time_month) :-
    string_codes("Wednesday, March 14th at 10:00", Cs),
    phrase(datetime_range(F), Cs),
    !, F = day_at(gregorian(_, 3, 14), hours([10])).

test(time_month_short) :-
    string_codes("mon, Jul 16th at 10:00", Cs),
    phrase(datetime_range(F), Cs),
    !, F = day_at(gregorian(_, 7, 16), hours([10])).

test(time_month_without_dow) :-
    string_codes("Jul 16th at 10:00", Cs),
    phrase(datetime_range(F), Cs),
    !, F = day_at(gregorian(_, 7, 16), hours([10])).

% multiple dates & times

test(multiple_days_and_hours) :-
    string_codes("Monday at 7am, 10am, or 6pm, Tuesday 9-11", Cs),
    phrase(datetime_ranges(F), Cs),
    !, F = one_of([day_at(dow([monday]), hours([7, 10, 18])),
                   day_at(dow([tuesday]), hours([9..11]))]).

test(multiple_days_group_and_hours) :-
    string_codes("Monday or wednesday at 7am, 10am, or 6pm, Tuesday 9-11", Cs),
    phrase(datetime_ranges(F), Cs),
    !, F = one_of([day_at(dow([monday,wednesday]), hours([7, 10, 18])),
                   day_at(dow([tuesday]), hours([9..11]))]).

% testing overall availability

test(can_do_times) :-
    string_codes("can do Monday or wednesday at 7am, 10am, or 6pm, Tuesday 9-11",
                 Cs),
    phrase(availability(F), Cs),
    !, F = one_of([day_at(dow([monday,wednesday]), hours([7, 10, 18])),
                   day_at(dow([tuesday]), hours([9..11]))]).

test(can_do_with_any) :-
    string_codes("can do Monday or wednesday at 7am, 10am, or 6pm, Tuesday 9-11 or any time friday",
                 Cs),
    phrase(availability(F), Cs),
    !, F = one_of([day_at(dow([monday,wednesday]), hours([7, 10, 18])),
                   day_at(dow([tuesday]), hours([9..11])),
                   day_at(dow([friday]), true)]).

test(cant_do_times) :-
    string_codes(
        "can't do Monday or wednesday at 7am, 10am, or 6pm, Tuesday 9-11", Cs),
    phrase(availability(F), Cs),
    !, F = [not(day_at(dow([monday,wednesday]), hours([7, 10, 18]))),
            not(day_at(dow([tuesday]), hours([9..11])))].

test(time_month_without_dow_times) :-
    string_codes("can do Jul 16th at 10:00", Cs),
    phrase(availability(F), Cs),
    !, F = one_of([day_at(gregorian(_, 7, 16), hours([10]))]).

test(can_is_optional) :-
    string_codes("Jul 16th at 10:00", Cs),
    phrase(availability(F), Cs),
    !, F = one_of([day_at(gregorian(_, 7, 16), hours([10]))]).

test(real_world_attempt1) :-
    string_codes("Mon Mar 12, 10 am", Cs),
    phrase(availability(F), Cs),
    !, F = one_of([day_at(gregorian(_, 03, 12), hours([10]))]).

:- end_tests(english_dates).
