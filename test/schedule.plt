:- use_module(library(plunit)).
:- use_module(library(clpfd)).
:- use_module(schedule).

:- begin_tests(schedule).

test(specify_times) :-
    schedule:all_viable_times(
                 [after(2018-02-21), before(2018-02-24),
                  [dow([thursday,friday]), hours([14..16])]],
                 Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-02-22T14:00:00",
        "2018-02-22T15:00:00",
        "2018-02-22T16:00:00",
        "2018-02-23T14:00:00",
        "2018-02-23T15:00:00",
        "2018-02-23T16:00:00"
    ].

test(specify_times_2) :-
    schedule:all_viable_times(
                 [after(2018-02-21), before(2018-02-24),
                  [dow([thursday,friday]), hours([14..16])],
                  [dow([friday,saturday]), hours([15..18])]],
                 Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-02-23T15:00:00",
        "2018-02-23T16:00:00"
    ].

test(specify_times_and_exclude) :-
    schedule:all_viable_times(
                 [after(2018-02-21), before(2018-02-24),
                  [dow([thursday,friday]), hours([14..16])],
                  [dow([thursday,friday,saturday]), hours([15..18])],
                  not([2018-02-22, hours([16])])
                 ],
                 Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-02-22T15:00:00",
        "2018-02-23T15:00:00",
        "2018-02-23T16:00:00"
    ].

test(specify_times_and_exclude_dow) :-
    schedule:all_viable_times(
                 [after(2018-02-21), before(2018-02-24),
                  [dow([thursday,friday]), hours([14..16])],
                  [dow([thursday,friday,saturday]), hours([15..18])],
                  not([dow(thursday), hours([16])])
                 ],
                 Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-02-22T15:00:00",
        "2018-02-23T15:00:00",
        "2018-02-23T16:00:00"
    ].

test(specify_times_and_exclude_dows) :-
    schedule:all_viable_times(
                 [after(2018-02-21), before(2018-02-24),
                  [dow([thursday,friday]), hours([14..16])],
                  [dow([thursday,friday,saturday]), hours([15..18])],
                  not([dow([thursday, friday]), hours([16])])
                 ],
                 Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-02-22T15:00:00",
        "2018-02-23T15:00:00"
    ].

test(complicated_schedule) :-
    schedule:all_viable_times(
                 [after(2018-02-21), before(2018-02-24),
                  not(2018-02-22),
                  hours([11..18]),
                  not(hours([12..13, 17])),
                  not(dow(friday)),
                  not([2018-02-24, hours([12..17])])
                 ],
                 Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-02-21T11:00:00",
        "2018-02-21T14:00:00",
        "2018-02-21T15:00:00",
        "2018-02-21T16:00:00",
        "2018-02-21T18:00:00",
        "2018-02-24T11:00:00",
        "2018-02-24T18:00:00"
    ].

test(complicated_schedule2) :-
    schedule:all_viable_times(
                 [after(2018-02-21), before(2018-03-06),
                  not(2018-02-22),
                  hours([9, 10]),
                  not(dow(monday)),
                  not(dow(friday)),
                  not([2018-02-24, hours([10..17])])
                 ],
                 Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-02-21T09:00:00",
        "2018-02-21T10:00:00",
        "2018-02-24T09:00:00",
        "2018-02-25T09:00:00",
        "2018-02-25T10:00:00",
        "2018-02-27T09:00:00",
        "2018-02-27T10:00:00",
        "2018-02-28T09:00:00",
        "2018-02-28T10:00:00",

        "2018-03-01T09:00:00",
        "2018-03-01T10:00:00",
        "2018-03-03T09:00:00",
        "2018-03-03T10:00:00",
        "2018-03-04T09:00:00",
        "2018-03-04T10:00:00",
        "2018-03-06T09:00:00",
        "2018-03-06T10:00:00"
    ].

test(exclude_weekday) :-
    schedule:all_viable_times(
                 [after(2018-02-21), before(2018-02-24),
                  hours([14..16]),
                  not(dow(wednesday))
                 ],
                 Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-02-22T14:00:00",
        "2018-02-22T15:00:00",
        "2018-02-22T16:00:00",
        "2018-02-23T14:00:00",
        "2018-02-23T15:00:00",
        "2018-02-23T16:00:00",
        "2018-02-24T14:00:00",
        "2018-02-24T15:00:00",
        "2018-02-24T16:00:00"
    ].

test(exclude_multi_weekday) :-
    schedule:all_viable_times(
                 [after(2018-02-21), before(2018-02-24),
                  hours([14..16]),
                  not(dow([wednesday, saturday]))
                 ],
                 Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-02-22T14:00:00",
        "2018-02-22T15:00:00",
        "2018-02-22T16:00:00",
        "2018-02-23T14:00:00",
        "2018-02-23T15:00:00",
        "2018-02-23T16:00:00"
    ].

test(another_one) :-
    schedule:all_viable_times(
                 [[after(2018-2-26),before(2018-3-3)],
                  [[dow([tuesday,wednesday]),hours([10,14])]],
                  not([dow([wednesday]),hours([14])])],
                 Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-02-27T10:00:00",
        "2018-02-27T14:00:00",
        "2018-02-28T10:00:00"
    ].

test(limit_days_simple) :-
    Constraints = [after(2018-2-27),before(2018-3-5),
                   day_at(dow(monday), hours([9..11])),
                   dow(monday)],
    schedule:all_viable_times(Constraints, Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-03-05T09:00:00",
        "2018-03-05T10:00:00",
        "2018-03-05T11:00:00"
    ].

test(limit_days_manual) :-
    Constraints = [after(2018-2-27),before(2018-3-5),
                   day_at(dow([monday]), hours([9..11])),
                   day_at(dow([tuesday]), hours([10..13])),
                   day_at(dow([friday]), hours([9..10])),
                   dow([monday,tuesday,friday])],
    schedule:all_viable_times(Constraints, Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-02-27T10:00:00",
        "2018-02-27T11:00:00",
        "2018-02-27T12:00:00",
        "2018-02-27T13:00:00",

        "2018-03-02T09:00:00",
        "2018-03-02T10:00:00",

        "2018-03-05T09:00:00",
        "2018-03-05T10:00:00",
        "2018-03-05T11:00:00"
    ].

test(limit_days) :-
    Constraints = [[after(2018-2-27),before(2018-3-5)],
                   one_of(
                       [day_at(2018-3-5, hours([9..11])),
                        day_at(2018-2-27, hours([10..13])),
                        day_at(2018-3-2, hours([9..10]))])],
    schedule:all_viable_times(Constraints, Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-02-27T10:00:00",
        "2018-02-27T11:00:00",
        "2018-02-27T12:00:00",
        "2018-02-27T13:00:00",

        "2018-03-02T09:00:00",
        "2018-03-02T10:00:00",

        "2018-03-05T09:00:00",
        "2018-03-05T10:00:00",
        "2018-03-05T11:00:00"
    ].

test(limit_days_dow) :-
    Constraints = [[after(2018-2-27),before(2018-3-5)],
                   one_of([day_at(dow([monday]), hours([9..11])),
                           day_at(dow([tuesday]), hours([10..13])),
                           day_at(dow([friday]), hours([9..10]))])],
    schedule:all_viable_times(Constraints, Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-02-27T10:00:00",
        "2018-02-27T11:00:00",
        "2018-02-27T12:00:00",
        "2018-02-27T13:00:00",

        "2018-03-02T09:00:00",
        "2018-03-02T10:00:00",

        "2018-03-05T09:00:00",
        "2018-03-05T10:00:00",
        "2018-03-05T11:00:00"
    ].

test(one_of_day_at_multi_down) :-
    Cons = [after(2018-03-04), before(2018-03-08),
            one_of([day_at(dow([monday,wednesday]), hours([7..10])),
                    day_at(dow([tuesday]), hours([9..11]))])],
    schedule:all_viable_times(Cons, Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-03-05T07:00:00",
        "2018-03-05T08:00:00",
        "2018-03-05T09:00:00",
        "2018-03-05T10:00:00",

        "2018-03-06T09:00:00",
        "2018-03-06T10:00:00",
        "2018-03-06T11:00:00",

        "2018-03-07T07:00:00",
        "2018-03-07T08:00:00",
        "2018-03-07T09:00:00",
        "2018-03-07T10:00:00"
    ].

test(one_of_day_at_multi_not) :-
    Cons = [after(2018-03-04), before(2018-03-08),
            one_of([day_at(dow([monday,wednesday]), hours([7..10])),
                    day_at(dow([tuesday]), hours([9..11]))]),
            not(day_at(dow([tuesday]), hours([10])))],
    schedule:all_viable_times(Cons, Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-03-05T07:00:00",
        "2018-03-05T08:00:00",
        "2018-03-05T09:00:00",
        "2018-03-05T10:00:00",

        "2018-03-06T09:00:00",
        "2018-03-06T11:00:00",

        "2018-03-07T07:00:00",
        "2018-03-07T08:00:00",
        "2018-03-07T09:00:00",
        "2018-03-07T10:00:00"
    ].

test(one_of_day_at_multi_not_and_any_date) :-
    Cons = [after(2018-03-04), before(2018-03-09),
            hours([7..12]),
            one_of([day_at(dow([monday,wednesday]), hours([7..10])),
                    day_at(dow([tuesday]), hours([9..11])),
                    day_at(dow([friday]), true)]),
            not(day_at(dow([tuesday]), hours([10])))],
    schedule:all_viable_times(Cons, Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = [
        "2018-03-05T07:00:00",
        "2018-03-05T08:00:00",
        "2018-03-05T09:00:00",
        "2018-03-05T10:00:00",

        "2018-03-06T09:00:00",
        "2018-03-06T11:00:00",

        "2018-03-07T07:00:00",
        "2018-03-07T08:00:00",
        "2018-03-07T09:00:00",
        "2018-03-07T10:00:00",

        "2018-03-09T07:00:00",
        "2018-03-09T08:00:00",
        "2018-03-09T09:00:00",
        "2018-03-09T10:00:00",
        "2018-03-09T11:00:00",
        "2018-03-09T12:00:00"
    ].

test(specific_date) :-
    Cons = [after(2018-02-13), before(2018-03-15),
            [day_at(gregorian(_, 3, 14), hours([10]))]],
    schedule:all_viable_times(Cons, Ds),
    !,
    maplist(schedule:rfc_time, Ds, Rfcs),
    Rfcs = ["2018-03-14T10:00:00"].

:- end_tests(schedule).
