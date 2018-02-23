:- use_module(library(plunit)).
:- use_module(library(clpfd)).
:- use_module(schedule).

:- begin_tests(schedule).

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
    maplist(schedule:rfc_time, Ds, Rfcs),
    !,
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

:- end_tests(schedule).
