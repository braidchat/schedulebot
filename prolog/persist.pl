:- module(persist, [attach_threads_db/1,
                    add_thread/3,
                    thread_constraints/2,
                    add_thread_constraints/2,
                    watched_thread/1]).
:- use_module(library(persistency)).

:- persistent
   schedule_threads(
       % uuid
       thread_id:any,
       % list of uuids
       users:list),
   schedule_constraints(
       % uuid
       thread_id:any,
       % list of constraints
       constraints:any).


attach_threads_db(File) :-
    db_attach(File, []).

add_thread(ThreadId, Users, InitialConstraints) :-
    assert_schedule_threads(ThreadId, Users),
    assert_schedule_constraints(ThreadId, InitialConstraints),
    db_sync(_).

watched_thread(ThreadId) :-
    schedule_threads(ThreadId, _).

thread_constraints(ThreadId, OverallConstraints) :-
    with_mutex(persist,
               findall(UserConstraints,
                       schedule_constraints(ThreadId, UserConstraints),
                       OverallConstraints)).

add_thread_constraints(ThreadId, NewConstraints) :-
    assert_schedule_constraints(ThreadId, NewConstraints).
