:- use_module(library(plunit)).
:- use_module(uuid, [random_uuid/1, uuid_atom/2]).

:- begin_tests(uuid).

test(generate_uuid) :-
    random_uuid(UUID),
    uuid(Hi, Lo) = UUID,
    integer(Hi), integer(Lo).

test(unparse_uuid) :-
    UUID = uuid(-5290345715462026334, -6664041822776228046),
    uuid_atom(UUID, Atom), !,
    ground(Atom),
    Atom = 'b694ea99-eae0-4ba2-a384-914511138732'.

test(parse_uuid) :-
    Atom = '9b5ee764-c1a2-4b23-9ed5-ca8644863c79',
    uuid_atom(UUID, Atom), !,
    ground(UUID),
    UUID = uuid(-7251103930088535261, -7001467367653491591).

test(parse_uuid_2) :-
    Atom = '7bc324d7-0b50-49bb-989f-64d6d65a2483',
    uuid_atom(UUID, Atom), !,
    ground(UUID), UUID = uuid(8918012193150093755, -7449124384765500285).
:- end_tests(uuid).
