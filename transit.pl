:- module(transit, [transit//1, transit_bytes/2]).
:- use_module(library(msgpack), [msgpack//1]).
:- use_module(library(assoc), [is_assoc/1, assoc_to_list/2, list_to_assoc/2]).

% Is there some way to unify these two rules? Seems kind of silly to
% have them both...
transit(keyword(K)) -->
    [str(S)],
    { string(S), string_concat("~:", Ks, S), atom_string(K, Ks) },
    !.
transit(keyword(K)) -->
    { atom(K), atom_string(K, Ks), string_concat("~:", Ks, S) },
    [str(S)],
    !.
transit(S) --> { string(S) }, [str(S)], !.
transit(As) -->
    { is_assoc(As),
      assoc_to_list(As, Pairs),
      maplist(convert_pairs, Pairs, Ms) },
    [dict(Ms)], !.
transit(time(T)) -->
    { ground(T),
      date_time_stamp(T, Ts),
      TsM is integer(Ts * 1000) },
    [list([str("~#m"), TsM])], !.
transit(time(T)) -->
    [list([str("~#m"), TsM])],
    { Ts is TsM / 1000,
      stamp_date_time(Ts, T, 'UTC') },
    !.
transit(uuid(Hi, Lo)) -->
    [list([str("~#u"), list([Hi, Lo])])].
transit(list(L)) -->
    [list([str("~#list"), list(Ll)])],
    { maplist(convert, L, Ll) } .
transit(As) -->
    [dict(Ms)],
    { maplist(convert_pairs, Ps, Ms),
      list_to_assoc(Ps, As) }, !.
transit(Tl) -->
    [list(L)],
    { maplist(convert, Tl, L) }, !.
transit(S) --> { var(S) }, [str(S)], !.
transit(T) --> [T].


convert(Ts, Msg) :- phrase(transit(Ts), [Msg]).

convert_pairs(Tk-Tv, Mk-Mv) :-
    phrase(transit(Tk), [Mk]),
    phrase(transit(Tv), [Mv]).

transit_bytes(T, Bs) :-
    ground(Bs),
    phrase(msgpack(M), Bs),
    phrase(transit(T), [M]).
transit_bytes(T, Bs) :-
    ground(T),
    phrase(transit(T), [M]),
    !, phrase(msgpack(M), Bs).

/*
:- use_module(libraryy(msgpack)).
?- phrase_from_file(msgpack(D), 'test.msgpack', [type(binary)]),
   phrase(transit(T), [D]),
   format('READ ~w~n', [T]),
   get_assoc(keyword(content), T, Content), format('CONTENT ~w~n', [Content]),
   get_assoc(keyword(id), T, Id), format('ID ~w~n', [Id]),
   assoc_to_keys(T, Keys), format('Keys ~w~n', [Keys]),
   phrase(transit(T), [Out]), format('OUT ~w~n', [Out]),
   phrase(msgpack(Out), Bytes), format('BYTES ~w~n', [Bytes]).
*/
