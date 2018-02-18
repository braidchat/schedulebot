:- module(transit, [transit//1]).
:- use_module(library(msgpack)).
:- use_module(library(assoc)).

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
transit(S) --> [str(S)], !.
transit(time(T)) -->
    [list([str("~#m"), TsM])],
    { Ts is TsM / 1000,
      stamp_date_time(Ts, T, 'UTC') },
    !.
transit(uuid(Hi, Lo)) -->
    [list([str("~#u"), list([Hi, Lo])])].
transit(list(L)) -->
    [list([str("~#list"), list(L)])].
transit(As) -->
    [dict(Ms)],
    { maplist(convert_pairs, Ps, Ms),
      list_to_assoc(Ps, As) }, !.
transit(Tl) -->
    [list(L)],
    { maplist(convert, Tl, L) }, !.
transit(T) --> [T].


convert(Ts, Msg) :- phrase(transit(Ts), [Msg]).

convert_pairs(Tk-Tv, Mk-Mv) :-
    phrase(transit(Tk), [Mk]),
    phrase(transit(Tv), [Mv]).

?- phrase_from_file(msgpack(D), 'test.msgpack', [type(binary)]),
   phrase(transit(T), [D]),
   format('READ ~w~n', [T]),
   get_assoc(keyword(content), T, Content),
   format('CONTENT ~w~n', [Content]),
   phrase(transit(T), [Out]),
   format('OUT ~w~n', [Out]) .
