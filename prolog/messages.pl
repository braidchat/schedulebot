:- module(messages, [handle_message/1]).
:- use_module(english_dates, [availability//1]).
:- use_module(persist, [add_thread/3,
                        thread_constraints/2,
                        add_thread_constraints/2,
                        watched_thread/1]).
:- use_module(transit, [transit_bytes/2]).
:- use_module(uuid, [random_uuid/1, uuid_atom/2]).
:- use_module(schedule, [all_viable_times/2, rfc_time/2]).
:- use_module(library(assoc), [list_to_assoc/2, put_assoc/4, get_assoc/3]).
:- use_module(library(dcg/basics), [integer//1]).
:- use_module(library(http/http_client), [http_post/4, http_put/4]).
:- use_module(library(settings), [setting/2]).

% Helpers for creating messages & sending to braid

reply_to(Msg, Content, Reply) :-
    random_uuid(NewId),
    put_assoc(keyword(id), Msg, NewId, Reply_),
    put_assoc(keyword('mentioned-tag-ids'), Reply_, list([]), Reply__),
    put_assoc(keyword('mentioned-user-ids'), Reply__, list([]), Reply___),
    text_to_string(Content, ContentStr),
    put_assoc(keyword(content), Reply___, ContentStr, Reply).

new_message(Msg) :-
    random_uuid(MsgId),
    Pairs = [keyword(id)-MsgId,
             keyword(content)-"",
             keyword('thread-id')-"",
             keyword('mentioned-user-ids')-list([]),
             keyword('mentioned-tag-ids')-list([])],
    list_to_assoc(Pairs, Msg).

send_message(Msg) :-
    setting(server:braid_api_url, BraidURL),
    setting(server:bot_id, BotId),
    setting(server:bot_token, BotToken),
    transit_bytes(Msg, Bytes),
    atom_concat(BraidURL, '/bots/message', URL),
    http_post(URL,
              bytes('application/transit+msgpack', Bytes),
              _,
              [authorization(basic(BotId, BotToken)),
               status_code(201)]).

subscribe_thread(ThreadId) :-
    setting(server:braid_api_url, BraidURL),
    setting(server:bot_id, BotId),
    setting(server:bot_token, BotToken),
    uuid_atom(ThreadId, ThreadIdAtom),
    atom_concat('/bots/subscribe/', ThreadIdAtom, Path),
    atom_concat(BraidURL, Path, URL),
    http_put(URL, atom(''), _,
             [authorization(basic(BotId, BotToken)), status_code(201)]).

strings_join([], _, "").
strings_join([S], _, S).
strings_join([H|T], Sep, S) :-
    strings_join(T, Sep, Ss),
    !,
    string_concat(H, Sep, Hs),
    string_concat(Hs, Ss, S).

% Message handler

handle_message(Msg) :-
    get_assoc(keyword('content'), Msg, Content),
    setting(server:bot_name, BotName),
    string_concat(BotName, Command, Content),
    debug(handler, 'Sending command to start a new schedule ~w', [Command]),
    get_assoc(keyword('mentioned-user-ids'), Msg, Mentioned),
    get_assoc(keyword('user-id'), Msg, Sender),
    Users = [Sender|Mentioned],
    debug(handler, 'for  users ~w', [Users]),
    string_codes(Command, Codes),
    schedule_command(InitialConstraints, Codes, _),
    debug(handler, 'Starting with constraints ~w', [InitialConstraints]),
    random_uuid(NewThreadId),
    add_thread(NewThreadId, Users, InitialConstraints),
    start_schedule_thread(NewThreadId, Users, InitialConstraints).
handle_message(Msg) :-
    % For a subscribed thread
    get_assoc(keyword('thread-id'), Msg, ThreadId),
    watched_thread(ThreadId),
    get_assoc(keyword('content'), Msg, Content),
    string_codes(Content, Codes),
    phrase(availability(Constraints), Codes),
    add_thread_constraints(ThreadId, Constraints),
    reply_schedule(ThreadId).
handle_message(Msg) :-
    debug(handler, 'unknown, reply current schedule', []),
    get_assoc(keyword('thread-id'), Msg, ThreadId),
    reply_schedule(ThreadId).
    %% reply_to(Msg, "Sorry, I don't understand", Reply),
    %% send_message(Reply).

reply_schedule(ThreadId) :-
    thread_availability(ThreadId, AvailableTimes),
    debug(handler, 'available times ~w', [AvailableTimes]),
    new_message(Msg__),
    put_assoc(keyword('thread-id'), Msg__, ThreadId, Msg_),
    put_assoc(keyword('content'), Msg_, AvailableTimes, Msg),
    debug(handler, 'created new message to send ~w', [Msg]),
    send_message(Msg).

thread_availability(ThreadId, AvailableTimes) :-
    debug(handler, 'checking availability', []),
    thread_constraints(ThreadId, Constraints),
    debug(handler, 'constraints ~w', [Constraints]),
    all_viable_times(Constraints, DateTimes),
    debug(handler, 'times ~w', [DateTimes]),
    maplist(rfc_time, DateTimes, Rfcs),
    debug(handler, 'rfcs ~w', [Rfcs]),
    strings_join(Rfcs, "\n", AvailableTimes).


plus(S) --> S.
plus(S) --> S, plus(S).

schedule_command([after(Start), before(End)]) -->
    plus(" "), !,
    integer(Ys), "-", integer(Ms), "-", integer(Ds),
    " to ",
    integer(Ye), "-", integer(Me), "-", integer(De),
    { Start = Ys-Ms-Ds,
      End = Ye-Me-De }.

start_schedule_thread(NewThreadId, Users, InitialConstraints) :-
    new_message(Msg___),
    put_assoc(keyword('thread-id'), Msg___, NewThreadId, Msg__),
    put_assoc(keyword('mentioned-user-ids'), Msg__, list(Users), Msg_),
    % TODO: make this formatted in a reasonable way
    format(string(S), '~w', [InitialConstraints]),
    debug(handler, 'Starting schedule thread ~w', [S]),
    put_assoc(keyword('content'), Msg_, S, Msg),
    assoc:assoc_to_list(Msg, L), debug(handler, 'sending msg ~w', [L]),
    send_message(Msg),
    subscribe_thread(NewThreadId).

%2018-02-26 to 2018-03-03 @5a8f6cbe-d8c7-41a2-bd0e-b753f595f9c3 @5a8f6cbe-7b09-4be5-854a-1c55b4656136
