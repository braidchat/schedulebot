:- module(messages, [handle_message/1]).
:- use_module(english_dates, [availability//1]).
:- use_module(persist, [add_thread/3,
                        thread_constraints/2,
                        add_thread_constraints/2,
                        watched_thread/1]).
:- use_module(transit, [transit_bytes/2]).
:- use_module(uuid, [random_uuid/1, uuid_atom/2]).
:- use_module(schedule, [all_viable_times/2, datetimes_format/2]).
:- use_module(library(assoc), [list_to_assoc/2, put_assoc/4, get_assoc/3]).
:- use_module(library(dcg/basics), [integer//1]).
:- use_module(library(http/http_client), [http_post/4, http_put/4]).
:- use_module(library(settings), [setting/2]).

% Helpers for creating messages & sending to braid

reply_to(Msg, Content, Reply) :-
    random_uuid(MsgId),
    get_assoc(keyword('thread-id'), Msg, ThreadId),
    text_to_string(Content, ContentStr),
    Pairs = [keyword(id)-MsgId,
             keyword(content)-ContentStr,
             keyword('thread-id')-ThreadId,
             keyword('mentioned-tag-ids')-list([]),
             keyword('mentioned-user-ids')-list([])],
    list_to_assoc(Pairs, Reply).

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
    get_assoc(keyword('mentioned-user-ids'), Msg, Mentioned),
    get_assoc(keyword('user-id'), Msg, Sender),
    Users = [Sender|Mentioned],
    string_codes(Command, Codes),
    % Not using `phrase` for dcg, because I'm okay with trailing text
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
    get_assoc(keyword('thread-id'), Msg, ThreadId),
    watched_thread(ThreadId),
    get_assoc(keyword('content'), Msg, Content),
    Content = "times?",
    reply_schedule(ThreadId).
handle_message(Msg) :-
    get_assoc(keyword('thread-id'), Msg, ThreadId),
    watched_thread(ThreadId), !,
    % don't spam the scheduling thread
    true.
handle_message(Msg) :-
    setting(server:bot_name, BotName),
    Txts = [
        "Sorry, I don't understand. ",
        "Try something like ",
        "'", BotName, " 2018-03-10 to 2018-03-17 @person1 @person2"
    ],
    strings_join(Txts, "", Txt),
    reply_to(Msg, Txt, Reply),
    send_message(Reply).

reply_schedule(ThreadId) :-
    thread_availability(ThreadId, AvailableTimes),
    new_message(Msg__),
    put_assoc(keyword('thread-id'), Msg__, ThreadId, Msg_),
    put_assoc(keyword('content'), Msg_, AvailableTimes, Msg),
    send_message(Msg).

thread_availability(ThreadId, AvailableTimes) :-
    thread_constraints(ThreadId, Constraints),
    all_viable_times(Constraints, DateTimes),
    datetimes_format(DateTimes, FormattedTimes),
    strings_join(FormattedTimes, "\n", AvailableTimes).


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
    [after(Start), before(End)] = InitialConstraints,
    format(string(S1), 'What times work for eveyone between ~w and ~w?~n',
           [Start, End]),
    format(string(S2),
           'Reply in this thread with "can do <times>" or "can\'t do <times>"',
           []),
    format(string(S3), 'and send "times?" to see the current availability', []),
    strings_join([S1, S2, S3], "\n", S),
    put_assoc(keyword('content'), Msg_, S, Msg),
    send_message(Msg),
    subscribe_thread(NewThreadId).
