:- module(server, [run/1]).
:- use_module(transit).
:- use_module(uuid).
:- use_module(library(assoc)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_log)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(settings)).
:- use_module(library(sha)).
:- use_module(library(thread_pool)).
:- use_module(library(uri)).

% Settings
:- setting(bot_id, atom, '00000000-0000-4000-0000-000000000000', 'Braid bot ID').
:- setting(bot_token, atom, 'sthasthsnthsnthsnthsth', 'Braid bot token').
:- setting(braid_api_url, atom, 'http://localhost:5557', 'Braid API URL').

% Main
run(Port) :-
    load_settings('config.pl'),
    http_server(http_dispatch, [port(Port)]).

% Thread pool for message handler lazy creation
:- multifile thread_pool:create_pool/1.

thread_pool:create_pool(message_handler) :-
    thread_pool_create(message_handler, 10, []).

% HTTP Routes
:- multifile http:location/3.
:- dynamic http:location/3.

http:location(braid, '/braid', []).

:- http_handler(braid(message), braid_msg_handler, []).

:- multifile prolog:message//1.

prolog:message(bad_signature) -->
    ['Bad Braid Signature'-[]].

% helper to verify message signature
signaturechk(Request, Body) :-
    % Check signature
    member(x_braid_signature(Sig), Request),
    setting(bot_token, BotToken),
    hmac_sha(BotToken, Body, Hmac, [algorithm(sha256)]),
    hash_atom(Hmac, HmacHex),
    HmacHex = Sig.
signaturechk(_, _) :-
    !, throw(http_reply(bad_request(bad_signature))).

braid_msg_handler(Request) :-
    member(method(put), Request), !,
    http_read_data(Request, Data, [to(codes)]),

    signaturechk(Request, Data), !,
    % Parse transit
    transit_bytes(Info, Data),

    format('Content-type: text/plain~n~n'),
    format('ok.'),

    thread_create_in_pool(message_handler, handle_message(Info), _,
                          [detached(true)]).
braid_msg_handler(Request) :-
    memberchk(path(Path), Request),
    throw(http_reply(not_found(Path))).

% Handle message

handle_message(Msg) :-
    reply_to(Msg, "Hi there!", Reply_),
    get_assoc(keyword('user-id'), Msg, SenderID),
    put_assoc(keyword('mentioned-user-ids'), Reply_, list([SenderID]), Reply),
    debug(handler, 'Sending ~k', [Reply]),
    send_message(Reply).

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
    setting(braid_api_url, BraidURL),
    setting(bot_id, BotId),
    setting(bot_token, BotToken),
    transit_bytes(Msg, Bytes),
    atom_concat(BraidURL, '/bots/message', URL),
    http_post(URL,
              bytes('application/transit+msgpack', Bytes),
              _,
              [authorization(basic(BotId, BotToken)),
               status_code(201)]).
