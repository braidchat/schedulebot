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
:- use_module(library(uri)).

:- setting(bot_id, atom, '00000000-0000-4000-0000-000000000000', 'Braid bot ID').
:- setting(bot_token, atom, 'sthasthsnthsnthsnthsth', 'Braid bot token').
:- setting(braid_api_url, atom, 'http://localhost:5557', 'Braid API URL').

run(Port) :-
    load_settings('config.pl'),
    http_server(http_dispatch, [port(Port)]).


:- multifile http:location/3.
:- dynamic http:location/3.

http:location(braid, '/braid', []).

:- http_handler(braid(message), braid_msg_handler, []).

signaturechk(Request, Body) :-
    % Check signature
    member(x_braid_signature(Sig), Request),
    setting(bot_token, BotToken),
    hmac_sha(BotToken, Body, Hmac, [algorithm(sha256)]),
    hash_atom(Hmac, HmacHex),
    HmacHex = Sig.
signaturechk(_) :-
    !, throw(http_reply(bad_request('Bad Signature'))).

braid_msg_handler(Request) :-
    member(method(put), Request), !,
    http_read_data(Request, Data, [to(codes)]),

    signaturechk(Request, Data),
    % Parse transit
    transit_bytes(Info, Data),

    format('Content-type: text/plain~n~n'),
    format('ok.'),

    handle_message(Info).
    %% thread_create(handle_message(Info), _, [debug(true)]).

handle_message(Msg) :-
    reply_to(Msg, "Hi there!", Reply_),
    get_assoc(keyword('user-id'), Msg, SenderID),
    put_assoc(keyword('mentioned-user-ids'), Reply_, list([SenderID]), Reply),
    debug(handler, 'Sending ~k', [Reply]),
    send_message(Reply).

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
    % XXX: http_post considers a 201 response an error, so try to
    % catch that
    catch(
        http_post(URL,
                  bytes('application/transit+msgpack', Bytes),
                  _,
                  [authorization(basic(BotId, BotToken))]),
        error(_, context(_, status(201,_))),
        true).
