:- module(server, [run/1]).
:- use_module(messages, [handle_message/1]).
:- use_module(persist, [attach_threads_db/1]).
:- use_module(transit, [transit_bytes/2]).
:- use_module(library(http/http_client), [http_read_data/3]).
:- use_module(library(http/http_dispatch), [http_dispatch/1, http_handler/3]).
:- use_module(library(http/http_log), []).
:- use_module(library(http/thread_httpd), [http_server/2]).
:- use_module(library(settings), [setting/4, setting/2, load_settings/1]).
:- use_module(library(sha), [hmac_sha/4]).
:- use_module(library(thread_pool), [thread_create_in_pool/4,
                                     thread_pool_create/3]).

% Settings
:- setting(bot_id, atom, '00000000-0000-4000-0000-000000000000', 'Braid bot ID').
:- setting(bot_token, atom, 'sthasthsnthsnthsnthsth', 'Braid bot token').
:- setting(bot_name, string, "/schedule", 'Name of the bot on the Braid server including leading /').
:- setting(braid_api_url, atom, 'http://localhost:5557', 'Braid API URL').
:- setting(bot_port, integer, 8080, 'Default port to run on').
:- setting(bot_db_file, atom, '../schedule_data', 'Path to database file').

config_file(F) :- getenv('CONFIG_FILE', F), !.
config_file('../config.pl').

% Main
main :-
    config_file(Conf),
    load_settings(Conf),
    setting(bot_port, Port),
    run(Port).
run(Port) :-
    config_file(Conf),
    load_settings(Conf),
    setting(bot_db_file, DbFile),
    attach_threads_db(DbFile),
    http_server(http_dispatch, [port(Port)]).

% Thread pool for message handler lazy creation
:- multifile thread_pool:create_pool/1.

thread_pool:create_pool(message_handler_pool) :-
    thread_pool_create(message_handler_pool, 10, []).

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
    member(method(Meth), Request), member(Meth, [put, post]), !,
    http_read_data(Request, Data, [to(codes)]),

    signaturechk(Request, Data), !,
    % Parse transit
    transit_bytes(Info, Data),

    format('Content-type: text/plain~n~n'),
    format('ok.'),

    thread_create_in_pool(message_handler_pool, handle_message(Info), _,
                          [detached(true)]).
braid_msg_handler(Request) :-
    memberchk(path(Path), Request),
    throw(http_reply(not_found(Path))).
