:- module(server, []).
:- use_module(transit).
:- use_module(library(assoc)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(settings)).
:- use_module(library(sha)).

:- setting(bot_id, atom, '00000000-0000-4000-0000-000000000000', 'Braid bot ID').
:- setting(bot_token, atom, 'sthasthsnthsnthsnthsth', 'Braid bot token').
:- setting(braid_api_url, atom, 'http://localhost:5557', 'Braid API URL').

run(Port) :-
    load_settings('config.pl'),
    http_server(http_dispatch, [port(Port)]).

:- http_handler(root(.), braid_msg_handler, []).

braid_msg_handler(Request) :-
    member(method(post), Request), !,
    http_read_data(Request, Data, [to(codes)]),

    % Check signature
    member(x_braid_signature(Sig), Request),
    setting(bot_token, BotToken),
    hmac_sha(BotToken, Data, Hmac, [algorithm(sha256)]),
    hash_atom(Hmac, HmacHex),
    HmacHex = Sig,

    % Parse transit
    transit_bytes(Info, Data),

    get_assoc(keyword(content), Info, Content),
    get_assoc(keyword('user-id'), Info, UserId),
    setting(braid_api_url, ApiURL),
    setting(bot_id, BotID),

    format('Content-type: text/plain~n~n'),
    format('Sig ~w~n', [HmacHex]),
    format('Msg ~w from ~w~n', [Content, UserId]),
    format('Sending to ~w as ~w~n', [ApiURL, BotID]).
    % verify signature
