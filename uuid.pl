:- module(uuid, [random_uuid/1]).
:- use_module(library(crypto)).
:- use_module(library(clpfd)).

bytes_integer(Bs, N) :-
    foldl(pow, Bs, 0-0, N-_).

pow(B, N0-I0, N-I) :-
    B in 0..255,
    N #= N0 + B*256^I0,
    I #= I0 + 1.

unsigned64_signed64(Un, Si) :-
    integer(Un),
    Un >= 0x8000_0000_0000_0000,
    !,
    Inv is 0xffff_ffff_ffff_ffff - Un,
    Si is -Inv - 1.
unsigned64_signed64(Un, Si) :-
    integer(Si),
    Si < 0,
    !,
    Inv is -Si - 1,
    Un is 0xffff_ffff_ffff_ffff - Inv.
unsigned64_signed64(Un, Un).

random_uuid(uuid(Hi, Lo)) :-
    crypto_n_random_bytes(8, HiBytes),
    bytes_integer(HiBytes, Hi64),
    % Set version in 4 sig bits of time_hi_and_version
    Hi_ is Hi64 /\ \ (0b1111 << 12 ),
    HiUn is Hi_ \/ (4 << 12),
    unsigned64_signed64(HiUn, Hi),

    crypto_n_random_bytes(8, LoBytes),
    bytes_integer(LoBytes, Lo64),
    % Set 2 sig bits of clock_seq_hi_res to 0 & 1
    Lo_ is Lo64 /\ \ (1 << (64-6)),
    LoUn is Lo_ \/ (1 << (64-7)),
    unsigned64_signed64(LoUn, Lo).
