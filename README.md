# Simple Scheduling in Braid

This is a bot/external extension for [Braid](https://github.com/braidchat/braid).

It is written in Prolog, assuming [SWI-Prolog](http://swi-prolog.org/).

To run it in development, you'll need to install the `msgpack`, `julian`, and `list_util` packages.
You can do that by running `pack_install(msgpack), pack_install(julian), pack_install(list_util).` in the REPL (actually, `julian` depends on `list_util`, so you shouldn't need to explicitally install that).

Demo of usage:

[![Scheduler Demo](https://img.youtube.com/vi/n9MBrBrhLwQ/0.jpg)](http://www.youtube.com/watch?v=n9MBrBrhLwQ)


Compiling to deploy:

In the `prolog/` directory, run:

`swipl --stand_alone=true -o scheduler -g server:main -c server.pl -O`
