# Simple Scheduling in Braid

This is a bot/external extension for [Braid](https://github.com/braidchat/braid).

It is written in Prolog, assuming [SWI-Prolog](http://swi-prolog.org/).

To run it in development, you'll need to install the `msgpack`, `julian`, and `list_util` packages.
You can do that by running `pack_install(msgpack), pack_install(julian), pack_install(list_util).` in the REPL (actually, `julian` depends on `list_util`, so you shouldn't need to explicitally install that).

Demo of usage:

<iframe width="560" height="315" src="https://www.youtube.com/embed/n9MBrBrhLwQ" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>
