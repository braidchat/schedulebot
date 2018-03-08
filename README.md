# Simple Scheduling in Braid

This is a bot/external extension for [Braid](https://github.com/braidchat/braid).

It is written in Prolog, assuming [SWI-Prolog](http://swi-prolog.org/).

To run it in development, you'll need to install the `msgpack`, `julian`, and `list_util` packages.
You can do that by running `pack_install(msgpack), pack_install(julian), pack_install(list_util).` in the REPL (actually, `julian` depends on `list_util`, so you shouldn't need to explicitally install that).

Demo of usage:

[![Scheduler Demo](https://img.youtube.com/vi/n9MBrBrhLwQ/0.jpg)](http://www.youtube.com/watch?v=n9MBrBrhLwQ)


## Deploying

Load the `server` module and run `qsave_program(scheduler, [autoload(true), stand_alone(true), goal(server:main)]).`

This will create an executable named "scheduler" that will run the server main goal.
Note that this will also stand a prolog repl when launched.
This may not be what you want, but it is pretty useful! (there's probably a way to remove this, but I haven't tried)

Running the binary, it will expect the `config.pl` file to be in the directory above it.
Your config.pl should look like this:

```prolog
setting(server:bot_id, '5a99acf9-9aba-4791-aaba-fa47f6d86cae').
setting(server:bot_token, 'orrc1_Eqer-E4gMNq_BVY5avfxP5HseI27OCcIl4').
setting(server:bot_port, 9191).
setting(server:braid_api_url, 'https://api.braid.chat')
% You can also set bot_name if you call it something other than "schedule"
% Note that it includes the leading "/" for mentioning bots
%setting(server:bot_name, "/schedulebot")
```

The `bot_id` and `bot_token` will be the ones given to you by Braid when adding a new bot.

You will also probably need the swipl shared library to run the binary.
You can find the one that your binary is linked against by running `ldd scheduler` in a terminal and looking for `libswipl.so`.
You can then upload the `.so` to the server as well & run the server like `LD_LIBRARY_PATH=/path/to/scheduler/lib scheduler`.

I recommend setting up the directories like this:

```shell
.
├── config.pl
├── bin
│   └── scheduler
└── lib
    └── libswipl.so.7.7
```
