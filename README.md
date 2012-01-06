# jane

## Prerequisites

* erlang (if you don't have it, you should be able to simply "brew install erlang")
* exmpp (get it from https://github.com/processone/exmpp)

##Installation

Move the example app.src file.

```
cp src/jane.app.src.example src/jane.app.src
```

Edit the connection information for your XMPP environment. I find it helpful to have a local instance of ejabberd running for development.

```
vim src/jane.app.src
```

Compile using **rebar**.

```
./rebar -v clean compile generate force=1
```

You can now run jane

```
./rel/jane/bin/jane console
Erlang R15B (erts-5.9) [source] [64-bit] [smp:8:8] [async-threads:5] [hipe] [kernel-poll:true]
Eshell V5.9  (abort with ^G)
(jane@127.0.0.1)1> jane_server:join_chat().
joining
```

Now you can chat with **jane** on your XMPP server!

## Description

**jane** is an Erlang XMPP bot I'm working on to teach myself the basics of Erlang and OTP.

**jane** hangs out in Multi-User Chat and responds to pre-defined commands. Right now, all it does is execute shell commands and return the result in an XMPP message.

The commands it responds to are defined in jane.app.src (this needs to go into a separate configuration file). In the commands tuple, the format is {"alias","command args"} where the alias is the command you send to jane in a chat message like:

>me: jane date
>jane: Mon Jan  2 23:53:39 CST 2012

jane will also interpolate some things from XMPP, prefixed by a dollar sign ($). Currently, that is just the alias of the message sender. In jane.app.src you can define the command tuple like the following:

>{"hi", "echo hello $sender"}.

jane will respond like so:

>me: hi jane

>jane: hello Brian

One final thing you can do is over-ride the output of a command. By default, you'll get the result of the shell command from stdin. If you do something like the following in jane.app.src

{"howdy", "echo 'you will not see this output", "howdy $sender"}

You'll get:

>me: howdy jane

>jane: howdy Brian

Interpolation won't affect things that aren't defined, that you might expect to have in your environment. For instance, you can add *{"where are your bins?", "echo $PATH"}* to jane.app.src, and jane will respond by exactly how you expect.

##Contributors

cosgroveb (https://github.com/cosgroveb)

plainlystated (https://github.com/plainlystated)

benmills (https://github.com/benmills)
