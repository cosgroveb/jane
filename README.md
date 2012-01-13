#jane

##Prerequisites

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
./rebar get-deps  clean compile generate force=1
```

You can now run jane using the generated startup script:

```
./rel/jane/bin/jane console
Erlang R15B (erts-5.9) [source] [64-bit] [smp:8:8] [async-threads:5] [hipe] [kernel-poll:true]
Eshell V5.9  (abort with ^G)
(jane@127.0.0.1)1> 
joining

```

Or, you can use ./run_dev_console to start jane in development mode. Note, that if you use sync:go(), jane's source code will be automatically reloaded as you save .erl files during development.

```
./rel/jane/bin/jane console
Erlang R15B (erts-5.9) [source] [64-bit] [smp:8:8] [async-threads:5] [hipe] [kernel-poll:true]
Eshell V5.9  (abort with ^G)
1> application:start(jane).
ok
2> sync:go().
Starting Sync (Automatic Code Compiler / Reloader)
Scanning source files...
ok
3>
```

Now you can chat with **jane** on your XMPP server!

##Description

**jane** is an Erlang XMPP we're using to teach ourselves about Erlang and OTP.

**jane** hangs out in Multi-User Chat and responds to pre-defined commands. Right now, all it does is execute shell commands and return the result in an XMPP message.

The commands it responds to are defined in command.erl.

##Contributors

cosgroveb (https://github.com/cosgroveb)

plainlystated (https://github.com/plainlystated)

benmills (https://github.com/benmills)

