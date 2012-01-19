#jane

A simple chat bot written in erlang.

***

##Prerequisites

* **erlang**: if you don't have it, you should be able to simply `brew install
  erlang`
  * **exmpp**:  get it from https://github.com/processone/exmpp

##Building Jane

**1.** Copy `rel/files/sys.config` into the root of project as `test.config`
and update the XMPP environment variables to match the server you're connecting
too.

```bash
cp rel/files/sys.config test.config
```
*Note*: *this file is ignored because it to contain senstive information*.

**2.** Compile using `rebar`.

```
./rebar get-deps clean compile
```

**3.** Start jane in development console. This starts an erlang console,
provides the correct paths to the relevent beam files and runs `jane:start()`.

```
./run_dev_console
```
*Note, that if you use sync:go(), jane's source code will be automatically
reloaded as you save .erl files during development.*


Now you can chat with **jane** on your XMPP server!

***

##Contributors

cosgroveb (https://github.com/cosgroveb)

plainlystated (https://github.com/plainlystated)

benmills (https://github.com/benmills)
