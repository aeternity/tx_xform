tx_xform
=====

A rebar plugin for finding aec_tx behavior modules and generating a
callback selector module for them.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

```
{plugins,
  [{tx_xform, {git, "https://github.com/aeternity/tx_xform.git", "master"}}]}.
```

Specify the name of the generated source file (a relative file name where the
top level is the application.)

```
{tx_xform_opts,
  [{out_file, "aecore/src/txs/tx_handlers.erl"}]}.
```

In order to run the transform after compile (the transform operates on the compiled
beam files), add a provider post-hook:

```
{provider_hooks,
  [{post, [{compile, {tx_xform, generate}}]}
```

Then just call your plugin directly in an existing application:


    $ rebar3 tx_xform generate
    ===> Fetching tx_xform
    ===> Compiling tx_xform
    <Plugin Output>
