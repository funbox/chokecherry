# Chokecherry

Wrapper around **lager** logger which limits the volume of messages irrespectively of the lager's **backend**.

The calls **chokecherry:info**, **chokecherry:warning**, **chokecherry:error** are getting translated into the **lager:info**, **lager:warning**, **lager:error**, retaining the proper arity.

There are two ways to log out the original module's name and the invocation process' Pid:

* Use the compiler option right inside the file which uses the **chockecherry**:

```
-compile([{parse_transform, chokecherry_transform}]).
```

**Important**: that line should precede the

```
-compile([{parse_transform, lager_transform}]).
```

* Use that compiler option in the global `rebar.config` for the project:

```
{erl_opts, [
    % ...
    {parse_transform, chokecherry_transform},
    {parse_transform, lager_transform}
    % ...
}.
```

**Important**: the *chokecherry_transform* should precede the *lager_transform*.

## Configuration

This application can be somewhat customized by redefining the following settings:

- the queue length for the **shaper**
- timeouts for the **shaper** and **writer**

Default settings are as follows:

```
[
    {chokecherry, [
        {shaper, [
            {timeout, 1000},
            {log_queue_capacity, 10000}
        ]},
        {writer, [
            {timeout, 200}
        ]}
    ]}
].

```

## Build status

[![Build Status](https://travis-ci.org/funbox/chokecherry.svg?branch=master)](https://travis-ci.org/funbox/chokecherry)

