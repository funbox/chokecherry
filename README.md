# Chokecherry

[![Build Status](https://travis-ci.org/funbox/chokecherry.svg?branch=master)](https://travis-ci.org/funbox/chokecherry)

Wrapper around **lager** logger which limits the volume of **info** messages irrespectively of the lager's **backend**.

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
        ]}
    ]}
].

```

## How it works

```
+------------+     +------------+     +------------+     +------------+
|            |     |            |     |            |     |            |
|    app     +----->   shaper   +----->   writer   +----->   lager    |
|            |     |            |     |            |     |            |
+------------+     +------------+     +------------+     +------------+
```

**shaper** accumulates incoming messages in the queue. If the queue size exceeds *log_queue_capacity* within a certain time period (1 second), it sends an *error_report* "chokecherry dropped N messages in the last second", and drops messages from the end of the queue, while receiving new ones and maintaining the maximum size of the queue.

**writer** pulls messages from **shaper** and transmits them to **lager**.
