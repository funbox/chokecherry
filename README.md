# Chokecherry

[![Build Status](https://travis-ci.org/funbox/chokecherry.svg?branch=master)](https://travis-ci.org/funbox/chokecherry)

Wrapper around **lager** logger which limits the volume of **info** messages irrespectively of the lager's backend.

The calls `chokecherry:info`, `chokecherry:warning`, `chokecherry:error` are getting translated into 
the `lager:info`, `lager:warning`, `lager:error`, retaining the proper arity.

There are two ways to log out the original module's name and the invocation process' Pid:

1. Use the compiler option right inside the file which uses the `chockecherry`:

   ```erlang
   -compile([{parse_transform, chokecherry_transform}]).
   ```

   **Important**: that line should precede this one:

   ```erlang
   -compile([{parse_transform, lager_transform}]).
   ```

2. Use that compiler option in the global `rebar.config` for the project:

   ```erlang
   {erl_opts, [
       % ...
       {parse_transform, chokecherry_transform},
       {parse_transform, lager_transform}
       % ...
   }.
   ```

   **Important**: the `chokecherry_transform` should precede the `lager_transform`.

## Configuration

This application can be somewhat customized by redefining the following settings:

- the queue length for the shaper;
- timeout for the shaper.

Default settings are as follows:

```erlang
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

`shaper` accumulates incoming messages in the queue. If the queue size exceeds `log_queue_capacity` 
within a certain time period (1 second), it sends an `error_report` “chokecherry dropped N messages in the last second”,
and drops messages from the end of the queue, while receiving new ones and maintaining the maximum size of the queue.

`writer` pulls messages from `shaper` and transmits them to `lager`.


## Changelog

### 0.2.8

1. Simplify logic in `chokecherry_shaper` and `chokecherry_writer`.
2. Configuration parameter `{writer, [{timeout, 200}]` doesn't need anymore.

### 0.2.7

1. Added `chokecherry_shaper_logger` shaped messages `gen_event` manager.

[![Sponsored by FunBox](https://funbox.ru/badges/sponsored_by_funbox_centered.svg)](https://funbox.ru)
