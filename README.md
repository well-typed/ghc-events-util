# Utilities for working with the GHC eventlog

## `show-delta`

### Introduction

This shows the time difference (delta) between events, as a way to estimate
how long events take. For example, here is a snippet from an eventlog shown
with `show-delta` without any additional arguments:

```
   0.16ms        965591  cap 0  stopping thread 5 (making a foreign call)
   ---          1121209  cap 0  running thread 5
```

The `stopping` event takes place at timestamp `965591`, and the event at
timestamp `1121209`; we can therefore estimate that the thread was stopped for
`1121209 - 965591 == 155618ns`, or roughly `0.16ms`.

For each event we compute the time from that event to the next event _on the
same capability_.

### Filtering

Suppose we have an eventlog of an application containing a server and a client,
and we are interested in the time it takes from the moment that the client
initiates a request and the handler starts the response. Assuming that we have
added the appropriate user events into the eventlog
(see [traceEventIO](https://hackage.haskell.org/package/base-4.19.1.0/docs/Debug-Trace.html#v:traceEventIO)), the eventlog might contain something like

```
   0.01ms    2155031916  cap 0  CLIENT start CREATE
   0.00ms    2155038086  cap 0  creating thread 138 "grapesy:clientInbound"
..
.. many more events
..
   ---       2195576184  cap 0  HANDLER start CREATE
```

We can pass `--match 'CLIENT|HANDLER'` to `ghc-events-util` to only show the
events we are interested in. When we do, we get an _additional_ delta which
shows the time interval between the _shown_ events only:

```
  40.54ms     0.01ms    2155031916  cap 0  CLIENT start CREATE
  ---         ---       2195576184  cap 0  HANDLER start CREATE
```

### Thread labels

We take `ThreadLabel` events into account and use them to show thread IDs
whenever possible. Moreover, when threads are created, we will look ahead
(limited by the `--max-lookahead` command line parameter) to see if we can
find a thread label for that thread. This results in snippets such as this:

```
   0.00ms    2155038086  cap 0  creating thread 138 "grapesy:clientInbound"
   0.00ms    2155044566  cap 0  running thread 138 "grapesy:clientInbound"
   0.00ms    2155045636  cap 0  thread 138 has label "grapesy:clientInbound"
   0.00ms    2155049206  cap 0  stopping thread 138 "grapesy:clientInbound" (thread yielding)
   0.00ms    2155050866  cap 0  running thread 138 "grapesy:clientInbound"
   0.00ms    2155055516  cap 0  stopping thread 138 "grapesy:clientInbound" (blocked in STM retry)
   0.00ms    2155058476  cap 0  waking up thread 138 "grapesy:clientInbound" on cap 0
...
   0.01ms    2287584148  cap 0  stopping thread 138 "grapesy:clientInbound" (thread finished)
```

### Incrementality

In order to be able to handle large eventlogs, `ghc-events-util` is carefully
written so that we process eventlogs in constant memory. For example, processing
a 10MB eventlog (with filtering) results in RTS stats

```
   1,599,063,888 bytes allocated in the heap
       5,922,936 bytes copied during GC
         210,328 bytes maximum residency (17 sample(s))
          40,032 bytes maximum slop
               7 MiB total memory in use (0 MiB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       363 colls,     0 par    0.004s   0.004s     0.0000s    0.0006s
  Gen  1        17 colls,     0 par    0.002s   0.002s     0.0001s    0.0002s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.437s  (  0.436s elapsed)
  GC      time    0.006s  (  0.006s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.443s  (  0.443s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    3,661,983,239 bytes per MUT second

  Productivity  98.5% of total user, 98.5% of total elapsed
```

## Other useful tools

The [trace-foreign-calls](https://github.com/well-typed/trace-foreign-calls)
plugin can be used for adding information into the eventlog about foreign calls.