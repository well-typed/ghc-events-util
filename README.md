# Utilities for working with the GHC eventlog

## `show-delta`

Suppose we are debugging an application which contains (in one application)
both a server and a client, and we are debugging occassional long delays between
the client initiating the request and the handler responding. Provided we
add suitable user events into the evnet long, we can use `ghc-events-util`
to debug this. First, run

```
cabal run ghc-events-util -- myapp.eventlog \
  show-delta --match 'handler|client'
```

This will give us events from the eventlog, along with the time of that event
(computed by taking a difference to the previous event on the same capability).
For example, we might see

```
   0.02ms     0.02ms    1097133660  cap 0  client start CREATE
   0.67ms     0.00ms    1097807455  cap 0  handler start CREATE
```

and occassionally

```
   0.00ms     0.00ms    2549430454  cap 0  client start CREATE
  43.90ms     0.01ms    2593331769  cap 0  handler start CREATE
```

which includes the aforementioned long day. The first two columns here are the
time difference to the previous _shown_ event (in this case, the `client start`)
and the time difference to the _actual_ previous event (which gives an answer
to how long that previous event actually took).

We can then inspect the relevant parts of the eventlog with, say

```
cabal run ghc-events-util -- myapp.eventlog \
  show-delta --show-from 1097133660 --show-until 1097807455
```

See also the
[trace-foreign-calls](https://github.com/well-typed/trace-foreign-calls)
plugin for adding information into the eventlog about foreign calls.