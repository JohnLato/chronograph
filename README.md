
Chronograph
============

An instrumentation wrapper for Haskell data

Chronograph allows you to measure and record data and IO evaluation time.

Chronograph is meant to be used as a lightweight, unobtrusive mechanism to
collect timings of evaluations and IO actions during program execution.

Installation
------------

Install the latest version from Hackage:

    $ cabal install chronograph

Alternatively, git clone this repo and:

    $ cabal install

Use
---

To measure the evaluation time of an expression, pass it as an argument to the
'chrono' function:

    > import Data.Chronograph
    >
    > let timed_someExpr = chrono (someExpr)

'chrono' has the type 'a -> Chronograph a'

Then just use the 'val' of a 'Chronograph' everywhere you would use the
original expression.

    > doSomethingWith (val timed_someExpr)

You can check the evaluation time with 'measure'

    > let exprTime = measure timed_someExpr

For IO actions, use 'chronoIO'.

Evaluation Order
----------------

Values in a Chronograph are lazy, and won't be evaluated until either the 'val'
or 'measure' is evaluated.

'chronoBy' and 'chronoIOBy' take an evaluation parameter to control how much of
the data will be evaluated and timed.  The shortcut functions 'chrono' and
'chronoNF' are provided for the common cases of weak head normal form (seq) and
normal form (deepseq).

Join in
-------

File bugs in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/JohnLato/chronograph`

# License

BSD3. See `LICENSE` for terms of copyright and redistribution.

[issue tracker]: http://github.com/JohnLato/chronograph/issues
[gh]: http://github.com/JohnLato/chronograph
