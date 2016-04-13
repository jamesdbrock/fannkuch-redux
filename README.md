# The Computer Language Benchmarks Game _fannkuch-redux_

A Haskell program submission for the _[fannkuch-redux](http://benchmarksgame.alioth.debian.org/u64q/performance.php?test=fannkuchredux)_ problem in [The Computer Language Benchmarks Game](http://benchmarksgame.alioth.debian.org/).

I chose the _fannkuch-redux_ problem because it's the worst data point in Haskell's distribution on the [Which programs are fastest?](http://benchmarksgame.alioth.debian.org/u64q/which-programs-are-fastest.html) page. _fannkuch-redux_ is an "embarassingly parallel" problem, and the fastest Haskell submission for _fannkuch-redux_, __Haskell GHC #4__, is not parallelized.

This program is a re-write with parallelization of the __Haskell GHC #4__ submission. 

## Submission Status

[Submitted to Alioth on 2016-03-10](https://alioth.debian.org/tracker/index.php?func=detail&aid=315336&group_id=100815&atid=413122)

Accepted by Alioth on 2016-04-13 as _[fannkuch-redux](http://benchmarksgame.alioth.debian.org/u64q/performance.php?test=fannkuchredux)_ program __[Haskell GHC #6](http://benchmarksgame.alioth.debian.org/u64q/program.php?test=fannkuchredux&lang=ghc&id=6)__.

Here are the before and after images from the [Which programs are fastest?](http://benchmarksgame.alioth.debian.org/u64q/which-programs-are-fastest.html) page. With a parallel _fannkuch-redux_ implementation, Haskell has moved ahead of OCaml and Closure.

### Before
<img src="http://jamesdbrock.github.io/fannkuch-redux/benchmarksgame.which-programs-are-fastest-firstlast.u64q.20160412.svg" />

### After
<img src="http://jamesdbrock.github.io/fannkuch-redux/benchmarksgame.which-programs-are-fastest-firstlast.u64q.20160413.svg" />

## Building

### Dependencies

* _ghc >= 7.10.2_ or _stack_

* Packages `base,vector,async`

* _LLVM 3.5_

### ghc MAKE
```
$ ghc --make -fllvm -O2 -threaded -XBangPatterns -XScopedTypeVariables -rtsopts fannkuch-redux.hs -o fannkuch-redux
```

### stack MAKE
```
$ stack build
$ stack install
```

## Running with 4-core parallelism

```
$ fannkuch-redux +RTS -N4 -RTS 12
```

