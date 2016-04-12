# The Computer Language Benchmarks Game _fannkuch-redux_

A Haskell program submission for the _fannkuch-redux_ program in [The Computer Language Benchmarks Game](http://benchmarksgame.alioth.debian.org/).

This program is based on the __Haskell GHC #4__ submission, but fixes two problems.

1. In the 1-core test, __Haskell GHC #4__ failed to build. (Gabriel Gonzales fixed that build problem, but it looks like his fix was only applied to the 4-core test.)

2. In the 4-core test, __Haskell GHC #4__ doesn't run in parallel. This program parallelizes __Haskell GHC #4__.

## Building

### Dependencies

* _ghc >= 7.10.2_ or _stack_

* Packages `base,vector,async`

### ghc MAKE
```
$ ghc --make -fllvm -O2 -threaded -XBangPatterns -XScopedTypeVariables -rtsopts fannkuch-redux.hs -o fannkuch-redux
```

### stack MAKE
```
$ stack build
$ stack install
```

## Running

### 1-core
```
$ fannkuch-redux +RTS -N1 -RTS 12
```

### 4-core
```
$ fannkuch-redux +RTS -N4 -RTS 12
```



## References

#### _fannkuch-redux_ Description
http://benchmarksgame.alioth.debian.org/u64q/fannkuchredux-description.html

#### _fannkuch-redux_ 4-Core Results
https://benchmarksgame.alioth.debian.org/u64q/performance.php?test=fannkuchredux

#### _fannkuch-redux_ 1-Core Results
https://benchmarksgame.alioth.debian.org/u64/performance.php?test=fannkuchredux

#### Haskell GHC #4 Implementation
https://benchmarksgame.alioth.debian.org/u64q/program.php?test=fannkuchredux&lang=ghc&id=4

#### Upload Instructions
http://benchmarksgame.alioth.debian.org/u64q/play.html

#### 4-Core Fastest Graph
http://benchmarksgame.alioth.debian.org/u64q/which-programs-are-fastest.html

#### 1-Core Fastest Graph
http://benchmarksgame.alioth.debian.org/u64/which-programs-are-fastest.html



## Submission Status

[Submitted to Alioth on 2016-03-10](https://alioth.debian.org/tracker/index.php?func=detail&aid=315336&group_id=100815&atid=413122)

### 4-Core

#### Before
<img src="http://jamesdbrock.github.io/fannkuch-redux/benchmarksgame.which-programs-are-fastest-firstlast.u64q.20160218.svg" />

### 1-Core

#### Before
<img src="http://jamesdbrock.github.io/fannkuch-redux/benchmarksgame.which-programs-are-fastest-middle.u64.20151022.svg" />
