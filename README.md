# Tiger in Haskell

[![Haskell CI](https://github.com/ksrky/Tiger/actions/workflows/haskell.yml/badge.svg)](https://github.com/ksrky/Tiger/actions/workflows/haskell.yml)

Tiger in Haskell is the Haskell implementation of Andrew Appel's "Modern Compiler Implementation in ML".
Haskell is a purely functional language unlike ML, so there are some changes from the original specifications.
Please see the implementation notes in each chapter.

## Prerequisities

Tiger has been tested with the following versions.

```
stack 2.7.5
cabal 3.6.2.0
GHC 8.10.7
```

Installing [GHCup](https://www.haskell.org/ghcup/) allows you to manage versions of all these tools.

## Build

### Building with Stack

```command
$ stack build
$ stack exec <executable> <option>
```

### Building with Cabal

```command
$ cabal build
$ cabal run <executable> <option>
```

## Part I

- [Chapter 2](https://github.com/ksrky/Tiger/tree/master/src/chapter2)
- [Chapter 3](https://github.com/ksrky/Tiger/tree/master/src/chapter3)
- [Chapter 4](https://github.com/ksrky/Tiger/tree/master/src/chapter4)
- [Chapter 5a](https://github.com/ksrky/Tiger/tree/master/src/chapter5a)
- [Chapter 5b](https://github.com/ksrky/Tiger/tree/master/src/chapter5b)
- [Chapter 6](https://github.com/ksrky/Tiger/tree/master/src/chapter6)
- [Chapter 7](https://github.com/ksrky/Tiger/tree/master/src/chapter7)
- [Chapter 8](https://github.com/ksrky/Tiger/tree/master/src/chapter8)
- [Chapter 9](https://github.com/ksrky/Tiger/tree/master/src/chapter9)
- [Chapter 10](https://github.com/ksrky/Tiger/tree/master/src/chapter10)
- Chapter 11
- Chapter 12

## Part II

- Chapter 13
- Chapter 14
- Chapter 15
  - Fun Tiger
  - Lazy Tiger
- Chapter 16
- Chapter 17
- Chapter 18
- Chapter 19
- Chapter 20

## My Work

- [LLVM](https://github.com/ksrky/Tiger/tree/master/src/llvm)
