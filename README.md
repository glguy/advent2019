# Advent of Code 2019

Thes are my Advent of Code 2019 solutions. I try to focus on making clean and documented Haskell solutions to each puzzle.

We'll be chatting about AoC on IRC all December. You can find AoC discussion on [freenode](https://freenode.net)'s `##adventofcode` and `#haskell` ([webchat](https://webchat.freenode.net/#haskell,##adventofcode))

## Building

I recommend installing `ghc` with `ghcup`.

```
$ curl https://get-ghcup.haskell.org -sSf | sh
```

I'm using `GHC 8.8.1` this year

```
$ ghcup install 8.8.1
$ ghcup set     8.8.1
```

`cabal-install-3` is the best way to get things built

```
$ cabal update
$ cabal build
```

## Running solutions

All the solutions take an optional command line argument that can be an input file name or `-` to read the input from `stdin`.

```
$ cabal run Day01
...
3188480
4779847
```

## Common libraries used

* **containers** - almost all of the solutions will benefit from having access to `Map` and `Set` types from this package.
* **megaparsec** - Each problem features a simple text-file format input. Parser combinators make it easy to define the simple parser needed to consume these inputs.
