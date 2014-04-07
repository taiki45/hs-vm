hs-vm [![Build Status](https://travis-ci.org/taiki45/hs-vm.svg?branch=master)](https://travis-ci.org/taiki45/hs-vm)
=====
Simple Virtual Machine written in Haskell.

## About
hs-vm is based on a stack machine architecture.

## Status
experiment phase

## Usage
To run example just:

```
cabal sandbox init
cabal configure --enable-tests
cabal install --enable-tests --only-dependencies
cabal build
./dist/build/hs-vm/hs-vm example/fib.hsvm
```

To test it:

```
cabal build && cabal test
```

hs-vm's instructions are in `src/VM/Instruction.hs`.

All label setting instructions are executed in preprocess phase.

A valid hs-vm program has `main` function. To set main function, use `Label main`.

To define function, use `Label func_name`. To return to calling point, use `Ret`.

hs-vm stacks Local Data Stack when calls a function.
And when returned from function, hs-vm pops back stacked one.
