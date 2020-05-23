# Hopinosis

[![Build](https://github.com/Twonki/Hopinosis/workflows/HaskellCI/badge.svg)](https://github.com/Twonki/Hopinosis/actions)
![License: MIT](https://img.shields.io/badge/License-MIT-hotpink.svg)

This repository contains the library "Hopinosis" - a Haskell implementation of [Opinosis](https://kavita-ganesan.com/opinosis/).

Opinosis builds a graph from a given text, where each node is a word in the text. For each node the occurrence is noted and the following word is connected via node.
Based on this the most redundant paths can be found given that redundancy is either defined by "word occurrence" (node magnitude) or "succession" (edge magnitude). 

To yield human readable sentences, only those paths are seen as valid which:

1. Start with a node marked as "start"
2. End with a node marked as "end"
3. Are acyclic

**Changes to proposal:**

- The position of a word in the sentence and document is not noted. Keeping track of this would need a strict, iterative approach which is not haskelly.
  Instead, a flag is set whether it was a start and/or end, and the number of occurrences is tracked.
- Due to the changes to the value-bag, both ValueBags and the Graph can be seen as Monoids
- There have been no clearly stated measures for similarity of paths. I have left the function as a parameter and provide jaccard- and cosine-distance as examples.
- It's free, open and for you to explore on Github :octocat:

I have put some [samples](./samples.md) up for you, so you can see what you might expect.

You can find [samples](samples.md) in this repository, with rough estimates of time on a common office-laptop (generic thinkpad).

## Build, Run and Test

### Interactive

To run the code, go to /Src and start your GHCI.

`You@GHCI> :load Hopinosis.hs`

This will let you use the lib. (don't forget to run cabal configure once! You will need libraries.)

### Build with Cabal

For more Information on the setup, see [the cabal file](Hopinosis.cabal).

```shell
$> cabal new-build --enable tests --enable-documentation
$> cabal new-test
$> cabal new-install
```

For the installation you need to have symlinks configured for your cabal.
After that, you can use the library from anywhere on your machine.

If you're using windows, I highly recommend to change that.

### Run when installed

To run after the new-install you can simply go:

```shell
Hopinosis -f ./Files/darkwing.txt -n 2 -d 0.51 -t 0.51 -v --sim jaccard
```

This will run the application. For an overview of the parameters, the [Program.hs](./App/Program.hs) is considerably well documented.

### Run with Cabal

To run without installation, you can do:

`cabal new-run Hopinosis -- -f ./Files/darkwing.txt -n 2 -d 0.51 -t 0.51 -v --sim jaccard`

This also accepts RTS-Parameters such as *+RTS -N2*

### Documentation

This seems to be the right way to build documentation from source:

```shell
$> cabal act-as-setup -- haddock --builddir=dist-newstyle/build/x86_64-windows/ghc-8.6.3/Hopinosis-M.m.f --internal
```

Which will create a lot of items for you. `index.html` is the starting point you are looking for.  

**Note:** You may have to run an `build --enable-documentation` beforehand.

### Performance Measurements

In regard of performance measuring, you need two tools:

- [Threadscope](https://github.com/haskell/ThreadScope/releases) + [this fix](https://askubuntu.com/questions/342202/failed-to-load-module-canberra-gtk-module-but-already-installed)
- [hp2ps](https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/hp2ps.html)

The first must be downloaded, the second was installed with GHC in my case.

`Hopinosis -f ./Files/darkwing.txt -n 2 -d 0.51 -t 0.51 -v --sim jaccard +RTS -hT -s`

To run with multiple cores, specify the number of cores with -Nx such as:

`Hopinosis -f ./Files/darkwing.txt -n 2 -d 0.51 -t 0.51 -v --sim jaccard +RTS -hT -s -N4`

To produce an eventlog visible by threadscope, you must run Hopinosis with the RTS Option `-lf`.
With a working threadscope installation you can then run `threadscope hopinosis.eventlog`

Be careful a bit: I might remove (comment out) the eventlog from cabal to get a faster executable.

However, honestly multicore is not working (well) at the moment.

## Contribution

You're contribution is welcome! There are several topics you can help with:

- Results: If you can show me some applications of your opinosis, please open a feedback issue ! Don't be shy if your results don't match your expectations, neither you nor I came up with the  algorithm!
- Code-Review: Code can always improve - and mine can sure improve a lot! When you find an ugly piece of code, or know a better solution, tell me so!
- Coding: *The more the merrier* - join the programming and help me out!

If you want to help me via code, please refer to the [Contribution Guidelines](CONTRIBUTING.md).

## Additional Notes

Here are some thoughts on the project which may come across your mind:

- using my own cosine-similarity. Yes, I also would like to use a library for that. But I have not found a *lightweight* library for this.
  If I import a nlp-library with cabal, this will blow up the whole build process. That's why I currently stick to two small functions.
- Performance Problems: One thing I am really concerned about is that I cannot imagine some java program to yield so much better results. I have the feeling that there was no similarity measure involved, as none is described in the paper, but after printing the top-n given the metric, the selection has been done manually. This is just a thought.
