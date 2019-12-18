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
- There have been no clearly stated measures for similarity of paths. I have left the function as a parameter and provide jaqqard- and cosine-distance as examples. 
- It's free, open and for you to explore on Github :octocat:


## Build, Run and Test
### Interactive
To run the code, go to /Src and start your GHCI. 

`You@GHCI> :load Hopinosis.hs`

This will let you use the lib. (don't forget to run cabal configure once! You will need libraries.)

### With Cabal
For more Information on the setup, see [the cabal file](Hopinosis.cabal).

```shell
$> cabal new-build --enable tests --enable-documentation
$> cabal new-test
$> cabal new-install
```

For the installation you need to have symlinks configured for your cabal. 
After that, you can use the library from anywhere on your machine. 

If you're using windows, I highly recommend to change that. 

### Documentation
This seems to be the right way to build documentation from source:

```shell
$> cabal act-as-setup -- haddock --builddir=dist-newstyle/build/x86_64-windows/ghc-8.6.3/Hopinosis-M.m.f --internal
```

Which will create a lot of items for you. `index.html` is the starting point you are looking for.  

**Note:** You may have to run an `build --enable-documentation` beforehand. 

### Distribution 
Sources have to be build beforehand:

```shell
$> cabal act-as-setup -- sdist --snapshot
```

## Contribution
You're contribution is welcome! There are several topics you can help with:

* Results: If you can show me some applications of your opinosis, please open a feedback issue ! Don't be shy if your results don't match your expectations, neither you nor I came up with the  algorithm!
* Code-Review: Code can always improve - and mine can sure improve a lot! When you find an ugly piece of code, or know a better solution, tell me so!
* Coding: *The more the merrier* - join the programming and help me out!

If you want to help me via code, please refer to the [Contribution Guidelines](CONTRIBUTING.md).

## Additional Notes
Here are some thoughts on the project which may come across your mind:
- using my own cosine-similarity. Yes, I also would like to use a library for that. But I have not found a *lightweight* library for this.
  If I import a nlp-library with cabal, this will blow up the whole build process. That's why I currently stick to two small functions. 
- HUnit vs. Quickcheck. Yes, I think a lot of parts are perfectly fit for Quickcheck, mostly the parsing.
- Performance Problems: One thing I am really concerned about is that i cannot imagine some java program to yield so much better results. I have the feeling that there was no similarity measure involved, as none is described in the paper, but after printing the top-n given the metric, the selection has been done manually. This is just a thought so. 