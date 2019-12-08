# Hopinosis

[![Build](https://github.com/Twonki/Hopinosis/workflows/HaskellCI/badge.svg)](https://github.com/Twonki/Hopinosis/actions) 

This repository contains the library "Hopinosis" - a haskell implementation of [Opinosis](https://kavita-ganesan.com/opinosis/). 


Opinosis builds a graph from a given text, where each node is a word in the text. For each node the occurrence is noted and the following word is connected via node.
Based on this the most redudant paths can be found given that redudancy is either defined by "word occurence" (node magnitude) or "succession" (edge magnitude). 

To yield human readable sentences, only those paths are seen as valid which:
1. Start with a node marked as "start"
2. End with a node marked as "end"
3. Are acyclic

**Changes to proposal:**
- The position of a word in the sentence and document is not noted. Keeping track of this would need a strict, iterative approach which is not haskelly. Instead, a flag is set whether it was a start and/or end, and the number of occurences is tracked. 
- Due to the changes to the value-bag, both ValueBags and the Graph can be seen as Monoids 
- It's free, open and for you to explore on Github :octocat:


## Build, Run and Test
### Interactive
To run the code, go to /Src and start your GHCI. 

`You@GHCI> :load Hopinosis.hs`

This will let you use the lib. (don't forget to run cabal configure once! You will need libraries.)

### With Cabal
For more Information on the setup, see [the cabal file](Hopinosis.cabal).

```
$ cabal new-build --enable tests
$ cabal new-test
$ cabal new-install
```

For the installation you need to have symlinks configured for your cabal. After that, you can invoke the game from anywhere on your machine. 

If you're using windows, I highly recommend to change that. 

## Contribution
You're contribution is welcome! There are several topics you can help with:

* Results: If you can show me some applications of your opinosis, please open a feedback issue ! Don't be shy if your results don't match your expectations, neither you nor I came up with the  algorithm!
* Code-Review: Code can always improve - and mine can sure improve a lot! When you find an ugly piece of code, or know a better solution, tell me so!
* Coding: *The more the merrier* - join the programming and help me out!

If you want to help me via code, please refer to the [Contribution Guidelines](CONTRIBUTING.md).
