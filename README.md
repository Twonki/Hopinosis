# Hopinosis

ToDo: Summary and cite on paper

## Build, Run and Test
### Interactive
To run the code, go to /Src and start your GHCI. 

`You@GHCI> :load Hopinosis.hs`

This will let you use the lib. To manually inspect features, I recommend to go to /Test and load the UnitTests. 

### With Cabal
You can build, test and run the game with Cabal. For more Information on the setup, see [the cabal file](Hopinosis.cabal).

```
$ cabal new-configure --enable-tests
$ cabal new-build --enable tests
$ cabal new-test
$ cabal new-install
```

For the installation you need to have symlinks configured for your cabal. After that, you can invoke the game from anywhere on your machine. 

If you're using windows, I highly recommend to change that. 

## Contribution
You're contribution is welcome! There are several topics you can help with:

* Chess related: I'm not totally sure about the Game itself, so if there is any flaw in possible movements, or other features of the real-life-chess missing, please open an issue!
* Code-Review: Code can always improve - and mine can sure improve a lot! When you find an ugly piece of code, or know a better solution, tell me so!
* Coding: *The more the merrier* - join the programming and help me out!

If you want to help me via code, please refer to the [Contribution Guidelines](CONTRIBUTING.md).
