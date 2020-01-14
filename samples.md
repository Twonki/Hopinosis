# Samples 
This markdown contains some examples of the Hopinosis results.

Most of these are pure garbage. I'm being honest. 

But I value honesty very much so I maybe as well safe you some time.
The considerable good ones will be marked with :star: so you can try those parameters first.  

## Parameters

Because you maybe have not seen the code yet, a short summary:

the first parameter is the metric function of a single path. The dummy is `\x -> 1`

the second parameter is the similarity function, which takes two paths and gives a value between 0 (sentences share nothing) to 1 (sentences are identical).
The dummy is `\a,b -> 1` 

the third parameter  is n, the number of result sentences you want to get. 

the fourth parameter is *sigmaAlpha*, which determines how often valid starts had to be valid starts percentual. 
It therefore goes from 0.0 (everything that has ever been an start) to atmost 0.99 (1 will always yield no result!). 
A value of 0.49 means, that every candidate start has to be atleast a start in half the sentences it occurs. 
The dummy is `0.0`.

the fitht parameter is *sigmaDelta*, which determines how high the given metric must be to be a valid path. 
The dummy is `0.0`.

## Text 1 

> I like cats. Cats eat a lot of food. 
> My cat is called Catie. my cats full name is Catharina. 
> This is not one of my worst jokes. Anyway, Catie likes poultry as her favorite food"

``` 
/Src> ghci
:load Hopinosis.hs
text1 = "I like cats. Cats eat a lot of food. My cat is called Catie. my cats full name is Catharina. This is not one of my worst jokes. Anyway, Catie likes poultry as her favorite food"
``` 


**Note** Length 3 takes *significantly* longer than length 2. 

**Warning: Length 4 takes over 5 minutes and is really strange.** 


With: 

`summarize Metric.averagedEdgeStrength Metric.jaccardSim 2 0.1 0.1 text1`

results in 
- "anyway, catie" 
- "my cats"

`summarize Metric.averagedEdgeStrength Metric.jaccardSim 2 0.51 0.51 text1 `

results in 
- "my cats full name is called catie"
- "my cats full name is catharina"

:star: `summarize Metric.averagedEdgeStrength Metric.jaccardSim 3 0.51 0.51 text1 `

- "this is not one of food"
- "this is not one of my cats"
- "this is not one of my worst jokes"

`summarize Metric.averagedEdgeStrengths Metric.jaccardSim 4 0.21 0.21 sample1`

*took over 10 minutes*

- "cats full name is called catie"
- "cats full name is catharina"
- "my cats full name is called catie"
- "my cats full name is catharina"

`summarize Metric.averagedEdgeStrength Metric.cosineSim 2 0.1 0.1 text1`

- "cats eat a lot of my cat is called catie"
- "cats eat a lot of my cat is catharina"

:star: `summarize Metric.averagedEdgeStrength Metric.cosineSim 2 0.51 0.51 text1`

- "i like cats eat a lot of my cat is called catie"
- "i like cats eat a lot of my cat is called catie likes poultry as her favorite food"

`summarize Metric.averagedEdgeStrength Metric.cosineSim 3 0.51 0.51 text1`

- "anyway, catie likes poultry as her favorite food"
- "i like cats eat a lot of my cat is called catie"
- "i like cats eat a lot of my cat is called catie likes poultry as her favorite food"

`summarize Metric.averagedEdgeStrengths Metric.cosineSim 4 0.21 0.21 sample1`

*took over 10 minutes*

- "anyway, catie"
- "anyway, catie likes poultry as her favorite food"
- "cats eat a lot of my cat is called catie"
- "cats eat a lot of my cat is catharina"

## Text 2 

> The eskimos have more than 50 words for snow. 
> For some time, eskimos should have been called Inuit, as eskimo was the name given by colonists. 
> apparently, eskimos seem to be fine with being called eskimos nowadays. 
> I do not know, whether inuit also have 50 words for snow. 
> but maybe I will meet both an Inuit and an eskimo, and clarify what is correct. 
> until then, I will use 1 word for snow. 
> which is snow. 

```
text2 = "The eskimos have more than 50 words for snow. For some time, eskimos should have been called inuit, as eskimo was the name given by colonists. apparently, eskimos seem to be fine with being called eskimos nowadays. I do not know, whether inuit also have 50 words for snow. but maybe i will meet both an inuit and an eskimo, and clarify what is correct. until then, i will use 1 word for snow. which is snow."
```

With:
`summarize Metric.averagedEdgeStrengths Metric.jaccardSim 2 0.51 0.51 text2`

- "but maybe i do not know, whether inuit and clarify what is snow"
- "which is snow"

`summarize Metric.averagedEdgeStrengths Metric.jaccardSim 2 0.01 0.01 text2` -- *note: very same for non-averaged-edge strengths*

- "apparently, eskimos nowadays"
- "the eskimos nowadays"

`summarize Metric.edgeStrengths Metric.jaccardSim 3 0.01 0.01 text2` -- *note: takes 10 minutes*

- "apparently, eskimos nowadays"
- "for some time, eskimos nowadays"
- "the eskimos nowadays"

`summarize Metric.averagedEdgeStrengths Metric.jaccardSim 3 0.51 0.51 text2` -- *Note: takes 3 minutes*

- "but maybe i do not know, whether inuit and clarify what is snow"
- "until then, i do not know, whether inuit and clarify what is snow"
- "which is snow"

:star: `summarize Metric.averagedEdgeStrengths Metric.jaccardSim 1 0.01 1.0 text2`

- "but maybe i do not know, whether inuit also have more than 50 words for some time, eskimos seem to be fine with being called inuit, as eskimo was the name given by colonists"

:star: `summarize Metric.averagedEdgeStrengths Metric.jaccardSim 1 0.01 1.2 text2`

- "but maybe i do not know, whether inuit also have 50 words for snow"

:star: `summarize Metric.averagedMagnitudes Metric.jaccardSim 1 0.01 1.2 text2`
- "apparently, eskimos seem to be fine with being called inuit, as eskimo was the name given by colonists"

`summarize Metric.averagedEdgeStrengths Metric.cosineSim 3 0.51 0.51 text2` -- *Note: takes 3 minutes* 

- "apparently, eskimos have 50 words for snow"
- "until then, i will meet both an inuit also have more than 50 words for some time, eskimos nowadays"
- "until then, i will meet both an inuit also have more than 50 words for some time, eskimos seem to be fine with being called inuit, as eskimo was the name given by colonists"


`summarize Metric.averagedMagnitudes Metric.cosineSim 3 0.51 0.51 text2`
- "apparently, eskimos have 50 words for snow"
- "until then, i will meet both an inuit also have more than 50 words for some time, eskimos nowadays"
- "until then, i will meet both an inuit also have more than 50 words for some time, eskimos seem to be fine with being called inuit, as eskimo was the name given by colonists"

`summarize Metric.averagedMagnitudes Metric.cosineSim 3 0.01 0.01 text2` -- *Note: takes 10 minutes* 
- "for snow","until then, i will meet both an inuit also have more than 50 words for some time, eskimos nowadays"
- "until then, i will meet both an inuit also have more than 50 words for some time, eskimos
- seem to be fine with being called inuit, as eskimo was the name given by colonists"


## Text 3
The following are various [Darkwing Duck](https://www.youtube.com/watch?v=3_ee6aZObyI)-quotes. 

> I am the terror that flaps in the night, I am the batteries that are not included.
> I am the terror that flaps in the night, I am the fingernail that scrapes the blackboard of your soul.
> I am the terror that flaps in the night, I am a special news bulletin that interrupts your favorite show.
> I am the terror that flaps in the night! I am the wrong number that wakes you at 3am.
> I am the terror that flaps in the night, I am the onion that stings in your eye.
> I am the terror that flaps in the night, I am the hairball that clogs your drains.
> I am the terror that flaps in the night, I am the low ratings that cancel your program.
> I am the terror that flaps in the night, I am the termite that devours your floorboards.
> I am the terror that flaps in the night, I am the slug that slimes your begonias.
> I am the terror that flaps in the night. I am Darkwing Duck.

*Let's get dangerous!*

```
text3 = "I am the terror that flaps in the night, I am the batteries that are not included. I am the terror that flaps in the night, I am the fingernail that scrapes the blackboard of your soul. I am the terror that flaps in the night, I am a special news bulletin that interrupts your favorite show. I am the terror that flaps in the night! I am the wrong number that wakes you at 3am. I am the terror that flaps in the night, I am the onion that stings in your eye. I am the terror that flaps in the night, I am the hairball that clogs your drains. I am the terror that flaps in the night, I am the low ratings that cancel your program. I am the terror that flaps in the night, I am the termite that devours your floorboards. I am the terror that flaps in the night, I am the slug that slimes your begonias. I am the terror that flaps in the night. I am Darkwing Duck."
```

With:

:star: :star: `summarize Metric.averagedEdgeStrengths Metric.jaccardSim 2 0.51 0.51 text3`
- "i am a special news bulletin that scrapes the blackboard of your begonias"
- "i am a special news bulletin that scrapes the blackboard of your favorite show"

`summarize Metric.averagedEdgeStrengths Metric.cosineSim 2 0.51 0.51 text3`

- "i am the terror that flaps in your begonias"
- "i am the terror that flaps in your drains"


`summarize Metric.averagedMagnitudes Metric.jaccardSim 1 0.51 0.51 text3`

- "i am a special news bulletin that wakes you at 3am"

:star: `summarize Metric.averagedMagnitudes Metric.jaccardSim 2 0.51 1.51 text3`

- "i am a special news bulletin that cancel your favorite show"
- "i am a special news bulletin that clogs your favorite show"


`summarize Metric.averagedEdgeStrengths Metric.cosineSim 3 0.51 1.51 text3` -- *Note:* takes 90 minutes

Failed due to memory (lol)

`summarize Metric.edgeStrengths Metric.cosineSim 2 0.51 1.51 text3`

- "i am the terror that flaps in your begonias"
- "i am the terror that flaps in your favorite show"

:star: :star: `summarize Metric.edgeStrengths Metric.jaccardSim 2 0.51 1.51 text3`
- "i am a special news bulletin that stings in your begonias"
- "i am a special news bulletin that stings in your drains"


** The function: `summarizeWithoutDistances` performs really really fast, however the results are very similar. **

## Text 4
The following is the introduction scene of Southparks *The Coon* [Season 13 Episode 2](https://www.southparkstudios.co.uk/full-episodes/s13e02-the-coon) and the transcript is taken [from the fandom](https://southpark.fandom.com/wiki/The_Coon/Script).

```
 text4 = "[voice over] The city isn't what it used to be. It all happened so fast. Everything went to crap. It's like... everyone's sense of morals just... disappeared. Bad economy made things worse. [A shot of Tele's TV store  Closed until further notice due to BAD ECONOMY ] The jobs started drying up. Then the stores had to shut down. [a shadow runs across the screen, in a cape. Next scene: an alley looking towards the street. An Obama poster for CHANGE is shown, with WHEN? spray-painted over the word] Then a black man was elected President. He was supposed to change things. [the shadow climbs up a pipe and runs across the roof of the building opposite the alley entrance] He didn't. [next shot is that of a street under a threatening red sky] As more and more people turned to crime and violence the town becomes gripped in fear. Dark times. The city needs protection. [another alley. An aluminum trash can falls over, spilling its contents. The shadow appears on a wall, larger than life, then runs away] There is an animal that lives by night, searches the trashcans and cleans out the garbage. [A rain gutter is shown. The shadow runs across the screen again, closer to the camera] To clean out the trashcan of society I've chosen to become more than a man. [another rooftop. The camera finds the shadow and zooms in on it.] I'm the hero this town needs. I am... [the shadow turns and is lit up. It's Cartman dressed as...] the Coon! [Cartman looks up to the sky, and graphics for  The Coon  pop up. Next scene, the Coon runs down a street and leaps onto the roof of a car] As the world plummets into despair the Coon fights to keep order intact. [he leaps off the car]."
```

**Note:** This completely fails on every try. Lol. I need a bigger machine.

## Summary of Findings
1. Having high alpha values seems to purge a lot of the items
2. jaccard and cosine distance both take ages given high n
3. cosine distance yield longer sentences 
4. averaged magnitudes yield worse sentences
5. With averaged metrics, higher sigma delta values yield shorter sentences (in general)
6. cosine and jaccard take about the same time. 