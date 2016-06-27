# GeneticMusicComposition

October 2015

This Haskell program uses the Haskore music library to create 8-measure piano compositions.  Starting with an inital population
of randomly generated measures, an evolutionary algorithm produces mutant offspring of the most successful individuals while 
culling the least successful.  The judgement of the user acts as the fitness function.  Once a large enough population of fit
individuals has evolved the program switches to a genetic algorithm in which the evolved measures are joined together into 
8-measure songs, and the most successful songs mate to produce offspring.  The offspring may mutate by shifted some number of 
measures, while preserving the measure order.  When the (human) fitness function ranks one of the songs highly enough the genetic
algorithm ends and the completed song is saved as a MIDI file.  A couple of example songs are included here.

Neil Babson
