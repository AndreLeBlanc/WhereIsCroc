source("WheresCroc.R")
source("markov.R")
source("stats.R")

runWheresCroc(markovMoves, T, 1)
runWheresCroc(randomWC, T, 1)
runWheresCroc(manualWC, T, 1)
