

GetDoublingTime <- function(initialCount, finalCount,growthTime){
  DoublingTime <- growthTime * (log(2)/log((finalCount/initialCount)))
}



toSeed <- function(growthTime,cellsWanted, DoublingTime){
cellsWanted/exp(growthTime *log(2)/DoublingTime)
}



