
plot.heaRt_fit <- function(x, decision.tree = FALSE, random.forest = FALSE){

  # checking if the object is either a decision tree or a random forest
  if(decision.tree == TRUE)
    return(rpart.plot::prp(x))

  if(random.forest == TRUE) #risolvere questo
    return(randomForest::partialPlot(x, ))

  dat <- x$data





}
