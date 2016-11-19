library("gramEvol")

#Define the possible formulas
grammarDef <- CreateGrammar(list(
  expr  = grule(op(expr, expr), func(expr), num),
  func  = grule(sin, cos, tan, log, sqrt, exp),
  op    = grule(`+`, `-`, `*`, `/`, `^`),
  num   = gvrule(0:1000)
))

#Define the fitness function
FitnessFunction <- function(expr) {
  result <- abs(pi - eval(expr))
  if(is.nan(result)) {
    result <- Inf
  }
  return (result)
}

#Run the genetic algorithm and show the results
geneticAlgorithm <- GrammaticalEvolution(grammarDef, FitnessFunction, iterations = 100)
print(geneticAlgorithm)

