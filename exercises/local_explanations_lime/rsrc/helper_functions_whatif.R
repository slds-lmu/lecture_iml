library("docstring")
library(StatMatch)

generate_whatif = function(x_interest, model, dataset) {
  #' Computes whatif counterfactuals for binary classification models,
  #' i.e., the closest data point with a different prediction.
  #
  #' @param x_interest (data.frame): Datapoint of interest, a single
  #' row data set.
  #' @param model: Binary classifier which can call a predict method.
  #' @param dataset (data.frame): Input data
  #'
  #' @return counterfactual (data.frame): data.frame with one row 
  #' presenting the counterfactuals
  #'    closest to  `x_interest` with a different prediction.
  
  # subset dataset to the observations having a prediction different 
  # to x_interest
  pred = predict(model, newdata = x_interest)
  preddata = predict(model, dataset)
  idx = which(preddata != pred)
  dataset = dataset[idx, ]
  
  # Pairwise Gower distances
  dists = StatMatch::gower.dist(data.x = x_interest, data.y = dataset)
  minid = order(dists)[1]
  
  # Return nearest datapoint
  return(dataset[minid, ])
}

evaluate_counterfactual = function(counterfactual, x_interest, model) {
  #' Evaluates if counterfactuals are minimal, i.e., if setting one feature to
  #' the value of x_interest still results in a different prediction than for 
  #' x_interest.
  #'
  #' @param counterfactual (data.frame): Counterfactual of `x_interest`, 
  #' a single row data set.
  #' @param x_interest (data.frame): Datapoint of interest, 
  #' a single row data set.
  #' @param model: Binary classifier which can call a predict method.
  #'
  #' @return (list): List with names of features that if set for the
  #'  counterfactual to the value of
  #' `x_interest`, still leads to a different prediction than for x_interest.
  pred = predict(model, newdata = x_interest)
  feature_nams = c()
  for (feature in names(counterfactual)) {
    if (counterfactual[feature] == x_interest[feature]) {
      next
    }
    newcf = counterfactual
    newcf[, feature] = x_interest[, feature]
    newpred = predict(model, newcf)
    if (newpred != pred) {
      feature_nams = c(feature_nams, feature)
    }
  }
  return(feature_nams)
}
