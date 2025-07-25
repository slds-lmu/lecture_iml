library("docstring")
library("StatMatch")

generate_whatif = function(x_interest, model, dataset) {
  #' Computes whatif counterfactuals for binary classification models,
  #' i.e., the closest data point with a different prediction.
  #
  #' @param x_interest (data.frame): Data point of interest, a single row data set.
  #' @param model: Binary classifier which can call a predict method.
  #' @param dataset (data.frame): Input data
  #'
  #' @return counterfactual (data.frame): data.frame with one row presenting the counterfactuals
  #'    closest to  `x_interest` with a different prediction.

 return(NULL)
}

evaluate_counterfactual = function(counterfactual, x_interest, model) {
  #' Evaluates if counterfactuals are minimal, i.e., if setting one feature to
  #' the value of x_interest still results in a different prediction than for x_interest.
  #'
  #' @param counterfactual (data.frame): Counterfactual of `x_interest`, a single row data set.
  #' @param x_interest (data.frame): Data point of interest, a single row data set.
  #' @param model: Binary classifier which can call a predict method.
  #'
  #' @return (list): List with names of features that if set for the counterfactual to the value of
  #' `x_interest`, still leads to a different prediction than for x_interest.

  return(NULL)
}


if (FALSE) {
  df <- read.csv(file = "datasets/wheat_seeds.csv")
  print(table(df$Type))

  # Create a binary classification task
  df$Type <- as.factor(ifelse(df$Type == "0", 1, df$Type))
  print(table(df$Type))

  # Fit a random forest to the data
  mod <- randomForest::randomForest(Type ~ ., data = df)
  df$Type <- NULL

  # Compute counterfactual for first observation
  x_interest <- df[1, ]
  cf <- generate_whatif(x_interest = x_interest, model = mod, dataset = df)
  print(cf)
  print(evaluate_counterfactual(counterfactual = cf, x_interest = x_interest, model = mod))
}
