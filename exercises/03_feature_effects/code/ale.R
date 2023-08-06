library("docstring")

get_bounds <- function(X, s, n_intervals = 100) {
  
  #' Calculates interval bounds given a X's min and max values.
  #' Only values in the s-th column are considered.
  #
  #' @param X (data.frame): Input data.
  #' @param s (integer(1)): Index of the feature x_s.
  #' @param n_intervals (integer(1)): Number of intervals.
  #'
  #' @return bounds (vector): Values of bounds with `n_intervals`+1 entries.
  #'

  return(NULL)
}

calculate_ale <- function(model, X, s, n_intervals = 100, centered = FALSE) {
  
  #' Compute the accumulated local effect of a numeric continuous feature.
  #'
  #' @param model: Classifier which has a predict method.
  #' @param X (data.frame): Input data.
  #' @param s (integer(1)): Index of the feature x_s.
  #' @param n_intervals (integer(1)): How many intervals should be used.
  #' @param centered (logical(1)): Whether to return uncentered or centered ALE.
  #'
  #' @return a list with the following elements: 
  #' bounds (vector): See function `get_bounds`.
  #' ale (vector): Values of ALE.

  return(list(bounds = NULL, ale = NULL))
}

prepare_ale <- function(model, X, s, n_intervals = 100, centered = TRUE) {
  #' Uses `calculate_ale` to prepare x and y data, which can be used
  #' by ggplot2 directly.
  #'
  #' @param model: Classifier which has a predict method.
  #' @param X (data.frame): Input data.
  #' @param s (integer(1)): Index of the feature x_s.
  #' @param n_intervals (integer(1)): Number of intervals.
  #' @param centered (logical(1)): Whether to return uncentered or centered ALE.
  #'
  #' @return data.frame with two columns and `n_intervals` rows 
  #'     * x: Centers of two bounds.
  #'     * y: ALE values.
  #'
  
  return(NULL)
  
}

### A function to create the ALE plot from the previous generated objects.
ale_plot <- function() {
  library(randomForest)
  library(ggplot2)
  
  ### Set up your working directory using swd() and get the dataset file. 
  df <- read.csv(file = 'datasets/wheat_seeds.csv')
  
  ### Split the dataset to 70% train data and 30% test data.
  set.seed(100)
  train <- sample(nrow(df), 0.7 * nrow(df), replace = FALSE)
  trainData <- df[train, ]
  testData <- df[-train, ]
  
  ### Normalize the target to be between 0 and 1.
  min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  
  trainData$Type <- min_max_norm(trainData$Type)
  
  ### Build a Random Forest model.
  model <- randomForest(Type ~ ., data = trainData, mtry = 4, importance = TRUE)
  
  ### Use the first feature from the dataset and set up
  ### 4 intervals to test the get_bounds function
  bounds <- get_bounds(df, 1, 4)
  
  ### Test the calculate_ale function, with centered = FALSE
  uncentered_ale <- calculate_ale(model, df, 1, 4, FALSE)
  
  ### Test the prepare_ale function, with centered = TRUE
  prepared_ale <- prepare_ale(model, df, 1, 4, TRUE)
  
  ggplot(data = prepared_ale, mapping = aes(x = x, y = y)) + 
    geom_line() + 
    geom_point()
}

### The warning message can be ignored here.
ale_plot()
