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
  
  ### Luckily for us the seq()-function already implements exactly what we want if we take two neighboring 
  ### equidistant points as the boundaries of one interval. Now we need n_intervals + 1 points since we always
  ### need two points for one interval resulting in one point more than the number of intervals we want.
  seq(min(X[ , s]), max(X[ , s]), length.out = n_intervals + 1)
}

calculate_ale = function(model, X, s, n_intervals = 100, centered = FALSE) {
  
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
  
  ### Get the feature column specified by s from the input data.
  x_s <- X[, s]
  
  ### Save all possible lower and upper bounds in lowerbounds and upperbounds, respectively. 
  boundary_points <- get_bounds(X, s, n_intervals)
  lowerbounds <- boundary_points[1:n_intervals]
  upperbounds <- boundary_points[2:(n_intervals + 1)]
  
  ### Now we want to iterate over all intervals. For this we will use mapply() in the following but for readability we define the
  ### function to apply before. This function returns the uncentered ALE value for one interval given the left and the right
  ### boundary of the interval.
  uncentered_ale_calculation <- function(left_boundary, right_boundary) {
    ### The first interval is of the form [left_boundary, right_boundary] (so that we do not ignore the points with the minimal feature value).
    if (left_boundary == lowerbounds[1]) {
      idxs <- which(x_s >= left_boundary & x_s <= right_boundary)
    }
    ### All other intervals are of the form (left_boundary, right_boundary] (so that no point falls into two intervals).
    else {
      idxs <- which(x_s > left_boundary & x_s <= right_boundary)
    }
    
    ### If there is no point in the interval return 0 (as stated in the task).
    if (length(idxs) == 0) {
      return(0)
    }
    
    ### Else we compute the ALE value. The first step is to replace the feature values of all the points that fall into the interval
    ### with the left and the right boundary of the interval.
    X_min <- X[idxs, ]
    X_max <- X[idxs, ]
    X_min <- replace(X_min, s, left_boundary)
    X_max <- replace(X_max, s, right_boundary)
    
    ### Then we let the model predict these "new" data points.
    y_min <- predict(model, X_min)
    y_max <- predict(model, X_max)
    
    ### We return the arithmetic mean of the prediction differences.
    mean(y_max - y_min)
  }
  
  ### Now we can calculate the ALE values all at once with the mapply()-function. 
  ### We accumulate them via the cumsum()-function.
  ale_values <- cumsum(mapply(uncentered_ale_calculation, lowerbounds, upperbounds))
  
  ### If the centered argument is set to TRUE the user wants us to center the ALE values.
  if (centered)
    ale_values <- ale_values - mean(ale_values)
  
  ### Return the boundary points as well as the centered or uncentered ALE values.
  list(bounds = boundary_points, ale = ale_values)
}

prepare_ale = function(model, X, s, n_intervals = 100, centered = TRUE) {
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
  
  ### Get the ALE values via our function from task b).
  ale <- calculate_ale(model, X, s, n_intervals, centered)
  
  ### Get the bounds and y from the list.
  bounds <- unlist(ale[1])
  y <- unlist(ale[2])
  
  ### Reconstruct the lowerbounds and upperbounds vectors.
  lowerbounds <- bounds[1:n_intervals]
  upperbounds <- bounds[2:(n_intervals + 1)]
  
  ### Get the center of each interval.
  x <- rowMeans(cbind(lowerbounds, upperbounds))
  
  ### Now for ggplot2 the expected format is a data.frame.
  data.frame(x = x, y = y)
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
