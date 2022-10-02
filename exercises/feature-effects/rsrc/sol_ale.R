library("docstring")

get_bounds = function(X, s, n_intervals = 100) {
  
  #' Calculates interval bounds given a X's min and max values.
  #' Only values in the s-th column are considered.
  #
  #' @param X (data.frame): Input data.
  #' @param s (integer(1)): Index of the feature x_s.
  #' @param n_intervals (integer(1)): Number of intervals.
  #'
  #' @return bounds (vector): Values of bounds with `n_intervals`+1 entries.
  #'
  x_s = X[, s]
  x_s_min = min(x_s)
  x_s_max = max(x_s)
  bounds = seq(x_s_min, x_s_max, length.out = n_intervals + 1)
  
  return(bounds)
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

  # Get the feature column specified by s from the input data.
  x_s = X[, s]
  
  # Get the interval bounds with the lowerbound and upperbound specified.
  bounds = get_bounds(X, s, n_intervals)
  boundsLength = length(bounds)
  lowerbound = bounds[1:boundsLength - 1]
  upperbound = bounds[2:boundsLength]
  
  # Calculate_ale function later passed to mapply()
  uncentered_ale_calculation = function(i1, i2) {
    # Get ids in between the interval
    if (i1 == lowerbound[1]) {
      idx = which(x_s >= i1 & x_s <= i2)
    }
    else{
      idx = which(x_s > i1 & x_s <= i2)
    }
    
    if (length(idx) == 0) {
      return(0)
    }
    
    # Now we replace the values at position s one time with smallest value
    # and one time with the highest value in this interval.
    X_min = X[idx,]
    X_max = X[idx,]
    X_min = replace(X_min, s, i1)
    X_max = replace(X_max, s, i2)
    
    # And get the new predictions
    y_min = predict(model, X_min)
    y_max = predict(model, X_max)
    
    # Calculate difference now
    diff = y_max - y_min
    uncentered_ale_k = sum(diff) / length(idx)
    return(uncentered_ale_k)
  }
  # Use mapply() to loop through each interval
  result = mapply(FUN = uncentered_ale_calculation, lowerbound, upperbound)
  
  # Now accumulate
  uncentered_ale = cumsum(result)
  
  if (centered == TRUE) {
    # Center ALE here
    centered_ale = c()
    mean_uncentered_ale = sum(uncentered_ale) / length(uncentered_ale)
    
    for (uncentered_ale_k in uncentered_ale) {
      centered_ale_k = uncentered_ale_k - mean_uncentered_ale
      centered_ale = c(centered_ale, centered_ale_k)
    }
    # Return the final results of bounds and
    # centered_ale/uncentered_ale as a list.
    return(list(bounds = bounds, ale = centered_ale))
  }
  else{
    return(finalResult = list(bounds = bounds, ale = uncentered_ale))
  }
}

prepare_ale = function(model, X, s, n_intervals = 100, centered = TRUE) {
  #' Uses `calculate_ale` to prepare x and y data, which can be used
  #' by matplotlib directly.
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
  
  # Calculate ALE first.
  ale = calculate_ale(model, X, s, n_intervals, centered)
  
  # Get the bounds and y from the list.
  bounds = array(unlist(ale[1]))
  y = array(unlist(ale[2]))
  
  # Set up lowerbound and upperbound.
  boundsLength = length(bounds)
  lowerbound = bounds[1:boundsLength - 1]
  upperbound = bounds[2:boundsLength]
  
  # Get x for the centers of two bounds.
  x = rowMeans(cbind(lowerbound, upperbound))
  
  # Return the final result as a list.
  finalResult = data.frame(x = x,
                      y = y)
  
  return(finalResult)
  
}

if (FALSE) {
  library(randomForest)
  library(ggplot2)
  
  # Set up your working directory using swd() and get the dataset file. 
  df = read.csv(file = 'datasets/wheat_seeds.csv')
  
  # Split the dataset to 70% train data and 30% test data.
  set.seed(100)
  train = sample(nrow(df), 0.7 * nrow(df), replace = FALSE)
  trainData = df[train, ]
  testData = df[-train, ]
  
  # Normalize the target to be between 0 and 1.
  min_max_norm = function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  
  trainData$Type = min_max_norm(trainData$Type)
  
  # Build a Random Forest model.
  model = randomForest(Type ~ ., data = trainData, mtry = 4, importance = TRUE)
  
  # Use the first feature from the dataset and set up
  # 4 intervals to test the get_bounds function
  bounds = get_bounds(df, 1, 4)
  
  # Test the calculate_ale function, with centered = FALSE
  uncentered_ale = calculate_ale(model, df, 1, 4, FALSE)
  
  #Test the prepare_ale function, with centered = TRUE
  prepared_ale = prepare_ale(model, df, 1, 4, TRUE)
  
  ggplot(data = prepared_ale, mapping = aes(x = x, y = y)) + 
    geom_line() + 
    geom_point()
}

