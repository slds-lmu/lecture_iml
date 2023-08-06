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
