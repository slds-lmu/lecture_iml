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