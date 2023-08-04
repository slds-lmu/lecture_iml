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
