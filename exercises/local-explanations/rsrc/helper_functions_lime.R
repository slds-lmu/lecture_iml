library("docstring")
library("ggplot2")
library("rpart")

get_grid = function(model, dataset, points_per_feature = 50) {
  #' Retrieve grid data for plotting a two-dimensional graph with 
  #' `points_per_feature` for each axis. The space is created by 
  #' the hyperparameters' lower and upper values. Only the first two input
  #' labels are used.
  
  #' @param model: Classifier which can call a predict method.
  #' @param dataset (data.frame): Input dataset (only contains two features).
  #' @param points_per_feature (integer(1)): How many points in each dimension.
  
  #' @return  Dataframe with three columns: 
  #'      - equidistant grid of first feature
  #'      - equidistant grid of second feature
  #'      - pred: prediction values for given feature input
  
  range_x1 = range(dataset[,1])
  range_x2 = range(dataset[,2])
  
  x1 = seq(range_x1[1], range_x1[2], length.out = points_per_feature)
  x2 = seq(range_x2[1], range_x2[2], length.out = points_per_feature)
  
  X = data.frame(expand.grid(x1, x2))
  names(X) = names(dataset)
  pred = predict(model, X, type = "class")
  
  return(data.frame(X, pred = pred))
  
}

plot_grid = function(grid) {
  #'  Uses the grid data to add a color grid to the plot.
  #'
  #'  @param grid (data.frame): Grid data for plot.
  #'
  #'  @return ggplot: grid data plotted with coloring displaying the prediction 
  #'  surface.
  
  xnam = names(grid)[1]
  ynam = names(grid)[2]
  ggplot(grid, aes_string(x = xnam, y = ynam, fill = "pred")) + 
    ggplot2::geom_tile() +
    ggplot2::guides(z = ggplot2::guide_legend(title = "pred")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "right")
}


plot_points_in_grid = function(plt, df, weights = NULL, x_interest = NULL, 
                               size = 4L) {
  #' Given a plot, add scatter points from `df` and `x_interest`.
  #' 
  #' @param plt (ggplot): Plot with color grid.
  #' @param df (data.frame): Points which should be added to the plot.
  #' @param weights (numeric): Normalized weights with elements equal to
  #' the number of rows in `df`. Weights are used to determine the size of the 
  #' points in the plot. If NULL, size of all points are equal.
  #' @param x_interest (data.frame): Single point (one row dataset) 
  #' whose prediction we want to explain. If NULL (default) no point is added.
  #' @param size (numeric(1)): Default size of the points. Default 4L.
  #' 
  
  if (!is.null(weights)) {
    w = weights
  } else {
    w = 1L
  }
  xnam = names(df)[1]
  ynam = names(df)[2]
  plt = plt + 
    geom_point(mapping = aes_string(x = xnam, y = ynam, color = "pred"), 
               size = w*size, data = df, alpha = 2) + 
    scale_colour_hue(l = 40)
  
  if (!is.null(x_interest)) {
    x_interest$pred = "1"
    plt = plt + geom_point(mapping = aes_string(x = xnam, y = ynam), 
                           x_interest, colour = "red")
  }
  return(plt)
}


sample_points = function(model, dataset, num_points, seed=0) {
  
  #' Samples points for the two first features and uses the model to 
  #' receive a prediction for these sampled points.
  #' 
  #'  @param model: Classifier which can call a predict method.
  #' 	@param dataset (data.frame): Input dataset (only contains two features).
  #'  @param num_points (int): How many points should be sampled.
  #'  @param seed (int): Seed to feed random.
  #' 
  #' @return dataset (data.frame) of sampled data including a column 'pred' with 
  #' the obtained prediction of the model for the sampled data.
  
  set.seed(seed)
  range_x1 = range(dataset[, 1])
  range_x2 = range(dataset[, 2])
  
  x1 = runif(n = num_points, min = range_x1[1], max = range_x1[2])
  x2 = runif(n = num_points, min = range_x2[1], max = range_x2[2])
  Z = data.frame(x1, x2)
  names(Z) = names(dataset)
  pred = predict(model, Z)
  
  return(data.frame(Z, pred))
}


weight_points = function(x_interest, df, kernel_width=0.2) {
  #' For every x in `df` returns a weight depending on the exponential kernel 
  #' distance to `x_interest`.
  #' 
  #' @param x_interest (data.frame): Single point (one row dataset)
  #' whose prediction we want to explain.
  #' @param df (data.frame): Data which needs to be weighted 
  #' (later used for surrogate model).
  #' @param kernel_width (float): Kernel width for exponential kernel.
  #' 
  #' @return weights (numeric): Normalized weights between 
  #' 0..1 for all datapoints in df.
  
  if ("pred" %in% names(df)) {
    df = df[names(df) != "pred"]
  } 
  
  df = as.matrix(df)
  weights = apply(df, MARGIN = 1, FUN = function(x) {
    eucldist = sqrt(sum((x-x_interest)^2))
    exp(-eucldist/(kernel_width*kernel_width))
  })
  
  # Normalize between 0 and 1
  weights = (weights - min(weights)) / (max(weights) - min(weights))
  
  return(weights)
}

fit_explainer_model = function(df, weights = NULL, seed = 0) {
  #' Fits a decision tree to the weighted data
  #' 
  #' @param df (data.frame): Data for surrogate model, must include an outcome
  #'  variable `pred`.
  #' @param weights (numeric): Normalized weights with number of elements equal 
  #' to number of rows of `df`.
  #' @param seed (int): Seed for the decision tree.
  #' 
  #' @return model (rpart): Fitted explainer model.
  set.seed(seed)
  xnam = names(df)[1]
  ynam = names(df)[2]
  form = formula(paste("pred ~", xnam, "+", ynam))
  tree = rpart(form, weights = weights, data = df)
  return(tree)
}