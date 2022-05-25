library("docstring")
library("ggplot2")
library("rpart")

get_grid = function(model, dataset, points_per_feature = 50) {
  #' Retrieve grid data for plotting a two-dimensional graph with `points_per_feature` for each axis.
  #' The space is created by the hyperparameters' lower and upper values. Only the first two input
  #' labels are used.
  
  #' @param model: Classifier which can call a `predict()` method.
  #' @param dataset (data.frame): Input dataset (only contains two features).
  #' @param points_per_feature: How many points in each dimension.
  
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
  pred = predict(model, X)
  
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


sample_points = function(model, dataset, num_points, seed=0) {
  
  #' Samples points for the two first features. Uses the bounds from configspace again.
  #' 
  #' Parameters:
  #'     model: Classifier which can call a predict method.
  #' @param dataset (data.frame): Input dataset (only contains two features).
  #'     num_points (int): How many points should be sampled.
  #'     seed (int): Seed to feed random.
  #' 
  #' Returns:
  #'     X (np.ndarray): Data with shape (`num_points`, 2)
  #'     y (np.ndarray): Target values with shape (`num_points`,)
  
  set.seed(seed)
  range_x1 = range(dataset[, 1])
  range_x2 = range(dataset[, 2])
  
  x1 = runif(n = num_points, min = range_x1[1], max = range_x1[2])
  x2 = runif(n = num_points, min = range_x2[1], max = range_x2[2])
  X = data.frame(x1, x2)
  names(X) = names(dataset)
  pred = predict(model, X)
  
  return(data.frame(X, pred))
}


weight_points = function(x_interest, df, kernel_width=0.2) {
  #'For every x in `df` returns a weight depending on the exponential kernel distance to `x_interest`.
  #' 
  #' @param x_interest (data.frame): Single point (one row dataset) whose prediction we want to explain.
  #' @param df (data.frame): Data which needs to be weighted (later used for surrogate model).
  #' @param kernel_width (float): Kernel width for exponential kernel.
  #' 
  #' @return weights (numeric): Normalized weights between 0..1 for all datapoints in df.

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

plot_points_in_grid = function(plt, df, weights = NULL, x_interest = NULL, size = 4L) {
  #' Given a plot, add scatter points from `df` and `x_interest`.
  #' 
  #' @param plt (ggplot): Plot with color grid.
  #' @param df (data.frame): Points which should be added to the plot.
  #' @param weights (numeric): Normalized weights with elements equal to the number of rows in `df`. 
  #' Weights are used to determine the size of the points in the plot. If NULL, size of all points are equal.
  #' @param x_interest (data.frame): Single point (one row dataset) whose prediction we want to explain. 
  #' If NULL no point is added.
  #' @param size (numeric(1)): Default size of the points. Default 4L.
  #' 

  if (!is.null(weights)) {
    w = weights
  } else {
    w = 1L
  }
  xnam = names(df)[1]
  ynam = names(df)[2]
  plt = plt + geom_point(mapping = aes_string(x = xnam, y = ynam, color = "pred"), size = w*size, data = df, alpha = 2) + 
    scale_colour_hue(l = 40)
  
  if (!is.null(x_interest)) {
    x_interest$pred = "1"
    plt = plt + geom_point(mapping = aes_string(x = xnam, y = ynam), x_interest, colour = "red")
  }
  return(plt)
}



fit_explainer_model = function(df, weights = NULL, seed = 0) {
  #' Fits a decision tree to the weighted data
  #' 
  #' @param df (data.frame): Data for surrogate model, must include an outcome variable `pred`.
  #' @param weights (numeric): Normalized weights with number of elements equal to number of rows of `df`.
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



if (FALSE) {
  library("e1071") # SVM 
  
  dataset = read.csv(file = "datasets/wheat_seeds.csv")
  dataset$Type = as.factor(dataset$Type)
  table(dataset$Type)
  
  min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  
  dataset  = dataset[c("Perimeter", "Asymmetry.Coeff", "Type")]
  dataset$Perimeter = min_max_norm(dataset$Perimeter)
  dataset$Asymmetry.Coeff = min_max_norm(dataset$Asymmetry.Coeff)
  
  # Fit a random forest to the data
  mod = svm(Type ~ ., data = dataset)
  dataset$Type = NULL
  
  # Compute counterfactual for first observation
  x_interest = dataset[1,]
  
  print("Run 'get_grid' ...")
  grid = get_grid(model = mod, dataset = dataset, points_per_feature = 50L)
  plot = plot_grid(grid)
  samp = sample_points(model = mod, dataset = dataset, num_points = 50L)
  w = weight_points(x_interest = x_interest, df = samp, kernel_width = 0.2)
  plot_points_in_grid(plt = plot, df = samp, weights = w, x_interest = x_interest)
  fit_explainer_model(df = samp, weights = w)
}