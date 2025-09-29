library("docstring")
library("ggplot2")
library("rpart")

get_grid <- function(model, dataset, points_per_feature = 50) {
  #' Retrieve grid data for plotting a two-dimensional graph with `points_per_feature` for each axis.
  #' The space is created by the lower and upper values of the respective feature. Only the first two input
  #' labels are used.
  
  #' @param model: Classifier which can call a predict method.
  #' @param dataset (data.frame): Input dataset (only contains two features).
  #' @param points_per_feature (integer(1)): How many points in each dimension.
  
  #' @return  data.frame with three columns: 
  #'      - equidistant grid of first feature
  #'      - equidistant grid of second feature
  #'      - pred: prediction values for given feature input
  
  range_x1 <- range(dataset[, 1])
  range_x2 <- range(dataset[, 2])
  
  x1 <- seq(range_x1[1], range_x1[2], length.out = points_per_feature)
  x2 <- seq(range_x2[1], range_x2[2], length.out = points_per_feature)
  
  X <- data.frame(expand.grid(x1, x2))
  names(X) <- names(dataset)
  pred <- predict(model, X, type = "class")
  
  data.frame(X, pred = pred)
}

plot_grid <- function(grid) {
  #'  Uses the grid data to add a color grid to the plot.
  #'
  #'  @param grid (data.frame): Grid data for plot.
  #'
  #'  @return ggplot: grid data plotted with coloring displaying the prediction 
  #'  surface.
  
  xnam <- names(grid)[1]
  ynam <- names(grid)[2]
  ggplot(grid, aes_string(x = xnam, y = ynam, fill = "pred")) + 
    geom_tile() +
    guides(z = guide_legend(title = "pred")) +
    theme_bw() +
    theme(legend.position = "right")
}

plot_points_in_grid <- function(plt, df, weights = NULL, x_interest = NULL, size = 4L) {
  #' Given a plot, add scatter points from `df` and `x_interest`.
  #' 
  #' @param plt (ggplot): Plot with color grid.
  #' @param df (data.frame): Points which should be added to the plot.
  #' @param weights (numeric): Normalized weights with elements equal to the number of rows in `df`. 
  #' Weights are used to determine the size of the points in the plot. If NULL, size of all points are equal.
  #' @param x_interest (data.frame): Single point (one row dataset) whose prediction we want to explain. 
  #' If NULL (default) no point is added.
  #' @param size (numeric(1)): Default size of the points. Default 4L.
  #' 
  
  if (!is.null(weights)) {
    w <- weights
  } else {
    w <- 1L
  }
  
  xnam <- names(df)[1]
  ynam <- names(df)[2]
  plt <- plt + geom_point(mapping = aes_string(x = xnam, y = ynam, color = "pred"), size = w*size, data = df, alpha = 2) + 
    scale_colour_hue(l = 40)
  
  if (!is.null(x_interest)) {
    x_interest$pred <- "1"
    plt <- plt + geom_point(mapping = aes_string(x = xnam, y = ynam), x_interest, colour = "red")
  }
  
  plt
}

sample_points <- function(model, dataset, num_points, seed = 0) {
  
  #' Samples points for the two first features and uses the model to 
  #' receive a prediction for these sampled points.
  #' 
  #'  @param model: Classifier which can call a predict method.
  #' 	@param dataset (data.frame): Input dataset (only contains two features).
  #'  @param num_points (int): How many points should be sampled.
  #'  @param seed (int): Seed to feed random.
  #' 
  #' @return dataset (data.frame) of sampled data (feature) plus a column 'pred' with 
  #' their obtained prediction of the `model`.
  
  return(NULL)
}

weight_points = function(x_interest, df, kernel_width = 0.2) {
  #' For every x in `df` returns a weight depending on the exponential kernel distance to `x_interest`.
  #' 
  #' @param x_interest (data.frame): Single point (one row dataset) whose prediction we want to explain.
  #' @param df (data.frame): Data which needs to be weighted (later used for surrogate model).
  #' @param kernel_width (float): Kernel width for exponential kernel.
  #' 
  #' @return weights (numeric): Normalized weights between 0 and 1 for all data points in df.
  
  return(NULL)
}

fit_explainer_model <- function(df, weights = NULL, seed = 0) {
  #' Fits a decision tree to the weighted data
  #' 
  #' @param df (data.frame): Data for surrogate model, must include an outcome variable `pred`.
  #' @param weights (numeric): Normalized weights with number of elements equal to number of rows of `df`.
  #' @param seed (int): Seed for the decision tree.
  #' 
  #' @return model (rpart): Fitted explainer model.
  
  return(NULL)
}

if (FALSE) {
  set.seed(2022L)
  library("e1071") # SVM 
  library("gridExtra") # to plot two ggplots next to each other
  
  dataset <- read.csv(file = "datasets/wheat_seeds.csv")
  dataset$Type <- as.factor(dataset$Type)
  table(dataset$Type)
  
  min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  
  dataset  <- dataset[c("Perimeter", "Asymmetry.Coeff", "Type")]
  dataset$Perimeter <- min_max_norm(dataset$Perimeter)
  dataset$Asymmetry.Coeff <- min_max_norm(dataset$Asymmetry.Coeff)
  
  traindata <- dataset[sample(seq_len(nrow(dataset)), round(0.6*nrow(dataset)), replace = TRUE),]
  
  # Fit a SVM to the data
  mod <- svm(Type ~ ., data = traindata)
  dataset$Type = NULL
  
  # Compute counterfactual for first observation
  x_interest <- data.frame(Perimeter = 0.31, Asymmetry.Coeff = 0.37)
  
  # Parameters for method
  points_per_feature <- 50L
  n_points <- 1000L
  
  print("Run 'get_grid' ...")
  grid <- get_grid(model = mod, dataset = dataset, points_per_feature = points_per_feature)
  
  print("Run `plot_grid` ...")
  plot <- plot_grid(grid)
  plot
  
  print("Run `sample_points` ...")
  samp <- sample_points(model = mod, dataset = dataset, num_points = n_points)
  
  print("Run `plot_points_in_grid` ...")
  plot_points_in_grid(plt = plot, df = samp, size = .5)
  
  print("Run `weight_points` ...")
  w <- weight_points(x_interest = x_interest, df = samp, kernel_width = 0.2)
  
  print("Run `plot_points_in_grid` ...")
  plot_points_in_grid(plt = plot, df = samp, weights = w, x_interest = x_interest)
  
  print("Run `fit_explainer_model` ...")
  explainer <- fit_explainer_model(df = samp, weights = w)
  
  print("Compare models ...")
  plt1 <- plot_points_in_grid(plt = plot, df = samp, x_interest = x_interest, size = .5)
  grid2 <- get_grid(model = explainer, dataset = dataset, points_per_feature = points_per_feature)
  plot2 <- plot_grid(grid2)
  plt2 <- plot_points_in_grid(plt = plot2, df = samp, x_interest = x_interest, size = .5)
  
  plt1 <- plt1 + ggtitle("SVM")
  plt2 <- plt2 + ggtitle("Decision Tree Explainer")
  grid.arrange(plt1, plt2, ncol = 2L)
}
