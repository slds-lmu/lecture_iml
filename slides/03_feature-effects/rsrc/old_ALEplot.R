## R code for Example 4
## Load relevant packages
library(ALEPlot)
## Generate some data and fit a neural network supervised learning model
n = 5000
x <- runif(n, min = 0, max = 1)
x1 <- x + rnorm(n, 0, 0.05)
x2 <- x + rnorm(n, 0, 0.05)
y = x1 + x2^2 + rnorm(n, 0, 0.1)
DAT = data.frame(y, x1, x2)

test.ind = sample(1:n, size = 0.5*n)
test = DAT[test.ind, ]
DAT = DAT[-test.ind, ]

set.seed(1)
library(mlr)
lrn = makeLearner("regr.nnet", skip = F, size = 10,
  decay = 0.0001, maxit = 1000, trace = F)

tsk = makeRegrTask(data = DAT, target = "y")

lrn.list = list(lrn,
  makeLearner("regr.featureless"),
  makeLearner("regr.lm"),
  makeLearner("regr.ranger"))
bench = benchmark(lrn.list, tsk, resamplings = cv3)
bench

lrn = makeLearner("regr.ranger")
mod = mlr::train(lrn, tsk)
# plotLearnerPrediction(lrn, task = tsk)
mseval = performance(predict(mod, newdata = test))
mseval

df = expand.grid(
  x1 = seq(min(test$x1), max(test$x1), length = 50),
  x2 = seq(min(test$x2), max(test$x2), length = 50)
)
df$y = predict(mod, newdata = df)$data$response

library(ggplot2)
surf = ggplot(data = df, aes(x = x1, y = x2, z = y)) +
  geom_contour_filled() +
  geom_point(data = test, aes(x1, x2)) +
  ggtitle(paste0("performance: ", mseval)) +
  NULL

library(iml)
pred = Predictor$new(mod, data = test)
fnames = c("x1", "x2")
ale = lapply(fnames, function(x)
  FeatureEffect$new(pred, feature = x, grid.size = 30)
)
pdp = lapply(fnames, function(x) {
  eff = FeatureEffect$new(pred, feature = x, grid.size = 30, method = "pdp")
  # center like aleplots, taken from ALEPlot package
  xmin = min(test[[x]])
  xgridval = eff$results[,1]
  a = cut(test[[x]], breaks = c(xmin - (xgridval[2] - xgridval[1]), xgridval),
    include.lowest = TRUE)
  b = as.numeric(table(a))
  eff$results$.value = eff$results$.value - sum(eff$results$.value * b)/sum(b)

  #eff$results$.value = eff$results$.value - mean(eff$results$.value)
  return(eff)
  }
)
pdpcenter = lapply(fnames, function(x) {
  eff = FeatureEffect$new(pred, feature = x, grid.size = 30, method = "pdp")
  eff$results$.value = eff$results$.value - mean(eff$results$.value)
  return(eff)
}
)

ale1 = ale[[1]]$plot() + ylab("") +
  geom_function(fun = function(x) x - 0.5, col = "red") +
  ggtitle("ALE x1")

ale2 = ale[[2]]$plot() + ylab("") +
  geom_function(fun = function(x) x^2 - (1/3+0.05^2), col = "red") +
  ggtitle("ALE x2")

pdp1 = pdp[[1]]$plot() + ylab("") +
  geom_function(fun = function(x) x - 0.5, col = "red") +
  ggtitle("PDP x1")

pdp2 = pdp[[2]]$plot() + ylab("") +
  geom_function(fun = function(x) x^2 - (1/3+0.05^2), col = "red") +
  ggtitle("PDP x2")

#pp = FeatureEffect$new(pred, feature = c("x1", "x2"), grid.size = 20, method = "pdp")
#pp$plot() + geom_point(data = test, aes(x = x1, y = x2))

library(patchwork)
p = surf | ((ale1 + ale2) / (pdp1 + pdp2)) #surf / (ale1 + pdp1) / (ale2 + pdp2)
p + plot_layout(heights = c(3, 2, 2), guides = "collect") & theme(legend.position = 'bottom') #& guides(fill=guide_legend(nrow = 1, byrow = TRUE))

library(nnet)
mod <- nnet(y ~., data = DAT, linout = T, skip = F, size = 10, decay = 0.0001, maxit = 1000, trace = F)
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata, type= "raw") )

# library(ranger)
# mod = ranger(y~., data = DAT)
# yhat <- function(X.model, newdata) predict(X.model, data = newdata)$prediction

library(mlr3verse)
#learner = lrn("regr.svm", type = "eps-regression", kernel = "radial")
#learner$param_set$values$gamma = to_tune(p_dbl(1e-4, 1e4, logscale = TRUE))
#learner$param_set$values$cost = to_tune(p_dbl(1e-4, 1e4, logscale = TRUE))

learner = lrn("regr.kknn")
learner$param_set$values$k = to_tune(3, 30)
learner$param_set$values$distance = to_tune(0, 3)

autolrn = AutoTuner$new(
  learner = learner,
  resampling = rsmp("cv", folds = 2),
  measure = msr("regr.mse"),
  terminator = trm("evals", n_evals = 100),
  tuner = tnr("random_search"),
  #search_space = searchspace, # use to_tune in learner!
  store_tuning_instance = TRUE)
task = TaskRegr$new("X", backend = DAT, "y")
mod = autolrn$train(task)
p = mod$predict_newdata(newdata = test)$score(msr("regr.mse"))
p
yhat <- function(X.model, newdata) X.model$predict_newdata(newdata = newdata)$data$response





yhat <- function(X.model, newdata) predict(X.model, newdata = newdata)$data$response

## Calculate and plot the ALE and PD main effects of x1 and x2
par(mfrow = c(2, 2), mar = c(4,4,2,2) + 0.1)
ALE.1 = ALEPlot(DAT[,2:3], mod, pred.fun = yhat, J = 1, K = 50, NA.plot = TRUE)
PD.1 = PDPlot(DAT[,2:3], mod, pred.fun = yhat, J = 1, K = 50)
ALE.2 = ALEPlot(DAT[,2:3], mod, pred.fun = yhat, J = 2, K = 50, NA.plot = TRUE)
PD.2 = PDPlot(DAT[,2:3], mod, pred.fun = yhat, J = 2, K = 50)

## Manually plot the ALE main effects on the same scale for easier
## comparison of the relative importance of the four predictor variables
## We also plot the true linear and quadratic effects in black for reference
plot(ALE.1$x.values, ALE.1$f.values, type="l", xlab="x1",
  ylab="ALE_main_x1", xlim = c(0,1), ylim = c(-1,1), col = "blue", main = "(a)")
curve(x - 0.5, from = 0, to = 1, add = TRUE)
plot(PD.1$x.values, PD.1$f.values, type="l", xlab="x1",
  ylab="PD_x1", xlim = c(0,1), ylim = c(-1,1), col = "blue", main = "(b) ")
curve(x - 0.5, from = 0, to = 1, add = TRUE)
plot(ALE.2$x.values, ALE.2$f.values, type="l", xlab="x2",
  ylab="ALE_main_x2", xlim = c(0,1), ylim = c(-1,1), col = "blue", main = "(c) ")
curve(x^2 - (1/3+0.05^2), from = 0, to = 1, add = TRUE)
plot(PD.2$x.values, PD.2$f.values, type="l", xlab="x2",
  ylab="PD_x2", xlim = c(0,1), ylim = c(-1,1), col = "blue", main = "(d) ")
curve(x^2 - (1/3+0.05^2), from = 0, to = 1, add = TRUE)
