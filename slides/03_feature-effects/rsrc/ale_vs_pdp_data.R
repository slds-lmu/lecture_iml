# PREREQ -----------------------------------------------------------------------
library(ALEPlot)
library(mlr)
library(iml)
library(ggplot2)
library(patchwork)
library(devtools)
#source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# DATA -------------------------------------------------------------------------

simData = function(n) {
  x = runif(n, min = 0, max = 1)
  x1 = x + rnorm(n, 0, 0.05)
  x2 = x + rnorm(n, 0, 0.05)
  y = x1 + x2^2 + rnorm(n, 0, 0.2)
  return(data.frame(y, x1, x2))
}

set.seed(1)
n = 5000
DAT = simData(n)

#test.ind = sample(1:n, size = 0.5*n)
test = simData(100000) #[test.ind, ]
#DAT = DAT[-test.ind, ]

#breaks = c(-Inf, seq(min(DAT$y), max(DAT$y), by = 0.5), Inf)
breaks = c(-Inf, seq(0, 2, by = 0.5), Inf)

tsk = makeRegrTask(data = DAT, target = "y")

# NN MODEL ------------------------------------------------------------------------

lrn.nn = makeLearner("regr.nnet", skip = F, size = 10,
                     decay = 0.0001, maxit = 1000, trace = F)
mod.nn = mlr::train(lrn.nn, tsk)

nn.eval = performance(predict(mod.nn, newdata = test))

# lrn.list = list(lrn,
#   makeLearner("regr.featureless"),
#   makeLearner("regr.lm"),
#   makeLearner("regr.ranger"))
# bench = benchmark(lrn.list, tsk, resamplings = cv3)
# bench
#m = mod.nn$learner.model
#m$call$formula = y~x1+x2
#plot(m)


pred = Predictor$new(mod.nn, data = DAT)
# plotLearnerPrediction(lrn, task = tsk)

df = expand.grid(
  x1 = seq(min(DAT$x1), max(DAT$x1), length = 50),
  x2 = seq(min(DAT$x2), max(DAT$x2), length = 50)
)
df$y = predict(mod.nn, newdata = df)$data$response

# Create RF MODEL ------------------------------------------------------------------------

#lrn.rf = makeLearner("regr.ranger")
#mod.rf = mlr::train(lrn.rf, tsk)

lrn.lm = makeLearner("regr.lm")
mod.lm = mlr::train(lrn.lm, tsk)
mod.lm$learner.model = lm(y ~ x1 + poly(x2, degree = 2, raw = TRUE), data = DAT)

lm.eval = performance(predict(mod.lm, newdata = test))

pred = Predictor$new(mod.lm, data = DAT)

df = expand.grid(
  x1 = seq(min(DAT$x1), max(DAT$x1), length = 50),
  x2 = seq(min(DAT$x2), max(DAT$x2), length = 50)
)
df$y = predict(mod.lm, newdata = df)$data$response
