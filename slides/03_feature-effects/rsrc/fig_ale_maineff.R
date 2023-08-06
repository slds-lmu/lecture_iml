# PREREQ -----------------------------------------------------------------------

library(mlr3)
library(mlr3learners)
library(paradox)
library(mlr3tuning)
library(ALEPlot)
source("ale_dat.R")

# DATA -------------------------------------------------------------------------

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
autolrn$train(task)
pred = autolrn$predict_newdata(newdata = test)$score(msr("regr.mse"))

yhat <- function(X.model, newdata) predict(X.model, newdata = newdata)

# PLOT -------------------------------------------------------------------------

pdf("../figure/ale_maineff.pdf")

par(mfrow = c(2, 2), mar = c(4,4,2,2) + 0.1)
ALE.1 = ALEPlot(DAT[,2:3], autolrn, pred.fun = yhat, J = 1, K = 50, NA.plot = TRUE)
PD.1 = PDPlot(DAT[,2:3], autolrn, pred.fun = yhat, J = 1, K = 50)
ALE.2 = ALEPlot(DAT[,2:3], autolrn, pred.fun = yhat, J = 2, K = 50, NA.plot = TRUE)
PD.2 = PDPlot(DAT[,2:3], autolrn, pred.fun = yhat, J = 2, K = 50)

dev.off()

