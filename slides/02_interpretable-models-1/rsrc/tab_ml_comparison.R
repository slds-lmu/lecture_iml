library("mlr3verse")
library("mlr3learners")
library("xtable")
set.seed(1)

load("../../../data/bike.RData")
bike_task = as_task_regr(bike, target = "cnt")
# bh_task = tsk("boston_housing")
# bh_task$select(setdiff(bh_task$feature_names, c("town")))
# tsks = c(bike_task, bh_task)
#tsks$bike_sharing$select(setdiff(tsks$bike_sharing$feature_names, c("date", "season", "weather")))
tsks = list(bike_task)

lrn = lrn("regr.xgboost")
lrn$param_set$values$nrounds = 100 # fix nrounds for speed
# tune the following hyperpars
lrn$param_set$values$eta = to_tune(0.01, 0.4, logscale = TRUE)
lrn$param_set$values$subsample = to_tune(0.5, 1)
lrn$param_set$values$colsample_bytree = to_tune(0.5, 1)

# combine xgb with one-hot encoding to support factor features
xgb = as_learner(po("encode") %>>% lrn)

# create auto-tuned xgboost
xgb_auto = auto_tuner(
  method = tnr("random_search", batch_size = 4L),
  learner = xgb,
  resampling = rsmp("holdout"),
  measure = msr("regr.mse"),
  term_evals = 30)

lrns = c(lrns(c("regr.lm", "regr.rpart", "regr.ranger")), xgb_auto)
rsmp = rsmps("cv", folds = 4)

design = benchmark_grid(tsks, lrns, rsmp)

future::plan("multicore", workers = 4L)
bmr = benchmark(design)
print(bmr)

# performance
acc = bmr$aggregate(msrs(c("regr.rmse", "regr.rsq")))
result = acc[, .(task_id, learner_id, regr.rmse, regr.rsq)]

# rename
names(result) = c("task", "Model", "RMSE", "R2")
result$Model = c("LM", "Tree", "Random Forest", "Boosting")
#result[, 3:4] = round(result[, 3:4], 2)

# create tables
print(xtable(result[1:4, -1], type = "latex", tabular.environment = "longtable"),  include.rownames = FALSE)
#xtable(result[5:8, -1], type = "latex", tabular.environment = "longtable")
