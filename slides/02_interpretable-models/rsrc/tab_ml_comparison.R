library("mlr3verse")
library("mlr3learners")
library(xtable)

tsks = tsks(c("bike_sharing", "boston_housing"))
tsks$bike_sharing$select(setdiff(tsks$bike_sharing$feature_names, c("date", "season", "weather")))
tsks$boston_housing$select(setdiff(tsks$boston_housing$feature_names, c("chas", "town")))
lrns = lrns(c("regr.lm", "regr.rpart", "regr.ranger", "regr.xgboost"))
rsmp = rsmps("cv", folds = 5)

design = benchmark_grid(tsks, lrns, rsmp)

bmr = benchmark(design)
print(bmr)

# performance
acc = bmr$aggregate(msrs(c("regr.rmse", "regr.rsq")))
result = acc[, .(task_id, learner_id, regr.rmse, regr.rsq)]

# rename
names(result) = c("task", "Model", "RMSE", "R2")
result$Model = rep(c("LM", "Tree", "Random Forest", "Boosting"), 2)
result[, 3:4] = round(result[, 3:4], 2)

# create tables
xtable(result[1:4, -1], type = "latex", tabular.environment="longtable")
xtable(result[5:8, -1], type = "latex", tabular.environment="longtable")
