# PREREQ -----------------------------------------------------------------------
library("iml")
library("mlr3")
library("mlr3verse")
library("ggplot2")
library("iml")
library("dplyr")
theme_set(theme_bw())

#setwd('~/university/phd/2021/teaching/lecture_iml/slides/feature-importance/rsrc')

# DATA -----------------------------------------------------------------------
set.seed(123)

nvars = 20
nobs = 10**4
ntrain = 50
target = 'X21'

noise = data.frame(replicate(nvars + 1, runif(nobs, -10, 10)))
task = TaskRegr$new(id = 'noise', backend = noise, target = target)
task
learner = lrn("regr.xgboost")

train_set = sample(task$nrow, ntrain)
test_set = setdiff(seq_len(task$nrow), train_set)

learner$train(task, row_ids = train_set)
prediction = learner$predict(task, row_ids = train_set)
prediction$score()
prediction = learner$predict(task, row_ids = test_set)
prediction$score()

predictor_train = Predictor$new(learner, noise[train_set,], y=target)
predictor_test = Predictor$new(learner, noise[test_set,], y=target)

imp_test <- FeatureImp$new(predictor_test,loss = "mae", n.repetitions = 10, compare='difference')

plot(imp_test)
mean(imp_test$results$importance)

imp_train <- FeatureImp$new(predictor_train,loss = "mae", n.repetitions = 10,  compare='difference')

plot(imp_train)
hist(imp_train$results$importance)

imp_train_res = imp_train$results[order(imp_train$results$feature), ]
imp_test_res = imp_test$results[order(imp_test$results$feature), ]
features = imp_train$results$feature[order(imp_train$results$feature)]

importance = c(imp_test_res$importance, imp_train_res$importance)
q95 = c(imp_test_res$importance.95, imp_train_res$importance.95)
q05 = c(imp_test_res$importance.05, imp_train_res$importance.05)
type = c(rep('PFI on test data', nvars), rep('PFI on train data', nvars))
colname = rep(features, 2)

results = data.frame(importance=importance, type=type, q95=q95, q05=q05, colname=colname)
#write.csv(results, 'results.csv')
#results = read.csv('results.csv')

# PLOT -----------------------------------------------------------------------
# TODO add error bars http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
p = ggplot(data=results, aes(x=reorder(colname, -importance), y=importance, fill=reorder(type, -importance)))
p = p + geom_bar(stat='identity', position=position_dodge())
p = p + geom_errorbar(aes(ymin=q05, ymax=q95), width=.2, position=position_dodge(.9))
p = p + labs(x='Feature', fill='IML method', y='Score')
p = p + theme_bw()
p

#ggplot(results, aes(x=type, y=importance)) + geom_boxplot()
ggsave('../figure_man/pfi_test_vs_train.pdf', width=8, height=2)

