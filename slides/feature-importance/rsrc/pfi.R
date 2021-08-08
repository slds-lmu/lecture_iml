library("iml")
library("mlr3")
library("mlr3verse")
library("ggplot2")
library("iml")
library("dplyr")

setwd('~/university/phd/2021/teaching/lecture_iml/slides/feature-importance/rsrc')

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
library("ggplot2")
plot(imp_test)
mean(imp_test$results$importance)

imp_train <- FeatureImp$new(predictor_train,loss = "mae", n.repetitions = 10,  compare='difference')
library("ggplot2")
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
write.csv(results, 'results.csv')
results = read.csv('results.csv')


# TODO add error bars http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
p = ggplot(data=results, aes(x=reorder(colname, -importance), y=importance, fill=reorder(type, -importance))) 
p = p + geom_bar(stat='identity', position=position_dodge())
p = p + geom_errorbar(aes(ymin=q05, ymax=q95), width=.2, position=position_dodge(.9))
p = p + labs(x='Feature', fill='IML method', y='Score')
p = p + theme_bw()
p

#ggplot(results, aes(x=type, y=importance)) + geom_boxplot()
ggsave('../figure_man/pfi_test_vs_train.pdf', width=7, height=1.5)



# extrapolation

n = 1000
ntrain = 0.7 * n

x1 = rnorm(n)
x2 = x1 + rnorm(n, sd=0.01)
x3 = rnorm(n)
x4 = rnorm(n)
y = x3 + rnorm(n, sd=0.1)

data = data.frame(x1=x1, x2=x2, x3=x3, x4=x4, y=y)
task = TaskRegr$new(id='correlated', backend=data, target='y')
learner = lrn('regr.lm')

train_set = sample(task$nrow, ntrain)
test_set = setdiff(seq_len(task$nrow), train_set)

learner$train(task, row_ids = train_set)
learner$model

predictor_test = Predictor$new(learner, data[test_set,], y='y')

imp_test <- FeatureImp$new(predictor_test,loss = "mae", n.repetitions = 10, compare='difference')
library("ggplot2")
p = plot(imp_test)
p

ggsave("../figure_man/pfi_extrapolation.pdf", width=3, height=1.5)


p2 = ggplot(data, aes(x=x1, y=x2)) + geom_hex() #+ theme_bw()
p2

ggsave("../figure_man/pfi_hexbin_pre.pdf", width=4, height=3)


data_perm = data.frame(data)
data_perm$x1 = data_perm$x1[sample(nrow(data_perm))]

p3 = ggplot(data_perm, aes(x=x1, y=x2)) + geom_hex() #+ theme_bw()
p3
ggsave("../figure_man/pfi_hexbin_post.pdf", width=4, height=3)

# interactions

n = 10000
ntrain = 0.7*n

x1 = sample(c(-1, 1), n, replace=TRUE)
x2 = sample(c(-1, 1), n, replace=TRUE)
x3 = sample(c(-1, 1), n, replace=TRUE)
x4 = sample(c(-1, 1), n, replace=TRUE)

y = x1 * x2 + x3 + rnorm(n)

data = data.frame(x1=x1, x2=x2, x3=x3, x4=x4, y=y)

task = TaskRegr$new(id='correlated', backend=data, target='y')
train_set = sample(task$nrow, ntrain)
test_set = setdiff(seq_len(task$nrow), train_set)

learner = lm(y ~ x1*x2 + x3, data=data[train_set,])

predictor_test = Predictor$new(learner, data[test_set,], y='y')

imp_test <- FeatureImp$new(predictor_test,loss = "mae", n.repetitions = 10, compare='difference')
p = plot(imp_test)
p

ggsave("../figure_man/pfi_interactions.pdf", height=1.5, width=3)
