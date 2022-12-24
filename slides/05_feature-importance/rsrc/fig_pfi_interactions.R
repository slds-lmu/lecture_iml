# PREREQ -----------------------------------------------------------------------
library("iml")
library("mlr3")
library("mlr3verse")
library("ggplot2")
library("iml")
library("dplyr")
library(patchwork)
theme_set(theme_bw())

# DATA -----------------------------------------------------------------------

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

# PLOT -----------------------------------------------------------------------
p = plot(imp_test)
p

ggsave("../figure_man/pfi_interactions.pdf", width=5, height=3)


