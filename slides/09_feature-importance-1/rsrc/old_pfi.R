library("iml")
library("mlr3")
library("mlr3verse")
library("ggplot2")
library("iml")
library("dplyr")
theme_set(theme_bw())

#setwd('~/university/phd/2021/teaching/lecture_iml/slides/feature-importance/rsrc')

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


# TODO add error bars http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
p = ggplot(data=results, aes(x=reorder(colname, -importance), y=importance, fill=reorder(type, -importance)))
p = p + geom_bar(stat='identity', position=position_dodge())
p = p + geom_errorbar(aes(ymin=q05, ymax=q95), width=.2, position=position_dodge(.9))
p = p + labs(x='Feature', fill='IML method', y='Score')
p = p + theme_bw()
p

#ggplot(results, aes(x=type, y=importance)) + geom_boxplot()
ggsave('../figure_man/pfi_test_vs_train.pdf', width=8, height=2)



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

p_pfi = plot(imp_test)
p_pfi

#ggsave("../figure_man/pfi_extrapolation.pdf", width=5, height=3)

p2 = ggplot(data, aes(x=x1, y=x2)) + geom_hex(bins = 20) + scale_fill_viridis_c() #+ theme_bw()
p2

#ggsave("../figure_man/pfi_hexbin_pre.pdf", width=6, height=4.5)

data_perm = data.frame(data)
data_perm$x1 = data_perm$x1[sample(nrow(data_perm))]

p3 = ggplot(data_perm, aes(x=x1, y=x2)) +
  geom_hex(bins = 20) + scale_fill_viridis_c() #+ theme_bw()
p3

#
# # explain conditional permutation scheme
# data_perm = data
# breaks = quantile(data_perm$x2, seq(0,1,by=0.1))
# breaks = seq(min(data_perm$x2), max(data_perm$x2), length = 20)
# data_perm$condition_x2 = findInterval(data_perm$x2, breaks)
# dlist = split(data_perm, data_perm$condition_x2)
# dlist = lapply(dlist, function(x) {
#   x$x1 = x$x1[sample(nrow(x))]
#   return(x)
# })
# data_perm = data.table::rbindlist(dlist)
#
# #data_perm[, x1_sample := sample(x1), by = condition_x2]
# #data_perm$x1 = data_perm$x1[sample(nrow(data_perm))]
# p4 = ggplot(data_perm, aes(x=x1, y=x2)) +
#   geom_hex(bins = 20) + scale_fill_viridis_c() #+ theme_bw()
# p4 + geom_hline(yintercept = breaks)

library(patchwork)

res = p2 + p3 + p_pfi & theme(aspect.ratio=1)#, legend.position = "bottom") #+ plot_layout(guides = 'collect')

ggsave(plot = res, "../figure_man/pfi_hexbin_extrapolation.pdf", width=10, height=3)

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

ggsave("../figure_man/pfi_interactions.pdf", width=5, height=3)


# pimp

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

p = plot(imp_test)


df = data.frame(imp_test$results[,c('feature', 'importance')])
df$type = 'H1'

for (ii in 1:1000) {
  y_perm = sample(y, length(y))
  data = data.frame(x1=x1, x2=x2, x3=x3, x4=x4, y=y_perm)

  task = TaskRegr$new(id='correlated', backend=data, target='y')
  learner = lrn('regr.lm')

  train_set = sample(task$nrow, ntrain)
  test_set = setdiff(seq_len(task$nrow), train_set)

  learner$train(task, row_ids = train_set)
  learner$model

  predictor_test = Predictor$new(learner, data[test_set,], y='y')

  imp_test_perm <- FeatureImp$new(predictor_test,loss = "mae", n.repetitions = 10, compare='difference')

  df_tmp = data.frame(imp_test_perm$results[,c('feature', 'importance')])
  df_tmp$type = 'H0'

  df = rbind(df_tmp, df)
}

saveRDS(df, file = "pimp.Rds")
df = readRDS("pimp.Rds")

library(data.table)
df_dt = as.data.table(df)
pvalues = df_dt[,mean(importance[type == "H0"]>importance[type == "H1"]), by = feature]

df_h1 = df[df$type == 'H1', c('feature', 'importance')]
df_h0 = df[df$type == 'H0', c('feature', 'importance')]
df_h1 = merge(pvalues, df_h1)
df_h0 = merge(pvalues, df_h0)

paste0(feature, ", p-value: ", round(V1, 3))


p = ggplot(df_h0, aes(x=importance)) + geom_histogram()#aes(y = ..density..)) + geom_density(col = "red")
p = p +
  geom_vline(data=df_h1, aes(xintercept=importance), colour="red", linetype="dashed")
  #ggtext::geom_richtext(data=df_h1, aes(x = 0, y = 0, label = paste("p-value:", round(V1, 3))), angle = 0, hjust = 0, vjust = 0)
#p = p + facet_grid(feature ~ .)
p = p + facet_wrap(. ~ paste0(feature, ", p-val: ", round(V1, 3)), scales = "free", nrow = 1)
p

ggsave("../figure_man/pimp.pdf", width=10, height=2.5)


#p2 = ggplot(data, aes(x=x1, y=x2)) + geom_de() #+ theme_bw()
#p2

#ggsave("../figure_man/pfi_hexbin_pre.pdf", width=4, height=3)


# data_perm = data.frame(data)
# data_perm$x1 = data_perm$x1[sample(nrow(data_perm))]
#
# p3 = ggplot(data_perm, aes(x=x1, y=x2)) + geom_hex() #+ theme_bw()
# p3
# ggsave("../figure_man/pfi_hexbin_post.pdf", width=4, height=3)
#
