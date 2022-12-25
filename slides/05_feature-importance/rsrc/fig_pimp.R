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

#paste0(feature, ", p-value: ", round(V1, 3))

# DATA -----------------------------------------------------------------------
p = ggplot(df_h0, aes(x=importance)) + geom_histogram()#aes(y = ..density..)) + geom_density(col = "red")
p = p +
  geom_vline(data=df_h1, aes(xintercept=importance), colour="red", linetype="dashed")
#ggtext::geom_richtext(data=df_h1, aes(x = 0, y = 0, label = paste("p-value:", round(V1, 3))), angle = 0, hjust = 0, vjust = 0)
#p = p + facet_grid(feature ~ .)
p = p + facet_wrap(. ~ paste0(feature, ", p-val: ", round(V1, 3)), scales = "free", nrow = 1)
p

ggsave("../figure_man/pimp.pdf", width=10, height=2.5)
