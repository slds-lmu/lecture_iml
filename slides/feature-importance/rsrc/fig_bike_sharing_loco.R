# PREREQ -----------------------------------------------------------------------
library("mlr3")
library("mlr3verse")
library("mlr3learners")
library("ggplot2")
library("data.table")

theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

#setwd('~/university/phd/2021/teaching/lecture_iml/slides/feature-importance/rsrc')

# DATA -----------------------------------------------------------------------
target = "cnt"
nruns = 30

set.seed(123)
load("../../../data/bike.RData")
bike = na.omit(bike)


lrn_type= 'regr.ranger'

task = TaskRegr$new(id = 'full', backend = bike, target = target)


ntrain = task$nrow * 0.7

train_set = sample(task$nrow, ntrain)
test_set = setdiff(seq_len(task$nrow), train_set)

df = data.frame(perf_pre=double(), perf_post=double(), score=double(), feature=character())

for (i in c(1:nruns)) {
  
  learner = lrn(lrn_type)
  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)
  prediction$score()
  
  perf_pre = prediction$score()
  
  cols = colnames(bike)
  features = cols[1:10]
  
  for (f in features){
    # compute new performance
    rmd = cols[cols != f]
    bike_tmp = data.frame(bike)
    bike_tmp = bike_tmp[,rmd]
    
    task_partial = TaskRegr$new(id = 'full', backend = bike_tmp, target = target)
    learner_partial = lrn(lrn_type)
    
    learner_partial$train(task_partial, row_ids = train_set)
    prediction = learner_partial$predict(task, row_ids = test_set)
    perf_post = prediction$score()
    
    row = c(unname(as.list(c(perf_pre, perf_post, perf_post - perf_pre))), f)
    names(row) = colnames(df)
    df = rbind(row, df)
  }
}

dt = data.table(df)
res = dt[, list(mean(score), quantile(score, 0.05), quantile(score, 0.95)), by=feature]
colnames(res) = c('feature', 'importance', 'q.05', 'q.95')

res$feature <- factor(res$feature,
                      levels = res$feature[order(res$importance)])

# PLOT -----------------------------------------------------------------------
ggplot(res, aes(y = feature, x = importance)) +
  geom_segment(aes(y = feature, yend = feature, x = q.05, xend = q.95), size = 1.5, color = "darkslategrey") +
  geom_point(size = 3)
#scale_x_continuous(sprintf("Feature Importance (loss: %s)", private$loss_string)) +
#scale_y_discrete("")

ggsave('../figure_man/bike_sharing_loco.pdf', width=8, height=4)
