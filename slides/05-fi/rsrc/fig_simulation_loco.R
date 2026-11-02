# PREREQ -----------------------------------------------------------------------
library("mlr3")
library("mlr3verse")
library("mlr3learners")
library("ggplot2")
library("data.table")# simulation

# DATA -----------------------------------------------------------------------
n = 1000
target='y'

x1 = rnorm(n, mean=0, sd = 5)
x2 = rnorm(n, mean=0, sd = 0.1) + x1
x3 = rnorm(n, mean=0, sd = 5)
y = rnorm(n, mean = 0, sd = 2) + x3 + x2

simulation = data.frame(x1=x1, x2 = x2, x3=x3, y=y)

task = TaskRegr$new(id = 'full', backend = simulation, target = target)

lrn_type = 'regr.lm'

ntrain = task$nrow * 0.7

train_set = sample(task$nrow, ntrain)
test_set = setdiff(seq_len(task$nrow), train_set)

df = data.frame(perf_pre=double(), perf_post=double(), score=double(), feature=character())

nruns = 1 # we don't need multiple LM fits on the same data
for (i in c(1:nruns)) {
  learner = lrn(lrn_type)
  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)
  prediction$score()
  
  perf_pre = prediction$score()
  
  cols = colnames(simulation)
  features = cols[cols != target]
  
  for (f in features){
    # compute new performance
    rmd = cols[cols != f]
    sim_tmp = data.frame(simulation)
    sim_tmp = sim_tmp[,rmd]
    
    task_partial = TaskRegr$new(id = 'partial', backend = sim_tmp, target = target)
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

learner$model

ggsave('../figure_man/simulation_loco.pdf', width=3, height=1)
