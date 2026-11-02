source("bike_example_Data.R")
#remotes::install_github("schalkdaniel/compboost", "dev")
library(compboost)
library("mlr3verse")
library("mlr3learners")
options("scipen"=100, "digits"=4)

# fit compboost model with linear and centered splines for numeric features and categorical
# base learner for season
set.seed(31415)
cboost = Compboost$new(data = dat, target = "y", learning_rate = 0.1,
                       loss = LossQuadratic$new())#, oob_fraction = 0.2)

cboost$addComponents("temp", df = 4)
cboost$addComponents("hum", df = 4)
cboost$addComponents("windspeed", df = 4)
cboost$addComponents("days_since_2011", df = 4)
cboost$addBaselearner("season", "ridge", BaselearnerCategoricalRidge, df = 4)

cboost$train(1000L, trace = 1000L)

# create feature importance plot
fi = plotFeatureImportance(cboost) + theme_bw()
ggsave("../figure/compboost_pfi.pdf", fi, width = 6, height = 3.5)

plotRisk(cboost)$data$risk[1001]


## OOB

task = as_task_regr(dat, target = "y")
cv10 = rsmp("cv", folds = 10)
cv10$instantiate(task)
r1000 = numeric()
for(i in 1:10){
  train_set = cv10$train_set(i)
  
  cboost = Compboost$new(data = dat[train_set,], target = "y", learning_rate = 0.1,
                         loss = LossQuadratic$new())#, oob_fraction = 0.2)
  
  cboost$addComponents("temp", df = 4)
  cboost$addComponents("hum", df = 4)
  cboost$addComponents("windspeed", df = 4)
  cboost$addComponents("days_since_2011", df = 4)
  cboost$addBaselearner("season", "ridge", BaselearnerCategoricalRidge, df = 4)
  
  cboost$train(1000L, trace = 1000L)
  pred1000 = cboost$predict(newdata = dat[cv10$test_set(i),])
  r1000[i] = mean((pred1000-dat[cv10$test_set(i),]$y)^2)/2
}
mean(r1000)
