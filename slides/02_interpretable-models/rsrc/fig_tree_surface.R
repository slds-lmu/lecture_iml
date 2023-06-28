library("mlr3verse")
library("mlr3learners")
library(mlr3viz)
library(mlr)
library(plotly)
set.seed(1)


load("../../../data/bike.RData")
task = as_task_regr(bike, target = "cnt")$select(c("temp", "hum"))
# task = tsk("bike_sharing")$select(c("temperature", "humidity"))

learner = lrn("regr.rpart")

# 2D plot
p = plot_learner_prediction(learner, task)
p 
ggsave("../figure/tree_surface1.pdf", p, width = 6, height = 3.5)

# 3D surface plot
dat = data.frame(p$data)[,c(2,3,5)]
estimate = matrix(dat$response, ncol = 100, byrow = FALSE)
temp = unique(dat$temperature)
hum = unique(dat$humidity)
# Plot
p2 <- plot_ly(z=~estimate, x= ~temp, y = ~hum,
              type = "surface") 
p2
