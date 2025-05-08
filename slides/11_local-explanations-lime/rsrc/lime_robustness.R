library(mlr3)
library(mlr3viz)
library(iml)
library(mlr3learners)
library(ggplot2)
theme_set(theme_bw())

#### linear task 
set.seed(1234)
lin = TaskGenerator2DNormals$new()
lin$plot(n = 700)

# train logistic regression model 
lm = lrn("classif.log_reg", predict_type = "prob")
ltsk = lin$generate(n = 700)
lm$train(ltsk)

lmod = Predictor$new(lm, data = ltsk$data(), y = "y")

### Surrogate model 
get_beta = function(mod, x.interest, data) {
  lmod1 = Predictor$new(mod, data = data, y = "y")
  lim1 = iml::LocalModel$new(lmod1, x.interest = x.interest, k = 2, dist.fun = "euclidean", kernel.width = 3)
  return(round(lim1$results[1:2, "beta"], 2))
}
l1 <- get_beta(lm, x.interest = data.frame(x1 = 0.05, x2 = 0.5), data = lin$generate(n = 30L)$data())
l2 <- get_beta(lm, x.interest = data.frame(x1 = -0.1, x2 = 0.2), data = lin$generate(n = 30L)$data())
l3 <- get_beta(lm, x.interest = data.frame(x1 = 0.35, x2 = -0.1), data = lin$generate(n = 30L)$data())


# create plot 
pll = plot_learner_prediction(lm, ltsk)

pll$layers[[2]] <- NULL # remove plotted dots

pll = pll + geom_point(aes(x = 0.05, y = 0.5)) + 
  geom_point(aes(x = -0.1, y = 0.2)) +
  geom_point(aes(x = 0.35, y = -0.1)) +
  geom_text(aes(x = 0.05, y = 0.8), label = paste0("(", l1[1], ", ", l1[2], ")")) + 
  geom_text(aes(x = 1.1, y = 0.2), label = paste0("(", l2[1], ", ", l2[2], ")")) + 
  geom_text(aes(x = 0.6, y = -0.4), label = paste0("(", l3[1], ", ", l3[2], ")")) +
  theme(legend.position = "none")

# ggsave(pll, filename = "figure/lime_robustness_1.png", width = 4, height = 4)


#### rf for circular data 
set.seed(1234)
circ = TaskGeneratorCircle$new()
circ$plot(n = 700)
tsk = circ$generate(n = 700)

# train random forest
rf = lrn("classif.ranger", predict_type = "prob", num.trees = 10L)
rf$train(tsk)

### Set up LIME 
mod1 = Predictor$new(rf, data = circ$generate(n = 30)$data(), y = "y")
x1 = data.frame(x1 = 0.2, x2 = -0.7)
mod$predict(x1)
lim1 = iml::LocalModel$new(mod1, x.interest = x1, k = 2, dist.fun = "euclidean", kernel.width = 3)
b1 = round(lim1$results[1:2, "beta"], 2)


mod2 = Predictor$new(rf, data = circ$generate(n = 30)$data(), y = "y")
x2 = data.frame(x1 = 0.3, x2 = -0.75)
mod$predict(x2)
lim2 = iml::LocalModel$new(mod2, x.interest = x2, k = 2, dist.fun = "euclidean", kernel.width = 3)
b2 = round(lim2$results[1:2, "beta"], 2)


mod3 = Predictor$new(rf, data = circ$generate(n = 30)$data(), y = "y")
x3 = data.frame(x1 = 0.35, x2 = -0.7)
mod$predict(x2)
lim3 = iml::LocalModel$new(mod3, x.interest = x2, k = 2, dist.fun = "euclidean", kernel.width = 3)
b3  = round(lim3$results[1:2, "beta"], 2)


# create plots
plt = plot_learner_prediction(rf, tsk)

plt$layers[[2]] <- NULL # remove dots

plt2 = plt + geom_point(aes(x = 0.2, y = -0.7)) + 
  geom_point(aes(x = 0.3, y = -0.75)) + 
  geom_point(aes(x = 0.35, y = -0.7)) + 
  geom_text(aes(x = -0.05, y = -0.6), label = paste0("(", b1[1], ", ", b1[2], ")")) + 
  geom_text(aes(x = 0.3, y = -0.85), label = paste0("(", b2[1], ", ", b2[2], ")")) + 
  geom_text(aes(x = 0.5, y = -0.6), label = paste0("(", b3[1], ", ", b3[2], ")")) +
  theme(legend.position = "none")

# ggsave(plt2, filename = "figure/lime_robustness_2.png", width = 4, height = 4)
