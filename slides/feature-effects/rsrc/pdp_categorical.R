library(iml)
library(mlr)

set.seed(123)
load("bike.RData")
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
pred.bike = Predictor$new(mod, data = bike)
bike.x = bike[names(bike) != 'cnt']

# Compute the partial dependence for the first feature
eff = FeatureEffects$new(pred.bike,
  features = c("season", "mnth", "weathersit"),
  method = "pdp+ice")
eff$plot(features = "season") +
  stat_summary(fun = mean, geom = "point", aes(group = 1), pch = 4, col = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), col = 2, lty = 2)

p = eff$plot(features = "season")
p$layers[[1]] = NULL
p + geom_path(aes(group = 1), alpha = 0.2) +
  geom_point() +
  #geom_boxplot(alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", aes(group = 1), pch = 4, col = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), col = 2, lty = 2)
#eff$plot(features = c("season"))


# library(pdp)
# library(ggplot2)  # required to use autoplot
#
# mod = randomForest(cnt ~ ., data = bike)
# # Partial dependence of cmedv on lstat
# mod %>%
#   partial(pred.var = "season", train = bike) %>%
#   autoplot(rug = TRUE, train = bike)
#
# # ICE curves and c-ICE curves
# age.ice <- partial(mod, pred.var = "season", ice = TRUE)
#
# autoplot(age.ice, alpha = 0.5)
# autoplot(age.ice, center = TRUE, alpha = 0.5)
