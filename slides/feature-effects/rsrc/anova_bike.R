library(mlr)
library(iml)

set.seed(123)
load("bike.RData")
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
pred.bike = Predictor$new(mod, data = bike)
bike.x = bike[names(bike) != 'cnt']



mean.pred = round(mean(pred.bike$predict(bike)$.prediction), 3)
pdp1 = FeatureEffect$new(pred.bike, feature = "temp", method = "pdp", center.at = mean.pred)
p1 = pdp1$plot() + xlab('Temperature') + ylab('Predicted number of bike rentals')
p1 = p1 + xlim(range(bike$temp))



pdp2 = FeatureEffect$new(pred.bike, feature = "hum", method = "pdp", center.at = mean.pred)
p2 = pdp2$plot() + xlab('Humidity') + ylab('Predicted number of bike rentals')
p2 = p2 + xlim(range(bike$hum))



pdp12 = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "pdp")
pdp12$plot() + scale_x_continuous('Temperature', limits = c(0, NA)) +  scale_y_continuous('Humidity', limits = c(0, NA))
pdp1uncentered = FeatureEffect$new(pred.bike, feature = "temp", method = "pdp")
pdp2uncentered = FeatureEffect$new(pred.bike, feature = "hum", method = "pdp")

yhatnew = merge(pdp12$results, pdp2uncentered$results[, 1:2], by = "hum")
yhatnew = merge(yhatnew, pdp1uncentered$results[, 1:2], by = "temp")
yhatnew$.y.hat.x = yhatnew$.y.hat.x - yhatnew$.y.hat.y - yhatnew$.y.hat
yhatnew = yhatnew[1:4]
names(yhatnew) = c("temp", "hum", ".y.hat", ".type")
pdp12$results = yhatnew
p12 = pdp12$plot()

# ggplot2::ggsave("figure_man/anova_bike1.jpg", plot = p1, width = 6.5, height = 5)
# ggplot2::ggsave("figure_man/anova_bike2.jpg", plot = p2, width = 6.5, height = 5)
# ggplot2::ggsave("figure_man/anova_bike3.jpg", plot = p12, width = 6.5, height = 5)
