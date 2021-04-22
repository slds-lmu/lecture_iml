library(ggplot2)
library(knitr)
library(mlr)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)
library(ggpubr)
library(patchwork)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))
source("helpers.R")

pdf(file = "../figure_man/PD.pdf", width = 5, height = 4)
par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
  split = split, i = 3, col = "gray")

splitx = split(X, X$x)
i = 1
d = splitx[[i]]
col = "red"
points(d$x, d$dL, pch = 19, col = col)
shadowtext(d$x, d$dL - 0.025,
  labels = paste0("i=", d$pch), pos = 3, col = col)
lines(X.aggr$x[i], X.aggr$dL[i], type = "b", lwd = 3, col = col)

#
par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
  split = split, i = 3, col = "gray")

d = splitx[[2]]
points(d$x, d$dL, pch = 19, col = col)
shadowtext(d$x, d$dL - 0.025,
  labels = paste0("i=", d$pch), pos = 3, col = col)
lines(X.aggr$x[1:2], X.aggr$dL[1:2], type = "b", lwd = 3, col = col)

#
par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
  split = split, i = 3, col = "gray")

d = splitx[[3]]
points(d$x, d$dL, pch = 19, col = col)
shadowtext(d$x, d$dL - 0.025,
  labels = paste0("i=", d$pch), pos = 3, col = col)
lines(X.aggr$x, X.aggr$dL, type = "b", lwd = 3, col = col)
dev.off()

######################################################

pred.sub = Predictor$new(mod, data = bike, y = bike$cnt)
pdp = FeatureEffect$new(pred.sub, "temp", method = "ice")
p1 = pdp$plot() + scale_x_continuous('Temperature') + scale_y_continuous('Predicted bike rentals', limits = c(0, 4000))

pdp = Partial$new(pred.bike, "temp", ice = TRUE, aggregation = "pdp")
p1 = pdp$plot() + scale_x_continuous('Temperature', limits = c(0, NA)) + scale_y_continuous('Predicted number of bike rentals')
p1 + xlim(range(bike$temp))

######################################################

set.seed(123)
load("bike.RData")
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
pred.bike = Predictor$new(mod, data = bike[sample(1:nrow(bike), 100),])
bike.x = bike[names(bike) != 'cnt']


pred.sub = Predictor$new(mod, data = bike, y = bike$cnt)
pdp = FeatureEffect$new(pred.sub, "temp", method = "pdp+ice")
p1 = pdp$plot() + scale_x_continuous('Temperature') + scale_y_continuous('Predicted bike rentals')
p1
ggsave("../figure_man/pdp_num.pdf", p1, width = 6, height = 3)



# Compute the partial dependence for the first feature
eff = FeatureEffect$new(pred.bike,
  feature = c("season"),#, "mnth", "weathersit"),
  method = "pdp+ice")
pd_cat = eff$plot() + #(features = "season") +
  stat_summary(fun = mean, geom = "point", aes(group = 1), pch = 4, col = 2, size = 2, stroke = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), col = 2, lty = 2, lwd = 1) +
  scale_y_continuous("Predicted number of bike rentals") + ggtitle("PD plot for a categorical feature")

p = eff$plot() #(features = "season")
p$layers[[1]] = NULL
ice_cat = p + geom_path(aes(group = 1), alpha = 0.2) +
  geom_point() +
  #geom_boxplot(alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", aes(group = 1), pch = 4, col = 2, size = 2, stroke = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), col = 2, lwd = 1, lty = 2) +
  scale_y_continuous("Predicted number of bike rentals") + ggtitle("ICE plot for a categorical feature")

pd_cat + ice_cat

ggsave("../figure_man/pdp_ice_cat.pdf", pd_cat + ice_cat, width = 8, height = 3)
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






set.seed(123)
pred.bike = Predictor$new(mod, data = bike)
pdp.2feature = FeatureEffect$new(pred.bike, feature = c("temp", "season"), method = "pdp")
pdp.2feature$plot() + scale_x_continuous('Temperature', limits = c(0, NA)) +  scale_y_continuous('Humidity', limits = c(0, NA))
