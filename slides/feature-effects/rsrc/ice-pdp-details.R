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

set.seed(123)
load("bike.RData")
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
bike.x = bike[names(bike) != 'cnt']

pred.sub = Predictor$new(mod, data = bike, y = bike$cnt)

pdp = FeatureEffect$new(pred.sub, "temp", method = "pdp+ice")
p = pdp$plot() + scale_x_continuous('Temperature') + scale_y_continuous('Predicted bike rentals')

pdp = FeatureEffect$new(pred.sub, "temp", method = "pdp+ice", center.at = min(bike$temp))
pmin = pdp$plot() + scale_x_continuous('Temperature') + scale_y_continuous('Predicted bike rentals')

pdp = FeatureEffect$new(pred.sub, "temp", method = "pdp+ice", center.at = max(bike$temp))
pmax = pdp$plot() + scale_x_continuous('Temperature') + scale_y_continuous('Predicted bike rentals')

pres = p + ggtitle("ICE plot") +
  pmin + ggtitle("c-ICE plot at x* = min(temp)") +
  pmax + ggtitle("c-ICE plot at x* = max(temp)")

ggsave("../figure_man/cICE.pdf", pres, width = 10, height = 3.25)

###############################################

set.seed(123)
load("bike.RData")
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
pred.bike = Predictor$new(mod, data = bike[sample(1:nrow(bike), 100),])
bike.x = bike[names(bike) != 'cnt']

# Compute the partial dependence for the first feature
eff = FeatureEffect$new(pred.bike,
  feature = c("season"),#, "mnth", "weathersit"),
  method = "pdp+ice")

library(data.table)
d = as.data.table(eff$results)
d = d[, list(season = season, .value = .value - .value[season=="SPRING"]), by = c(".type", ".id")]

cpdcat = ggplot(d[d$.type=="ice"], aes(x = season, y = .value)) +
  geom_path(aes(group = 1), alpha = 0.2) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", aes(group = 1), pch = 4, col = 2, size = 2, stroke = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), col = 2, lty = 2, lwd = 1) +
  scale_y_continuous('Predicted bike rentals')

ggsave("../figure_man/cICEcat.pdf", cpdcat, width = 5.5, height = 3)
















library(pdp)
library(ggplot2)  # required to use autoplot

mod = randomForest(cnt ~ ., data = bike)
# Partial dependence of cmedv on lstat
mod %>%
  partial(pred.var = "season", train = bike) %>%
  autoplot(rug = TRUE, train = bike)

# ICE curves and c-ICE curves
age.ice <- partial(mod, pred.var = "season", ice = TRUE)

autoplot(age.ice, alpha = 0.5)
autoplot(age.ice, center = TRUE, alpha = 0.5)

age.ice2 = pdp:::center_ice_curves.ice(age.ice)
