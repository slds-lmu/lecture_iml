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
source("slides/feature-effects/rsrc/helpers.R")

set.seed(123)
load("data/bike.RData")
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
bike.x = bike[names(bike) != 'cnt']

pred.sub = Predictor$new(mod, data = bike, y = bike$cnt)

pdp = FeatureEffect$new(pred.sub, "hum", method = "pdp+ice")
p = pdp$plot() + scale_x_continuous('Humidity in % (hum)') + scale_y_continuous('Predicted bike rentals')

pdp = FeatureEffect$new(pred.sub, "hum", method = "pdp+ice", center.at = min(bike$hum))
pmin = pdp$plot() + scale_x_continuous('Humidity in % (hum)') + scale_y_continuous('Predicted bike rentals')

# pdp = FeatureEffect$new(pred.sub, "hum", method = "pdp+ice", center.at = max(bike$hum))
# pmax = pdp$plot() + scale_x_continuous('Temperature') + scale_y_continuous('Predicted bike rentals')

pres = p + ggtitle("ICE plot") +
  pmin + ggtitle("c-ICE plot at x* = min(hum)") #+
  #pmax + ggtitle("c-ICE plot at x* = max(temp)")

#ggsave("../figure_man/cICE.pdf", pres, width = 10, height = 3.25)
ggsave("../figure_man/cICE.pdf", pres, width = 8, height = 3)

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
  stat_summary(fun = mean, geom = "point", aes(group = 1), pch = 4, col = "gold", size = 2, stroke = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), col = "gold", lty = 2, lwd = 1.5) +
  scale_y_continuous('Predicted bike rentals')

ggsave("../figure_man/cICEcat.pdf", cpdcat, width = 5.5, height = 3)

######################################################

set.seed(10)
n = 700
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
x3 = runif(n, -1, 1)
eps = rnorm(n, 0, 1)
y = 0.2*x2 - 8*x1 + ifelse(x3 >= 0, 16*x1, 0) + eps
dat = data.frame(x1, x2, x3, y)

tsk = makeRegrTask(data = dat, target = "y")
lrn = makeLearner("regr.gbm", interaction.depth = 6)

# # stupid mini grid
# ps = makeParamSet(
#   makeIntegerParam("interaction.depth", lower = 3, upper = 10),
#   makeNumericParam("shrinkage", lower = 0.05, upper = 0.2)
# )
# ctrl = makeTuneControlRandom()
# inner = makeResampleDesc("CV", iters = 3)
# lrn = makeTuneWrapper(lrn, resampling = inner, par.set = ps, control = ctrl)
# b = benchmark(list(lrn, makeLearner("regr.gbm", interaction.depth = 3)), tsk, cv3)

mod = train(lrn, tsk)
pred = Predictor$new(mod, data = dat[-which(names(dat) == "y")], y = dat$y)

pdp = FeatureEffect$new(pred, "x1", method = "pdp+ice")
p1 = pdp$plot()
ggxor = p1 + scale_y_continuous(name = expression(hat(f)[S])) +
  scale_x_continuous(name = expression(x[1]))

ggsave("../figure_man/pdp_xor.pdf", ggxor, width = 6, height = 4)
# bhd.ice = ice(object = mod$learner.model, X = dat, y = dat$y,
#   predictor = "x3", verbose = FALSE)
# plot(bhd.ice)
# bhd.dice = dice(bhd.ice)
# plot(bhd.dice)
######################################################



# bhd.ice = ice(object = mod$learner.model, X = bike.x, y = bike$cnt,
#   predictor = "temp", frac_to_build = 0.2)
# plot(bhd.ice)
# bhd.dice = dice(bhd.ice)
# plot(bhd.dice)











#
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
#
# age.ice2 = pdp:::center_ice_curves.ice(age.ice)
