library(knitr)
library(mlr)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)
library(ggplot2)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))
source("helpers.R")

set.seed(123)
load("bike.RData")
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
predictor = Predictor$new(mod, data = bike[-which(names(bike) == "cnt")], y = bike$cnt)
bike.x = bike[names(bike) != 'cnt']

pred.bike = Predictor$new(mod, data = bike)

#############################################################
# ICE
#############################################################

set.seed(123)
pred.sub = Predictor$new(mod, data = bike[721:731,], y = bike$cnt[721:731])
pdp = FeatureEffect$new(pred.sub, "temp", method = "ice")
p1 = pdp$plot() + scale_x_continuous('Temperature') + scale_y_continuous('Predicted bike rentals', limits = c(0, 4000))
# pdp$set.feature("hum")
# p2 = pdp$plot() +  scale_x_continuous('Humidity') + scale_y_continuous('', limits = c(0, 2500))
# pdp$set.feature("windspeed")
# p3 = pdp$plot() + scale_x_continuous('Windspeed') + scale_y_continuous('', limits = c(0, 2500))
p1
#gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

ggsave("../figure_man/ice_bike10obs.pdf", p1, width = 4, height = 2.5)

############################################################

i = pch = rep(1:3, 3) #c(2,3,1,3,1,2)
x = c(1,1,1,2,2,2,3,3,3)
y = c(
  1,1,0,
  1,1,0,
  1,1,0)
fh = c(
  0.7, 0.7, 0.1,
  0.4, 0.7, 0.1,
  0.7, 0.6, 0.3)
l = abs(y - fh)
li = l[c(1,5,9)]
li = li[i]
dL = c(0.4,0.6,0.1,0.6,0.8,0.5,0.7,0.9,0.6)
lreplace = dL + mean(l)
X = data.frame(pch, x, dL)
X.aggr = aggregate(dL ~ x, data = X, FUN = mean, na.rm = TRUE)

pdf(file = "../figure_man/ICE.pdf", width = 5, height = 4)
par(mar = c(3,3.5,0.25,0.25))
pch.sym = paste0("i=", c("1","2","3"))
p = pch.sym[pch]
#p[p == x] = NA

split = split(X, X$pch)
split = lapply(split, function(x) x[order(x$x), ])

plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
  split = split, i = 1, col  = "red")
# plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
#   split = list("1" = split[[1]]))

######################################################

par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
  split = split, i = 1)

d = split[[2]]
col = "red"
lines(d$x, d$dL, lty = d$pch + 1, lwd = 2, col = col)
points(d$x, d$dL, pch = 19, col = col)
shadowtext(d$x, d$dL - 0.025,
  labels = paste0("i=", d$pch), pos = 3, col = col)
#########################################################

par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
  split = split, i = 2)

d = split[[3]]
col = "red"
lines(d$x, d$dL, lty = d$pch + 1, lwd = 2, col = col)
points(d$x, d$dL, pch = 19, col = col)
shadowtext(d$x, d$dL - 0.025,
  labels = paste0("i=", d$pch), pos = 3, col = col)
dev.off()
############################################################












############################################################

set.seed(123)
pred.bike = Predictor$new(mod, data = bike)
pdp.2feature = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "pdp")
pdp.2feature$plot() + scale_x_continuous('Temperature', limits = c(0, NA)) +  scale_y_continuous('Humidity', limits = c(0, NA))

