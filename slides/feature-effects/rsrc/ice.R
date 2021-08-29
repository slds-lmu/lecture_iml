library(knitr)
library(mlr)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)
library(ggplot2)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))
source("slides/feature-effects/rsrc/helpers.R")

load("data/bike.RData")

library(mgcv)
library(mgcViz)
library(effects)

#pdf(file = "slides/feature-effects/figure_man/lm_main_effects.pdf", width = 8, height = 3)
#lm.mod = lm(cnt ~ temp + season, data = bike)
#plot(allEffects(lm.mod))
#dev.off()

p1 = ggplot(data = bike, aes(x = temp, y = cnt)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") +
  labs(x = "Temperature in °C", y = "Marginal Effect on \n 'number of bike rentals'") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5))

# ggplot(data = bike, aes(x = temp, y = cnt, fill = season)) +
#   geom_point(aes(col = season)) +
#   geom_smooth(aes(col = season), method = "lm")

p2 = ggplot(data = bike, aes(x = temp, y = cnt)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "gam") +
  labs(x = "Temperature in °C", y = "Marginal Effect on \n 'number of bike rentals'") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5))

p = gridExtra::grid.arrange(p1 + ggtitle("LM"), p2 + ggtitle("GAM"), ncol = 2)

ggsave("slides/feature-effects/figure_man/lm_main_effects.pdf", p, width = 8, height = 3)




p1 = ggplot(data = bike, aes(x = temp, y = cnt, fill = season)) +
  geom_point(aes(col = season), alpha = 0.5) +
  geom_smooth(aes(col = season), method = "lm", se = FALSE) + #, fullrange = TRUE) +
  labs(x = "Temperature in °C", y = "Number of bike rentals")

p2 = ggplot(data = bike, aes(x = temp, y = cnt, fill = season)) +
  geom_point(aes(col = season), alpha = 0.5) +
  geom_smooth(aes(col = season), method = "gam", se = FALSE) + #, fullrange = TRUE) +
  labs(x = "Temperature in °C", y = "Number of bike rentals")

p = gridExtra::grid.arrange(p1 + ggtitle("LM"), p2 + ggtitle("GAM"), ncol = 2)


lm.mod = lm(cnt ~ temp*season, data = bike) #season + yr + holiday + temp + hum + windspeed + season:temp, data = bike)
pdf(file = "slides/feature-effects/figure_man/lm_interaction.pdf", width = 8, height = 3)
plot(allEffects(lm.mod), layout = c(4, 1))
dev.off()
#gam.mod = gam(cnt ~ season + yr + holiday + s(temp) + s(hum) + s(windspeed), data = bike)
#print(plot(getViz(lm.mod), allTerms = TRUE), pages = 1)
#print(plot(getViz(gam.mod), allTerms = TRUE), pages = 1)

set.seed(123)
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

ggsave("slides/feature-effects/figure_man/ice_bike10obs.pdf", p1, width = 4, height = 2.5)

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

pdf(file = "slides/feature-effects/figure_man/ICE.pdf", width = 5, height = 4)
pch.sym = paste0("i=", c("1","2","3"))
p = pch.sym[pch]
#p[p == x] = NA

split = split(X, X$pch)
split = lapply(split, function(x) x[order(x$x), ])


par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x[1], dL[1], ylab = expression(hat(f)[S]), main = "",
  split = lapply(split[1], function(x) x[1,]), i = 1, col  = "red")

par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x[1], dL[1], ylab = expression(hat(f)[S]), main = "",
  split = lapply(split[1], function(x) x[1:2,]), i = 1, col  = "red")

par(mar = c(3,3.5,0.25,0.25))
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
#
#
#
set.seed(12345678)
n = 100
ngrid = 11
x1 = rexp(n, rate = 1)
x2 = x1 + rnorm(n, mean(x1), sd = 0.5*sd(x1)) #rexp(1000, rate = 100)
#x2 = sqrt(x1)*abs(x2)
plot(density(x1))
plot(x1, x2)

par(mfrow = c(1, 3), mar = c(4,4,2,0))
# grid
grid.x1 = seq(min(x1), max(x1), length = ngrid+1)
grid.x2 = seq(min(x2), max(x2), length = ngrid+1)

const.x2 = rep(sort(x2, decreasing = T)[2], length = ngrid+1)
data1.const = expand.grid(x1 = grid.x1, x2 = const.x2)
data1.const$method = "equidistant grid"


data1 = expand.grid(x1 = grid.x1, x2 = grid.x2)
data1$method = "equidistant grid"
plot(data1$x1, data1$x2, pch = 4, main = "equidistant grid", xlim = range(x1), ylim = range(x2), col = "#000000A0")
points(x1, x2, col = "#FF000040", pch = 19)

# subsample
grid.x1 = sample(x1, size = ngrid+1)
grid.x2 = sample(x2, size = ngrid+1)

data2.const = expand.grid(x1 = grid.x1, x2 = const.x2)
data2.const$method = "randomly sampled grid"

data2 = expand.grid(x1 = grid.x1, x2 = grid.x2)
data2$method = "randomly sampled grid"
plot(data2$x1, data2$x2, pch = 4, main = "randomly sampled grid", xlim = range(x1), ylim = range(x2), col = "#000000A0")
points(x1, x2, col = "#FF000040", pch = 19)

# quantile
grid.x1 = quantile(x1, 0:(ngrid)/(ngrid), type = 1)
grid.x2 = quantile(x2, 0:(ngrid)/(ngrid), type = 1)

data3.const = expand.grid(x1 = grid.x1, x2 = const.x2)
data3.const$method = "quantile grid"

data3 = expand.grid(x1 = grid.x1, x2 = grid.x2)
data3$method = "quantile grid"
plot(data3$x1, data3$x2, pch = 4, main = "quantile grid", xlim = range(x1), ylim = range(x2), col = "#000000A0")
points(x1, x2, col = "#FF000040", pch = 19)

data = rbind(data1, data2, data3)
data$method = factor(data$method, levels = c("equidistant grid", "randomly sampled grid", "quantile grid"))

data.const = rbind(data1.const, data2.const, data3.const)
data.const$method = factor(data.const$method, levels = c("equidistant grid", "randomly sampled grid", "quantile grid"))

ind = which(x2 == sort(x2, decreasing = TRUE)[2])

p = ggplot(data = data, aes(x1, x2)) +
  geom_point(data = data.frame(x1 = x1[ind], x2 = x2[ind]), aes(x1, x2), size = 3, col = "blue") +
  geom_point(data = data.frame(x1 = x1, x2 = x2), aes(x1, x2), alpha = 0.25) +
  geom_rug(data = data.frame(x1 = x1, x2 = x2), aes(x = x1), alpha = 0.25, sides = "b") +
  geom_point(data = data.const, aes(x1, x2), shape = 4, alpha = 0.5, col = "red") +
  facet_grid(~ method) +
  #geom_point(data = data[data$x2 == min(data[data$method=="randomly sampled grid", "x2"]),], shape = 4, alpha = 0.5, col = "red") +
  #geom_rug(aes(x = x1), col = "blue", sides = "b") +
  xlab("Feature"~X[S]) +
  ylab("Feature"~X[C]) +
  ggtitle("Grid points for"~X[S]~"(red) for highlighted observation (blue)")


ann_text = data.frame(x1 = max(x1), x2 = x2[ind], diff = max(diff(sort(x1))), #lab = "Text",
  method = factor("equidistant grid", levels = levels(data$method)))
p = p + geom_text(data = ann_text, aes(x = x1-diff/2, y = x2), nudge_y = -1, label = paste0("unrealistic?"), parse = F, col = "red") + # atop('unrealistic', 'values')
  geom_rect(data = ann_text, mapping = aes(ymax = x2 + 0.5, ymin = x2 - 0.5,
    xmax = x1 - 0.3, xmin = (x1-diff) + 0.3), alpha = 0, size = 0.5,
    colour = "red", fill = "red") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("slides/feature-effects/figure_man/sampling.pdf", p, height = 2.5, width = 7.5)
# pdf(file = "./figures/sampling.pdf", height = 3, width = 9)
# print(p)
# dev.off()










############################################################

set.seed(123)
pred.bike = Predictor$new(mod, data = bike)
pdp.2feature = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "pdp")
pdp.2feature$plot() + scale_x_continuous('Temperature', limits = c(0, NA)) +  scale_y_continuous('Humidity', limits = c(0, NA))

