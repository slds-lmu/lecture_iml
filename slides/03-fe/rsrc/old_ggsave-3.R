library(knitr)
library(mlr)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)
library(ggplot2)

set.seed(123)
load("data/bike.RData")
task = makeRegrTask(data = bike, target = "cnt")
mod = mlr::train("regr.randomForest", task)
predictor = Predictor$new(mod, data = bike[-which(names(bike) == "cnt")], y = bike$cnt)
bike.x = bike[names(bike) != 'cnt']
pred.bike = Predictor$new(mod, data = bike)

# Helpers
shadowtext = function(x, y = NULL, labels, col = 'black', bg = 'white',
                      theta = seq(0, 2*pi, length.out = 50), r = 0.1, ... ) {

  xy = xy.coords(x,y)
  xo = r*strwidth('A')
  yo = r*strheight('A')

  # draw background text with small shift in x and y in background colour
  for (i in theta) {
    text(xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col = bg, ... )
  }
  # draw actual text in exact xy position in foreground colour
  text(xy$x, xy$y, labels, col = col, ... )
}

plotImportanceDemo = function(x, dL, split, i = 1, ylab = expression(Delta~L), main = "Individual Conditional Importance (ICI) Curves", col = "black", polygon = FALSE) {
  plot(NULL, xlim = c(1, 3), ylim = c(0, 1), type = "n",
       xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       frame.plot = FALSE, # Remove the frame
       panel.first = {
         grid(lty = 1)
         if (polygon) polygon(c(split[[i]]$x, rev(split[[i]]$x)), c(split[[i]]$dL, rep(0, length(split[[i]]$x))), border = NA, col = rgb(0, 0, 0, 0.25))
       })
  box(bty = "L")
  title(main = main, adj = 0, font.main = 1)
  title(xlab = expression(x[1]), ylab = ylab, line = 2)
  axis(1, at = c(1, 2, 3), padj = -0.5, cex.axis = 0.8, col.axis = "gray40")
  axis(2, at = seq(0, 1, by = 0.2), las = 2, hadj = 0.75,
       labels = prettyNum(seq(0, 1, by = 0.2)), padj = 0.5, cex.axis = 0.8, col.axis = "gray40")

  for (d in split[1:i]) {
    lines(d$x, d$dL, lty = d$pch + 1, lwd = 2, col = col)
    points(d$x, d$dL, pch = 19, col = col)
    shadowtext(d$x, d$dL - 0.025,
               labels = paste0("i=", d$pch), pos = 3, col = col)
  }
}

# Artifical example
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

#############################################################

set.seed(123)
pred.sub = Predictor$new(mod, data = bike[721:731,], y = bike$cnt[721:731])
pdp = Partial$new(pred.sub, "temp", ice = TRUE, aggregation = "none")
p1 = pdp$plot() + scale_x_continuous('Temperature') + scale_y_continuous('Predicted bike rentals', limits = c(0, 4000))
# pdp$set.feature("hum")
# p2 = pdp$plot() +  scale_x_continuous('Humidity') + scale_y_continuous('', limits = c(0, 2500))
# pdp$set.feature("windspeed")
# p3 = pdp$plot() + scale_x_continuous('Windspeed') + scale_y_continuous('', limits = c(0, 2500))
p1
#gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

############################################################

set.seed(123)
pred.bike = Predictor$new(mod, data = bike)
pdp.2feature = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "pdp")
pdp.2feature$plot() + scale_x_continuous('Temperature', limits = c(0, NA)) +  scale_y_continuous('Humidity', limits = c(0, NA))

###########################################################
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
#####################################################

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

######################################################

par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
                   split = split, i = 3, col = "gray")

d = splitx[[2]]
points(d$x, d$dL, pch = 19, col = col)
shadowtext(d$x, d$dL - 0.025,
           labels = paste0("i=", d$pch), pos = 3, col = col)
lines(X.aggr$x[1:2], X.aggr$dL[1:2], type = "b", lwd = 3, col = col)
######################################################

par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
                   split = split, i = 3, col = "gray")

d = splitx[[3]]
points(d$x, d$dL, pch = 19, col = col)
shadowtext(d$x, d$dL - 0.025,
           labels = paste0("i=", d$pch), pos = 3, col = col)
lines(X.aggr$x, X.aggr$dL, type = "b", lwd = 3, col = col)
dev.off()
#######################################################

pdp = Partial$new(pred.bike, "temp", ice = TRUE, aggregation = "pdp")
p1 = pdp$plot() + scale_x_continuous('Temperature', limits = c(0, NA)) + scale_y_continuous('Predicted number of bike rentals')
p1 + xlim(range(bike$temp))

###################################################

set.seed(123)
pred.bike = Predictor$new(mod, data = bike)
pdp.2feature = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "pdp")
pdp.2feature$plot() +
  geom_point(data = bike, mapping = aes(x = temp, y = hum), alpha = 0.5)

####################################################

set.seed(10)
x1 = runif(20, -5, 5)
x2 = x1 + rnorm(20, 0, 1)
df_observed = data.frame(x1, x2)
df_permuted = data.frame(expand.grid(x1, x2))
names(df_permuted) = c("x1", "x2")

dens = density(x2)
d = data.frame(x = dens$x, y = dens$y)

p1 = ggplot() +
  geom_point(data = df_observed, aes(x1, x2),
             shape = 3, color = "red", size = 2, stroke = 2) +
  theme_bw() + ylim(range(d$x))

p2 = ggplot() +
  geom_point(data = df_permuted, aes(x1, x2),
             shape = 3, color = "green", size = 2, stroke = 1) +
  geom_point(data = df_observed, aes(x1, x2),
             shape = 3, color = "red", size = 2, stroke = 2) +
  theme_bw() + ylim(range(d$x))

grid.arrange(p1, p2, ncol = 2, respect = TRUE)
#####################################################

p2 = p2 + geom_line(data = d, aes(x = sort(x1)[19] + 5*y, y = x), alpha = 0.25, color = "red")
grid.arrange(p1, p2, ncol = 2, respect = TRUE)

####################################################

set.seed(10)
n = 700
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
x3 = runif(n, -1, 1)
eps = rnorm(n, 0, 1)
y = 0.2*x1 - 8*x2 + ifelse(x3 >= 0, 16*x2, 0) + eps
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

pdp = Partial$new(pred, "x2", ice = TRUE, aggregation = "pdp")
p1 = pdp$plot()
p1

####################################################








