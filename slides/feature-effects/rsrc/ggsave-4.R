
library(knitr)
library(mlr)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)


set_parent("../style/preamble.Rnw")

set.seed(123)
load("bike.RData")
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
set.seed(123)
pred.bike = Predictor$new(mod, data = bike)
bike.x = bike[names(bike) != 'cnt']

######################################################

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

###################################################

inv = seq(floor(min(x1)), ceiling(max(x1)), length.out = 5)
id = which(x1 >= inv[1] & x1 < inv[2])
p3 = p1 + geom_vline(xintercept = inv, colour = "black") +
  geom_segment(aes(x = inv[1], xend = inv[2], y = x2[id], yend = x2[id]), colour = "blue") +
  geom_point(aes(x = inv[1], y = x2[id]), colour = "blue") +
  geom_point(aes(x = inv[2], y = x2[id]), colour = "blue")
grid.arrange(p1, p3, ncol = 2, respect = TRUE)

####################################################

require("plot3D")
fn_sig = function(x) 1 / (1 + exp(-x))
fn_sigd = function(x) (1 + exp(-x))^(-2) * exp(-x)
fn_sig2d = function(x) ((1 + exp(-x))^(-2) * exp(-x) * (-1)) + (exp(-x)^2 * (2) * (1 + exp(-x))^(-3))
fn_sig3d = function(x) (6 * (1 + exp(-x))^(-4) * exp(-x) * exp(-x)^2 - 2 * (1 + exp(-x))^(-3) * 2 * exp(-x)^2) + (-2 * (1 + exp(-x))^(-3) * exp(-x)^2 + (1 + exp(-x))^(-2) * exp(-x))


set.seed(500)
x1 = seq(-10, 10, 0.1)
x1 = x1 + rnorm(length(x1), 0, 1)
x2 = seq(-10, 10, 0.1)
x2 = x2 + rnorm(length(x2), 0, 1)
errors = rnorm(length(x1), 0, 3)
y = 100 * fn_sig2d(x1) + 2 * x2 + errors
df = data.frame(x1, x2, y)
pred_fun = function(X.model, newdata) as.numeric(predict(X.model, newdata))
tsk = makeRegrTask(target = "y", data = df)
mod = train("regr.ksvm", tsk)

predicted = predict(mod, newdata = df)
prediction = data.frame(x1, x2, y = predicted$data$response)

interval.boundaries = quantile(x1, prob = seq(0, 1, 0.1), type = 1)
# length(interval.boundaries)
interval.boundaries.left = interval.boundaries[-length(interval.boundaries)]
interval.boundaries.right = interval.boundaries[-1]

# plot(x, y)
# plot(x, fn_sig3d(x))
x1lim = range(x1)
x2lim = range(x2)
ylim = range(y)
grid.lines = 25
x1.pred <- seq(min(x1), max(x1), length.out = grid.lines)
x2.pred <- seq(min(x2), max(x2), length.out = grid.lines)
x1x2 <- expand.grid(x1 = x1.pred, x2 = x2.pred)
y.pred <- matrix(predict(mod, newdata = x1x2)$data$response,
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface

par(mar = c(1, 1, 1, 2))

colfunc <- colorRampPalette(c("red", "yellow", "springgreen", "royalblue"))
p = scatter3D(x1, x2, y, ticktype = "detailed",
              bg = "black", pch = 21, lwd = 2, cex = 2.5, col = colfunc(20),
              surf = list(x = x1.pred, y = x2.pred, z = y.pred,
                          facets = NA, fit = prediction$y, col = "steelblue"),
              xlim = x1lim, ylim = x2lim, zlim = ylim,
              xlab = "x1", ylab = "x2", zlab = "y",
              theta = 40, phi = 00,
              bty = "b2",
              xaxs = "i", yaxs = "i")

addHyperplane = function(x.value) {
  
  x2.min = x2lim[1]
  x2.max = x2lim[2]
  horizontal.line = data.frame(x1 = c(x.value, x.value), x2 = c(x2.min, x2.max), y = c(max(y), max(y)))
  lines(trans3D(x = horizontal.line$x1, y = horizontal.line$x2, z = horizontal.line$y, p),
        col = '#FF0000', lwd = 2.5)
  horizontal.line = data.frame(x1 = c(x.value, x.value), x2 = c(x2.min, x2.max), y = c(min(y), min(y)))
  lines(trans3D(x = horizontal.line$x1, y = horizontal.line$x2, z = horizontal.line$y, p),
        col = '#FF0000', lwd = 2.5)
  vertical.line = data.frame(x1 = c(x.value, x.value), x2 = c(x2.min, x2.min), y = c(max(y), min(y)))
  lines(trans3D(x = vertical.line$x1, y = vertical.line$x2, z = vertical.line$y, p),
        col = '#FF0000', lwd = 2.5)
  vertical.line = data.frame(x1 = c(x.value, x.value), x2 = c(x2.max, x2.max), y = c(max(y), min(y)))
  lines(trans3D(x = vertical.line$x1, y = vertical.line$x2, z = vertical.line$y, p),
        col = '#FF0000', lwd = 2.5)
  # for (i in seq(-10, 10, 0.15)) {
  #   line.3d = trans3D(x = rep(x.value, length(x)), y = y, z = rep(i, length(z)), p)
  #   lines(line.3d, col = '#FF000005')
  # }
}


for (i in interval.boundaries) {
  addHyperplane(i)
  # addCutlines(i)
}

####################################################

df.filtered = df[df$x1 <= interval.boundaries[9] & df$x1 > interval.boundaries[8], ]
df.filtered.right = df.filtered
df.filtered.right$x1 = interval.boundaries[9]
df.filtered.left = df.filtered
df.filtered.left$x1 = interval.boundaries[8]
df.filtered.permuted = rbind(df.filtered.right, df.filtered.left)

predictions.right = predict(mod, newdata = df.filtered.right)$data$response
df.filtered.right$pred = predictions.right
predictions.left = predict(mod, newdata = df.filtered.left)$data$response
df.filtered.left$pred = predictions.left

addHyperplaneSubset = function(x.value) {
  y = df.filtered$y
  x2 = df.filtered$x2
  horizontal.line = data.frame(x1 = c(x.value, x.value), x2 = c(min(x2), max(x2)), y = c(max(y), max(y)))
  lines(trans3D(x = horizontal.line$x1, y = horizontal.line$x2, z = horizontal.line$y, p),
        col = '#FF0000', lwd = 3.5)
  horizontal.line = data.frame(x1 = c(x.value, x.value), x2 = c(min(x2), max(x2)), y = c(min(y), min(y)))
  lines(trans3D(x = horizontal.line$x1, y = horizontal.line$x2, z = horizontal.line$y, p),
        col = '#FF0000', lwd = 3.5)
  vertical.line = data.frame(x1 = c(x.value, x.value), x2 = c(min(x2), min(x2)), y = c(max(y), min(y)))
  lines(trans3D(vertical.line$x1, vertical.line$x2, vertical.line$y, p),
        col = '#FF0000', lwd = 3.5)
  vertical.line = data.frame(x1 = c(x.value, x.value), x2 = c(max(x2), max(x2)), y = c(max(y), min(y)))
  lines(trans3D(vertical.line$x1, vertical.line$x2, vertical.line$y, p),
        col = '#FF0000', lwd = 3.5)
  # for (i in seq(-10, 10, 0.15)) {
  #   line.3d = trans3D(x = rep(x.value, length(x)), y = y, z = rep(i, length(z)), p)
  #   lines(line.3d, col = '#FF000005')
  # }
}

################################################

colfunc <- colorRampPalette(c("springgreen", "royalblue"))
# par("mar" = c(4, 1, 1, 1))
p = scatter3D(df.filtered$x1,
              df.filtered$x2,
              df.filtered$y,
              ticktype = "detailed",
              bg = "black", pch = 21, lwd = 3, cex = 4, col =  colfunc(10),
              # surf = list(x = x.pred, y = y.pred, z = z.pred,
              # facets = NA, fit = fitpoints, col = "steelblue"),
              theta = 40, phi = 00,
              xlab = "x1", ylab = "x2", zlab = "y",
              # xlim = c(interval.boundaries[8], interval.boundaries[9]),
              # zlim = c(min(df.filtered$y), max(df.filtered$y)),
              bty = "b2",
              xaxs = "i", yaxs = "i")
addHyperplaneSubset(interval.boundaries[8] + 0.05)
points(trans3D(df.filtered$x1, df.filtered$x2, df.filtered$y, p),
       bg = "black", pch = 21, lwd = 3, cex = 4, col =  colfunc(10))
addHyperplaneSubset(interval.boundaries[9])

#######################################################

colfunc <- colorRampPalette(c("springgreen", "royalblue"))
# par("mar" = c(4, 1, 1, 1))
p = scatter3D(x = df.filtered.permuted$x1,
              y = df.filtered.permuted$x2,
              z = df.filtered.permuted$y,
              ticktype = "detailed",
              bg = "black", pch = 21, lwd = 3, cex = 4, col =  colfunc(10),
              # surf = list(x = x.pred, y = y.pred, z = z.pred,
              # facets = NA, fit = fitpoints, col = "steelblue"),
              theta = 00, phi = 00,
              xlab = "x1", ylab = "x2", zlab = "y",
              # xlim = c(interval.boundaries[8] - 0.5, interval.boundaries[9] + 0.5),
              # zlim = c(min(df.filtered$y) - 1, max(df.filtered$y) + 1),
              bty = "b2",
              xaxs = "i", yaxs = "i")

addHyperplaneSubset(interval.boundaries[8])
addHyperplaneSubset(interval.boundaries[9])
# points(trans3D(df.filtered.left$x, df.filtered.left$y, df.filtered.left$z, p), pch = 21, bg = "black", col = "green")
# points(trans3D(df.filtered.right$x, df.filtered.right$y, df.filtered.right$z, p), pch = 21, bg = "black", col = "green")

points(trans3D(df.filtered.permuted$x1,
               df.filtered.permuted$x2,
               df.filtered.permuted$y, p),
       bg = "black", pch = 21, lwd = 3, cex = 4, col =  colfunc(10))
########################################################

par("mar" = c(4, 1, 1, 1))
p = scatter3D(x = df.filtered.permuted$x1,
              y = df.filtered.permuted$x2,
              z = df.filtered.permuted$y,
              ticktype = "detailed", col = rgb(0, 0, 0, max = 255, alpha = 0),
              # surf = list(x = x.pred, y = y.pred, z = z.pred,
              # facets = NA, fit = fitpoints, col = "steelblue"),
              theta = 40, phi = 00,
              xlab = "x1", ylab = "x2", zlab = "y",
              # xlim = c(interval.boundaries[8] - 0.5, interval.boundaries[9] + 0.5),
              # zlim = c(min(df.filtered$y) - 1, max(df.filtered$y) + 1),
              bty = "b2",
              xaxs = "i", yaxs = "i")

addHyperplaneSubset(interval.boundaries[8])

for (i in 1:nrow(df.filtered)) {
  y.right = predictions.right[i]
  y.left = predictions.left[i]
  x1.left = interval.boundaries[8]
  x1.right = interval.boundaries[9]
  x2.fixed = df.filtered$x2[i]
  point.left = data.frame("x1" = x1.left, "x2" = x2.fixed, "y" = y.left)
  point.right = data.frame("x1" = x1.right, "x2" = x2.fixed, "y" = y.right)
  line.coords = rbind(point.left, point.right)
  line.coords.trans = trans3D(
    x = line.coords$x1,
    y = line.coords$x2,
    z = line.coords$y,
    p)
  lines(line.coords.trans, col = "black", lwd = 8)
  lines(line.coords.trans, col = "#E69F00", lwd = 4)
}
addHyperplaneSubset(interval.boundaries[9])

####################################################

par("mar" = c(4, 1, 1, 1))
p = scatter3D(x = df.filtered.permuted$x1,
              y = df.filtered.permuted$x2,
              z = df.filtered.permuted$y,
              ticktype = "detailed", col = rgb(0, 0, 0, max = 255, alpha = 0),
              # surf = list(x = x.pred, y = y.pred, z = z.pred,
              # facets = NA, fit = fitpoints, col = "steelblue"),
              theta = 00, phi = 00,
              xlab = "x1", ylab = "x2", zlab = "y",
              # xlim = c(interval.boundaries[8] - 0.5, interval.boundaries[9] + 0.5),
              # zlim = c(min(df.filtered$y) - 1, max(df.filtered$y) + 1),
              bty = "b2",
              xaxs = "i", yaxs = "i")
addHyperplaneSubset(interval.boundaries[8])
addHyperplaneSubset(interval.boundaries[9])
for (i in 1:nrow(df.filtered)) {
  y.right = mean(predictions.right)
  y.left = mean(predictions.left)
  x1.left = interval.boundaries[8]
  x1.right = interval.boundaries[9]
  x2.mean = mean(df.filtered$x2)
  point.left = data.frame("x1" = x1.left, "x2" = x2.mean, "y" = y.left)
  point.right = data.frame("x1" = x1.right, "x2" = x2.mean, "y" = y.right)
  line.coords = rbind(point.left, point.right)
  line.coords.trans = trans3D(
    x = line.coords$x1,
    y = line.coords$x2,
    z = line.coords$y,
    p)
  lines(line.coords.trans, col = "black", lwd = 8)
  lines(line.coords.trans, col = "#E69F00", lwd = 4)
}

#####################################################
########################
############

iml_pred = Predictor$new(mod, df)
ale = FeatureEffect$new(iml_pred, "x1", method = "ale", grid.size = 10)
p = ale$plot() +
  geom_line(size = 3, color = "black") +
  geom_line(size = 1.5, color = "#E69F00") +
  geom_vline(xintercept = interval.boundaries) +
  geom_vline(xintercept = c(interval.boundaries[8], interval.boundaries[9]),
             color = "red", size = 1.5) +
  scale_y_continuous('First order ALE of x1')
plot(p)

######################################################

set.seed(123)
pdp = FeatureEffect$new(pred.bike, "hum", method = "pdp", grid.size = 50)
pdp_plot <- pdp$plot() + scale_y_continuous('Univariate PD on humidity')
ale = FeatureEffect$new(pred.bike, "hum", method = "ale", grid.size = 50)
ale_plot <- ale$plot() + scale_y_continuous('First order ALE of humidity')
grid.arrange(pdp_plot, ale_plot, nrow = 1, ncol = 2)

###################################################

set.seed(123)
pdp = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "pdp", grid.size = 10)
pdp_plot <- pdp$plot() + theme(legend.position = "top", legend.key.width = unit(1.5,"cm")) + ggtitle("Bivariate PD")
ale = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "ale", grid.size = 10)
ale_plot <- ale$plot() + theme(legend.position = "top", legend.key.width = unit(1.5,"cm")) + ggtitle("Second order ALE")
grid.arrange(pdp_plot, ale_plot, nrow = 1, ncol = 2)

#
# set.seed(123)
# pred.bike = Predictor$new(mod, data = bike)
# pdp.2feature = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "pdp")
# pdp.2feature$plot() + scale_x_continuous('Temperature', limits = c(0, NA)) +  scale_y_continuous('Humidity', limits = c(0, NA))
















