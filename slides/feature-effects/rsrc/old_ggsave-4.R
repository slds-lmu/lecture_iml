library(ggplot2)
library(knitr)
library(mlr)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)
library(ggpubr)

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

dens2 = density(df_observed$x2)
p2 + geom_path(data = data.frame(x = dens2$x, y = dens2$y),
  aes(x = -5*y - 3, y = x)) +
  geom_line(data = NULL, aes(x = c(-3,-3), y = range(dens2$x)))

p = grid.arrange(p1, p2, ncol = 2, respect = TRUE)
ggsave("../figure_man/pd_grid.pdf", p, width = 8, height = 4)


######################################################

set.seed(1)
library(mvtnorm)
#x1 = runif(1000, -5, 5)
#x2 = x1 + rnorm(1000, 0, 1)
sig = matrix(c(1,0.9,0.9,1), byrow = TRUE, ncol = 2)
df_observed = as.data.frame(rmvnorm(500, sigma = sig))
df_observed$y = - 1*df_observed$V1 + 2*df_observed$V2 + rnorm(500)

cor(df_observed$V1, df_observed$V2)

mod = lm(y ~ V1 + V2, data = df_observed)

mplot = function(data, feature, target, eps) {
  x = data[, feature]
  y = data[, target]
  x.lower = x - eps
  x.upper = x + eps
  m = n = numeric(length(x))
  for(i in 1:length(x)) {
   ind = x > x.lower[i] & x < x.upper[i]
   m[i] = mean(y[ind])
   n[i] = sum(ind)
  }
  return(data.frame(x = x, mplot = m, n = n))
}

pred = Predictor$new(mod, data = df_observed, y = df_observed$y)
pdp = FeatureEffect$new(pred, feature = "V1", method = "pdp")
mpl = mplot(df_observed, "V1", target = "y", eps = 0.5)
p1 = ggplot(data = df_observed, aes(V1, V2)) +
  geom_point() +
  stat_cor(method = "pearson", aes(label = gsub("R", "Cor(x[1],x[2])", ..r.label..))) +
  #stat_cor(aes(label = ..r.label..)) +
  labs(x = expression(x[1]), y = expression(x[2])) +
  theme_minimal()
p2 = ggplot() +
  stat_function(data = NULL, fun = function(x) -1*x, mapping = aes(col = "function f(x) = -x")) +
  geom_line(data = pdp$results, aes(V1, .value, col = "PD plot")) +
  geom_line(data = mpl, aes(x, mplot, col = "M-plot")) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(colour = "Method", x = expression(x[1]), y = expression("Marginal Effect "~hat(f)[1](x[1]))) +
  #scale_colour_manual(values = c("black", "red", "blue"),
  #  labels=c("PD plot", "M-plot", "f(x) = -0.1x")) +
  theme_minimal() + theme(legend.position="bottom")
p = grid.arrange(p1, p2, ncol = 2, respect = TRUE)
ggsave("../figure_man/pd_vs_mplot.pdf", p, width = 8, height = 4)
# pdp = FeatureEffect$new(pred, feature = "x2", method = "pdp")
# mpl = mplot(df_observed, "x2", eps = 0.1)
# p2 = pdp$plot() +
#   geom_line(data = mpl, aes(x, mplot), col = 2, lwd = 2) +
#   stat_function(data = NULL, fun = function(x) x, col = 3, lty = 2, lwd = 2) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept = 0) +
#   theme_minimal()
#
#




###################################################

library(ggplot2)
library(gridExtra)
theme_set(theme_bw())
set.seed(10)
# Generate Pseudo Random Variables
x1 = runif(100, -5, 5)
x2 = x1 + rnorm(100, 0, 1)
df_observed = data.frame(x1, x2)
df_permuted = data.frame(expand.grid(x1, x2))
names(df_permuted) = c("x1", "x2")
# Generate Densities
marg_dens = density(x2)
marg_discrete = data.frame(x = marg_dens$x, y = marg_dens$y)
eps = 0.1
df_cond = subset(df_observed, x1 < (0 + eps) & x1 > (0 - eps))
cond_dens = density(df_cond$x2)
cond_discrete = data.frame(x = cond_dens$x, y = cond_dens$y)
# Plot Data
p1 = ggplot() +
  geom_point(data = df_observed, aes(x1, x2), size = 1) +
  labs(x = expression(X[1]), y = expression(X[2]))
# Plot Marginal
parse1 ="paste(\"PD-plot at \",X[1], \" averages f(\", X[1], \", \", X[2], \")\")"
parse2 = "paste(\"over the marginal distribution of \", X[2])"

marg_plot = p1 +
  geom_line(data = marg_discrete, aes(x = -5*y + min(df_observed$x1) -eps, y = x),
    alpha = 0.5) +
  geom_segment(data = df_observed, aes(x = -2.75, y = -8, xend = -4.75, yend = -5),
    arrow = arrow(length = unit(0.15, "cm")), size = 1) +
  geom_text(aes(x = -2.5, y = -7.5, label = parse1), hjust = 0, parse = TRUE) +
  geom_text(aes(x = -2.5, y = -9, label = parse2), hjust = 0, parse = TRUE) +
  labs(subtitle = expression(paste("Marginal distribution of ", X[2])))
# Plot Conditional
parse3 ="paste(\"M plot averages f(\", X[1], \", \", X[2], \")\")"
parse4 = "paste(\"over the condtional distribution of \", X[2], \"|\", X[1], \"=\", X[1])"

cond_plot = p1 +
  geom_line(data = cond_discrete, aes(x = -5*y + 0 -eps, y = x), alpha = 0.5) +
  geom_segment(data = df_observed, aes(x = -1, y = -7, xend = 0, yend = -3),
    arrow = arrow(length = unit(0.15, "cm")), size = 1) +
  geom_text(aes(x = -2.5, y = -7.5, label = parse3), hjust = 0, parse = TRUE) +
  geom_text(aes(x = -2.5, y = -9, label = parse4), hjust = 0, parse = TRUE) +
  labs(subtitle = expression(paste("Conditional distribution of ", X[2], "|", X[1], "=0")))

# Plot both
gridExtra::grid.arrange(marg_plot, cond_plot, nrow = 1)




# new
# Plot Data
p1 = ggplot() +
  geom_point(data = df_observed, aes(x1, x2), size = 1) +
  labs(x = expression(X[1]), y = expression(X[2]))
# Plot Marginal
parse1 ="paste(\"PD-plot at \",X[1], \" averages f(\", X[1], \", \", X[2], \")\")"
parse2 = "paste(\"over the marginal distribution of \", X[2])"

marg_plot = p1 +
  geom_line(data = marg_discrete, aes(x = -5*y + min(df_observed$x1) -eps, y = x),
    alpha = 0.5) +
  geom_segment(data = df_observed, aes(x = -2.75, y = -8, xend = -4.75, yend = -5),
    arrow = arrow(length = unit(0.15, "cm")), size = 1) +
  geom_text(aes(x = -2.5, y = -7.5, label = parse1), hjust = 0, parse = TRUE) +
  geom_text(aes(x = -2.5, y = -9, label = parse2), hjust = 0, parse = TRUE) +
  labs(subtitle = expression(paste("Marginal distribution of ", X[2])))
# Plot Conditional
parse3 ="paste(\"M plot averages f(\", X[1], \", \", X[2], \")\")"
parse4 = "paste(\"over the condtional distribution of \", X[2], \"|\", X[1], \"=\", X[1])"

cond_plot = p1 +
  geom_path(data = cond_discrete, aes(x = -5*y + min(df_observed$x1) -eps, y = x + mean(with(df_observed, x2[x2<quantile(x2, 0.05)]))), alpha = 0.5) +
geom_segment(data = df_observed, aes(x = -1, y = -7, xend = 0, yend = -3),
  arrow = arrow(length = unit(0.15, "cm")), size = 1) +
  geom_text(aes(x = -2.5, y = -7.5, label = parse3), hjust = 0, parse = TRUE) +
  geom_text(aes(x = -2.5, y = -9, label = parse4), hjust = 0, parse = TRUE) +
  labs(subtitle = expression(paste("Conditional distribution of ", X[2], "|", X[1], "=-5")))


p1 +
  geom_path(data = cond_discrete, aes(x = -3*y + min(df_observed$x1) -eps, y = x + mean(with(df_observed, x2[x2<quantile(x2, 0.05)]))), alpha = 0.5) +
  annotate(
    geom = "line",
    x = min(df_observed$x1),
    y = mean(with(df_observed, x2[x2<quantile(x2, 0.05)])),
    xend = min(df_observed$x1) + 3,
    yend = mean(with(df_observed, x2[x2<quantile(x2, 0.05)])),
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text",
    x = -2,
    y = -4,
    label = "subaru", hjust = "left")

gridExtra::grid.arrange(marg_plot, cond_plot, nrow = 1)

























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
















