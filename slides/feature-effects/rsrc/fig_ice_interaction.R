library(plot3D)
library(cowplot)
#pdf("slides/feature-effects/figure/ice_motivation.pdf", width = 11, height = 4)

set.seed(100)
x = rnorm(100, 0, 5)#runif(100, -11, 11)
y = rnorm(100, 0, 5)#runif(100, -11, 11)

errors = rnorm(100, 0, 1)
z = (x+1)^2 + (y+1)^2 + errors
df = data.frame(x, y, z)

library(mlr)
tsk = makeRegrTask(data = df, target = "z")
mod = train("regr.lm", tsk)
mod$learner.model = lm(z~poly(x, degree = 2, raw = TRUE) + poly(y, degree = 2, raw = TRUE) + x*y, data = df)

predicted = predict(mod, newdata = df)$data$response
prediction = data.frame(x, y, z = predicted)
grid.lines = 100
x.pred = seq(min(x), max(x), length.out = grid.lines)
y.pred = seq(min(y), max(y), length.out = grid.lines)
xy = expand.grid( x = x.pred, y = y.pred)
z.pred = matrix(predict(mod, newdata = xy)$data$response,
  nrow = grid.lines, ncol = grid.lines)
fitpoints <- predicted#predict(fit)

library(plot3D)

par(mar = c(2,2,1,1))
#layout(matrix(c(1,1,1,1,2,2,2,2,2), nrow = 1, byrow = TRUE))
p = persp3D(x.pred, y.pred, z.pred, ticktype = "detailed",
  theta = 30, phi = 30, lighting = TRUE, lphi = 180,
  xlab = "x1", ylab = "x2", zlab = "prediction",
  col = terrain.colors(100),
  bty = "b2", xlim = c(-12, 12))#, cex.lab = 2, cex.axis = 2)

# par(xpd = TRUE)
# text3D(-10, -10, 1, expression(x[1]), add = T)

points(trans3D(x = x, y = y,
  z = predicted, pmat = p),
  col = "black", pch = 19)

ind = which(x >= 5 & y <= -5 & y > -9)[1]
x1_1 = x[ind]
x1_2 = x[ind]

x2_1 = min(y)
x2_2 = max(y)

ldf = data.frame(x = seq(x1_1, x1_2), y = seq(x2_1, x2_2))
line.df = data.frame(
  x = seq(x1_1, x1_2),
  y = seq(x2_1, x2_2),
  z = predict(mod, newdata = ldf)$data$response)


lines(trans3D(x = line.df$x, y = line.df$y, z = line.df$z, pmat = p), lwd = 3, col = "gold")

points(trans3D(x = line.df$x, y = line.df$y, z = line.df$z, pmat = p), col = "red", pch = 4, cex = 1.5)

points(trans3D(x = x[ind], y = y[ind],
  z = predicted[ind], pmat = p),
  col = "gold",# pch = 19,
  cex = 2, pch = 21, bg = "red")
p1 = recordPlot()
#
# par(mar = c(4,5,2,8))
# plot(ldf$y, predict(mod, newdata = ldf)$data$response, col = "gold", type = "l", lwd = 2, panel.first = { grid(lty = 1) },
#   xlab = expression(x[2]), ylab = "prediction")
# points(ldf$y, predict(mod, newdata = ldf)$data$response, pch = 4, col = "red")
# rug(y)
# points(x = y[ind], y = predicted[ind], col = "gold", cex = 2, pch = 21, bg = "red")
# par(xpd = TRUE)
# legend("topleft",
#   legend = c("observation \nof interest", "grid points", "changes in \nprediction", "training data"),
#   inset = c(1.01,0.3),
#   lty = c(NA, NA, 1, NA),
#   pch = c(21, 4, NA, 19),
#   pt.cex = c(2, 1, NA, 1),
#   pt.bg = c("red", NA, NA, NA),
#   bg = "white",
#   y.intersp = 2,
#   lwd = c(NA, NA, 2, NA),
#   col = c("gold", "red", "gold", "black"))

#dev.off()


library(iml)

m = Predictor$new(mod, data = df)
eff = FeatureEffect$new(m, feature = "y", grid.size = 21, method = "ice", center.at = min(df$y))
eff$plot()

plot_grid(p1, eff$plot())

# set.seed(100)
# x = rnorm(100, 0, 5)#runif(100, -11, 11)
# y = rnorm(100, 0, 5)#runif(100, -11, 11)
#
# errors = rnorm(100, 0, 10)
# z = (x + y^3 + 0.01*x*y) + errors
# df = data.frame(x, y, z)

# library(mlr)
# tsk = makeRegrTask(data = df, target = "z")
# mod = train("regr.ksvm", tsk)
#
# m = Predictor$new(mod, data = df)
# eff = FeatureEffect$new(m, feature = "y", grid.size = 21, method = "ice")
# eff$plot()
