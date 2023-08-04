x = runif(1000, min = -10, max = 10)
y = runif(1000, min = -10, max = 10)
# errors = rnorm(1000, mean = 0, sd = 1)
# z =  x^2 - 15 * x * y + errors
z = x^3 * y
df = data.frame(x, y, z)

png("../figure/interaction.png")
# plot(y, z)
library(mlr)
library(plot3D)

tsk = makeRegrTask(data = df, target = "z")
mod = train("regr.ksvm", tsk)

predicted = predict(mod, newdata = df)
prediction = data.frame(x, y, z = predicted)
grid.lines = 100
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
# fit = mod$learner.model
preds = predict(mod, newdata = xy)$data$response
z.pred <- matrix(preds,
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
# fitpoints <- predict(fit)

# model.preds = predict(mod, newdata = df)$data$response
# plot(x, model.preds)
p = persp3D(x.pred, y.pred, z.pred, ticktype = "detailed",
            theta = 40, phi = 30, lighting = TRUE, lphi = 40,
            xlab = "x1", ylab = "x2", zlab = "y",
            bty = "b2")

dev.off()
# fdrawLines = function(pmat, x.start, x.end, y.start, y.end) {
#   
#   line.df = data.frame(x = seq(x.start, x.end), y = y.start, z = predict(mod, newdata = data.frame(x = seq(x.start, x.end), y = y.start))$data$response)
#   lines(trans3D(x = line.df$x, y = line.df$y, z = line.df$z, pmat = pmat), lwd = 3)
#   
# }
# 
# drawLines(p, x.start = -8, x.end = -2, y.start = -8, y.end = -8)
# points(trans3D(x = -8, y = -8, z = predict(mod, newdata = data.frame(x = -8, y = -8))$data$response, p),
#        pch = 21, bg = "gold", col = "black", lwd = 3, cex = 2)
# points(trans3D(x = -2, y = -8, z = predict(mod, newdata = data.frame(x = -2, y = -8))$data$response, p),
#        pch = 21, bg = "gold", col = "black", lwd = 3, cex = 2)
# 
# drawLines(p, x.start = 2, x.end = 8, y.start = -8, y.end = -8)
# points(trans3D(x = 2, y = -8, z = predict(mod, newdata = data.frame(x = 2, y = -8))$data$response, p),
#        pch = 21, bg = "gold", col = "black", lwd = 3, cex = 2)
# points(trans3D(x = 8, y = -8, z = predict(mod, newdata = data.frame(x = 8, y = -8))$data$response, p),
#        pch = 21, bg = "gold", col = "black", lwd = 3, cex = 2)
# 
# drawLines(p, x.start = -8, x.end = -2, y.start = -8, y.end = -8)
# points(trans3D(x = -8, y = -8, z = predict(mod, newdata = data.frame(x = -8, y = -8))$data$response, p),
#        pch = 21, bg = "gold", col = "black", lwd = 3, cex = 2)
# points(trans3D(x = -2, y = -8, z = predict(mod, newdata = data.frame(x = -2, y = -8))$data$response, p),
#        pch = 21, bg = "gold", col = "black", lwd = 3, cex = 2)
# 
# drawLines(p, x.start = 2, x.end = 8, y.start = -8, y.end = -8)
# points(trans3D(x = 2, y = -8, z = predict(mod, newdata = data.frame(x = 2, y = -8))$data$response, p),
#        pch = 21, bg = "gold", col = "black", lwd = 3, cex = 2)
# points(trans3D(x = 8, y = -8, z = predict(mod, newdata = data.frame(x = 8, y = -8))$data$response, p),
#        pch = 21, bg = "gold", col = "black", lwd = 3, cex = 2)
# 
# 
# 
# 
# # drawLines(p, x.start = 2, x.end = 8, y.start = -8, y.end = -8)
# # points(trans3D(x = -8, y = -8, z = predict(mod$learner.model, data.frame(x = 5, y = 0)), p),
# #        pch = 21, bg = "gold", col = "black", lwd = 3, cex = 2)
# # points(trans3D(x = -2, y = -8, z = predict(mod$learner.model, data.frame(x = 10, y = -5)), p),
# #        pch = 21, bg = "gold", col = "black", lwd = 3, cex = 2)
# #
# #
# # drawLines(p, x.start = -8, x.end = -2, y.start = -5, y.end = -5)
# # drawLines(p, x.start = 2, x.end = 8, y.start = -5, y.end = -5)
# #
# # drawLines(p, x.start = -8, x.end = -2, y.start = 0, y.end = 0)
# # drawLines(p, x.start = 2, x.end = 8, y.start = 0, y.end = 0)
# #
# # drawLines(p, x.start = -8, x.end = -2, y.start = 5, y.end = 5)
# # drawLines(p, x.start = 2, x.end = 8, y.start = 5, y.end = 5)
# 
# # drawLines(p, x.start = -8, x.end = -2, y.start = 8, y.end = 8)
# # drawLines(p, x.start = 2, x.end = 8, y.start = 8, y.end = 8)
# 
# drawLines(p, x.start = -8, x.end = -2, y.start = 8, y.end = 8)
# points(trans3D(x = -8, y = 8, z = predict(mod, newdata = data.frame(x = -8, y = 8))$data$response, p),
#        pch = 21, bg = "gold", col = "black", lwd = 3, cex = 2)
# points(trans3D(x = -2, y = 8, z = predict(mod, newdata = data.frame(x = -2, y = 8))$data$response, p),
#        pch = 21, bg = "gold", col = "black", lwd = 3, cex = 2)
# 
# 
# drawLines(p, x.start = 2, x.end = 8, y.start = 8, y.end = 8)
# points(trans3D(x = 2, y = 8, z = predict(mod, newdata = data.frame(x = 2, y = 8))$data$response, p),
#        pch = 21, bg = "gold", col = "black", lwd = 3, cex = 2)
# points(trans3D(x = 8, y = 8, z = predict(mod, newdata = data.frame(x = 8, y = 8))$data$response, p),
#        pch = 21, bg = "gold", col = "black", lwd = 3, cex = 2)
# 
# dev.off()
# 
