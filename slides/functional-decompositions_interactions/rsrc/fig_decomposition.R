# PREREQ -----------------------------------------------------------------------
library(iml)
library(ggplot2)
library(patchwork)
theme_set(theme_bw())

# DATA -------------------------------------------------------------------------
x1 = seq(from = -10, to = 10, length.out = 21)
x2 = seq(from = -10, to = 10, length.out = 21)
f = function(x1, x2){
  2 + x1^2 - x2^2 + (x1 * x2)
}
dat = expand.grid(x1 = x1, x2 = x2)
dat$y = f(dat$x1, dat$x2)

pred.fun = function(model = NULL, newdata) {
  f(newdata$x1, newdata$x2)
}

pred = Predictor$new(predict.function = pred.fun, data = dat, y = "y")

# digits = 2
# x1val = round(x1[100], digits)
# x2val = round(x2[100], digits)
#
# ale1 = FeatureEffect$new(pred, feature = "x1",
#   method = "ale", grid.size = 101)
# ale2 = FeatureEffect$new(pred, feature = "x2",
#   method = "ale", grid.size = 101)
# ale12 = FeatureEffect$new(pred, feature = c("x1", "x2"),
#   method = "ale", grid.size = 101)
#
# f0 = mean(dat$y)
# f1 = ale1$results$.value[round(ale1$results$x1, digits) == x1val]
# f2 = ale2$results$.value[round(ale2$results$x2, digits) == x2val]
# f12 = ale12$results$.ale[round(ale12$results$x1, digits) == x1val & round(ale12$results$x2, digits) == x2val]
#
# f0 + f1 + f2 + f12
#
# f(x1val, x2val)
#
# p1 = ale1$plot(rug = FALSE) +
#   ggtitle(expression(paste(f[1], " main effect of ", X[1]))) +
#   scale_x_continuous(latex2exp::TeX(r'(Feature $X_1$)')) +
#   geom_vline(xintercept = x1val, lty = 1, col = 2) +
#   geom_hline(yintercept = f1, lty = 1, col = 2)
#
# p2 = ale2$plot(rug = FALSE) +
#   ggtitle(expression(paste(f[2], " main effect of ", X[2]))) +
#   scale_x_continuous(latex2exp::TeX(r'(Feature $X_2$)')) +
#   geom_vline(xintercept = x2val, lty = 1, col = 2) +
#   geom_hline(yintercept = f2, lty = 1, col = 2)
#
# p12 = ale12$plot(rug = FALSE) + geom_contour(aes(z = .ale), color = "black") +
#   #scale_fill_continuous("value", low = "blue",  high = "yellow") +
#   ggtitle(expression(paste(f[12], " interaction between ", X[1], " and ", X[2]))) +
#   scale_x_continuous(latex2exp::TeX(r'(Feature $X_1$)')) +
#   scale_y_continuous(latex2exp::TeX(r'(Feature $X_2$)')) +
#   geom_vline(xintercept = x1val, lty = 1, col = 2) +
#   geom_hline(yintercept = x2val, lty = 1, col = 2)
#
# (p1 + p2) / p12 + patchwork::plot_layout(heights = c(1,2.5))

pdp1 = FeatureEffect$new(pred, feature = "x1",
  method = "pdp", grid.size = 21)
pdp2 = FeatureEffect$new(pred, feature = "x2",
  method = "pdp", grid.size = 21)
pdp12 = FeatureEffect$new(pred, feature = c("x1", "x2"),
  method = "pdp", grid.size = 21)

f0 = mean(dat$y)

pdp1$results$.value = pdp1$results$.value - f0
pdp2$results$.value = pdp2$results$.value - f0
pdp12$results$.value = pdp12$results$.value - f0

fanova = merge(pdp12$results, pdp1$results, by = "x1", suffixes = c("12", "1"))
fanova = merge(fanova, pdp2$results, by = "x2")
fanova$.value = fanova$.value12 - fanova$.value1 - fanova$.value
#fanova = fanova[, c("x1", "x2", ".value", ".type")]

pdp12$results = fanova[order(fanova$x2, fanova$x1), c("x1", "x2", ".value", ".type")]

digits = 1
x1val = round(pdp1$results$x1, digits)[16]
x2val = round(pdp2$results$x2, digits)[21]
f1 = pdp1$results$.value[round(pdp1$results$x1, digits) == x1val]
f2 = pdp2$results$.value[round(pdp2$results$x2, digits) == x2val]
f12 = pdp12$results$.value[round(pdp12$results$x1, digits) == x1val & round(pdp12$results$x2, digits) == x2val]

f0 + f1 + f2 + f12

f(x1val, x2val)

# PLOT -------------------------------------------------------------------------

lab0 = paste("g[0] ==", round(f0, 2))
p0 = ggplot(data = NULL) +
  #geom_hline(yintercept = f0) + ylim(c(-10,10)) +
  geom_segment(aes(x = min(x1), xend = max(x1), y = f0, yend = f0)) + ylim(c(-10,10)) +
  ggtitle(expression(paste(g[0], " constant mean "))) +
  scale_x_continuous(latex2exp::TeX(r'(Feature $x_1$ (or $x_2$))')) +
  scale_y_continuous(latex2exp::TeX(r'($g_{0}$ value)')) +
  #geom_vline(xintercept = x1val, lty = 1, col = 2, lwd = 1) +
  #geom_hline(yintercept = f1, lty = 1, col = 2, lwd = 1) +
  geom_label(aes(x = 0, y = f0, label = lab0),
    col = 2, hjust = 1.1, vjust = -0.1, parse = TRUE)

lab1 = paste("g[1] ==", round(f1, 2))
p1 = pdp1$plot(rug = FALSE) +
  ggtitle(expression(paste(g[1], " main effect of ", X[1]))) +
  scale_x_continuous(latex2exp::TeX(r'(Feature $x_1$)')) +
  scale_y_continuous(latex2exp::TeX(r'($g_{1}$ values)')) +
  geom_vline(xintercept = x1val, lty = 1, col = 2, lwd = 1) +
  geom_hline(yintercept = f1, lty = 1, col = 2, lwd = 1) +
  geom_label(aes(x = x1val, y = f1, label = lab1),
    col = 2, hjust = 1.1, vjust = -0.1, parse = TRUE)

lab2 = paste("g[2] ==", round(f2, 2))
p2 = pdp2$plot(rug = FALSE) +
  ggtitle(expression(paste(g[2], " main effect of ", X[2]))) +
  scale_x_continuous(latex2exp::TeX(r'(Feature $x_2$)')) +
  scale_y_continuous(latex2exp::TeX(r'($g_{2}$ values)')) +
  geom_vline(xintercept = x2val, lty = 1, col = 2, lwd = 1) +
  geom_hline(yintercept = f2, lty = 1, col = 2, lwd = 1) +
  geom_label(aes(x = x2val, y = f2, label = lab2),
    col = 2, hjust = 1.1, vjust = -0.1, parse = TRUE)

lab12 = paste("g[1,2] ==", round(f12, 2))
p12 = pdp12$plot(rug = FALSE) + geom_contour(aes(z = .value), color = "black") +
  #scale_fill_continuous("value", low = "blue",  high = "yellow") +
  scale_fill_gradient2(expression("value"), midpoint = f12, low = 3, mid = "white",
    high = 4, space = "Lab") +
  ggtitle(expression(paste(g["1,2"], " interaction between ", X[1], " and ", X[2]))) +
  scale_x_continuous(latex2exp::TeX(r'(Feature $x_1$)')) +
  scale_y_continuous(latex2exp::TeX(r'(Feature $x_2$)')) +
  geom_vline(xintercept = x1val, lty = 1, col = 2, lwd = 1) +
  geom_hline(yintercept = x2val, lty = 1, col = 2, lwd = 1) +
  geom_label(aes(x = x1val, y = x2val, label = lab12),
    col = 2, hjust = 1.1, vjust = 1.1, parse = TRUE) + theme(legend.position = "none")
#+ theme(legend.position = "bottom")

dat2 = pdp12$results
dat2$y = f(dat2$x1, dat2$x2)
labf = paste("hat(f) ==", f(x1val, x2val))
f = ggplot(data = dat2, aes(x = x1, y = x2, z = y)) +
  geom_tile(aes(fill = y)) + geom_contour(color = "black") +
  scale_fill_gradient2(expression("value"), midpoint = f12, low = 3, mid = "white", high = 4, space = "Lab") +
  geom_vline(xintercept = x1val, lty = 1, col = 2, lwd = 1) +
  geom_hline(yintercept = x2val, lty = 1, col = 2, lwd = 1) +
  geom_label(aes(x = x1val, y = x2val, label = labf),
    col = 2, hjust = 1.1, vjust = 1.1, parse = TRUE) +
  ggtitle(expression(paste(hat(f)(x), " = ", g[0], " + ", g[1](x[1]), " + ", g[2](x[2]), " + ", g["1,2"](x[1], x[2])))) +
  #theme(legend.position = "none") +
  NULL

#(res = (f | (p1 / p2) | p12) +
#    patchwork::plot_layout(heights = c(1, 1, 1.25))) # & theme(legend.position = "bottom") , guides = "collect"
#(p1 + p2) / p12 + patchwork::plot_layout(heights = c(1,2.5))

#(res = (f | p0 | p1 | p2 | p12) +
#    patchwork::plot_layout(heights = c(1, 1, 1, 1, 1.25)))

(res = (f | (p0 + p1) / (p2 | p12)) +
    patchwork::plot_layout(widths = c(1, 1.75), guides = "collect"))

ggsave("../figure/decomposition.pdf",
  height = 5, width = 12, res)

#
# persp3D(
#   z=matrix(dat$y, ncol = length(unique(dat$x1))),
#   theta = -45, , phi = 25, d = 5
# )
#
# persp3D(
#   z=matrix(pdp1$results$.value,
#     ncol = length(unique(pdp1$results$x1)),
#     nrow = length(unique(pdp2$results$x2))),
#   theta = -45, , phi = 25, d = 5
# )
#
# persp3D(
#   z = matrix(pdp2$results$.value,
#     ncol = length(unique(pdp1$results$x1)),
#     nrow = length(unique(pdp2$results$x2))),
#   theta = -45, , phi = 25, d = 5
# )
#
# persp3D(
#   x = unique(pdp1$results$x1),
#   y = unique(pdp2$results$x2),
#   z = matrix(f0,
#     ncol = length(unique(pdp1$results$x1)),
#     nrow = length(unique(pdp2$results$x2)))
#   #,theta = -45, , phi = 25, d = 5
# )
#
# persp3D(
#   x = unique(pdp12$results$x1),
#   y = unique(pdp12$results$x2),
#   z = matrix(pdp12$results$.value,
#     ncol = length(unique(pdp12$results$x1)),
#     nrow = length(unique(pdp12$results$x2)))
#   #, theta = -45, , phi = 25, d = 5
# )
library(mgcv)
mod = lm(.value ~ x1*x2, data = pdp12$results)
  #gam(.value ~ s(x1, x2), data = pdp12$results)

dat = expand.grid(x1 = x1, x2 = x2)
dat$y = predict(mod, newdata = dat)

scatterplot3d::scatterplot3d(pdp12$results$x1, pdp12$results$x2, pdp12$results$.value)
scatterplot3d::scatterplot3d(dat$x1, dat$x2, dat$y)

pred.fun = function(model = NULL, newdata) {
  predict(mod, newdata = newdata)
}

pred = Predictor$new(predict.function = pred.fun, data = dat, y = "y")

FeatureEffect$new(pred, feature = "x1",
  method = "pdp", grid.size = 21)$plot()
