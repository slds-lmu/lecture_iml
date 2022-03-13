library(iml)
library(ggplot2)
theme_set(theme_bw())

x1 = seq(from = -10, to = 10, length.out = 51)
x2 = seq(from = -10, to = 10, length.out = 51)
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
#   geom_vline(xintercept = x1val, lty = 2, col = 2) +
#   geom_hline(yintercept = f1, lty = 2, col = 2)
#
# p2 = ale2$plot(rug = FALSE) +
#   ggtitle(expression(paste(f[2], " main effect of ", X[2]))) +
#   scale_x_continuous(latex2exp::TeX(r'(Feature $X_2$)')) +
#   geom_vline(xintercept = x2val, lty = 2, col = 2) +
#   geom_hline(yintercept = f2, lty = 2, col = 2)
#
# p12 = ale12$plot(rug = FALSE) + geom_contour(aes(z = .ale), color = "black") +
#   #scale_fill_continuous("value", low = "blue",  high = "yellow") +
#   ggtitle(expression(paste(f[12], " interaction between ", X[1], " and ", X[2]))) +
#   scale_x_continuous(latex2exp::TeX(r'(Feature $X_1$)')) +
#   scale_y_continuous(latex2exp::TeX(r'(Feature $X_2$)')) +
#   geom_vline(xintercept = x1val, lty = 2, col = 2) +
#   geom_hline(yintercept = x2val, lty = 2, col = 2)
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

x1val = 5
x2val = 10
digits = 1
f1 = pdp1$results$.value[round(pdp1$results$x1, digits) == x1val]
f2 = pdp2$results$.value[round(pdp2$results$x2, digits) == x2val]
f12 = pdp12$results$.value[round(pdp12$results$x1, digits) == x1val & round(pdp12$results$x2, digits) == x2val]

f0 + f1 + f2 + f12

f(x1val, x2val)


lab1 = paste("g[1] ==", round(f1, 2))
p1 = pdp1$plot(rug = FALSE) +
  ggtitle(expression(paste(g[1], " main effect of ", X[1]))) +
  scale_x_continuous(latex2exp::TeX(r'(Feature $x_1$)')) +
  scale_y_continuous(latex2exp::TeX(r'($g_{1}$ values)')) +
  geom_vline(xintercept = x1val, lty = 2, col = 2) +
  geom_hline(yintercept = f1, lty = 2, col = 2) +
  geom_label(aes(x = x1val, y = f1, label = lab1),
    col = 2, hjust = 1.1, vjust = -0.1, parse = TRUE)

lab2 = paste("g[2] ==", round(f2, 2))
p2 = pdp2$plot(rug = FALSE) +
  ggtitle(expression(paste(g[2], " main effect of ", X[2]))) +
  scale_x_continuous(latex2exp::TeX(r'(Feature $x_2$)')) +
  scale_y_continuous(latex2exp::TeX(r'($g_{2}$ values)')) +
  geom_vline(xintercept = x2val, lty = 2, col = 2) +
  geom_hline(yintercept = f2, lty = 2, col = 2) +
  geom_label(aes(x = x2val, y = f2, label = lab2),
    col = 2, hjust = 1.1, vjust = -0.1, parse = TRUE)

lab12 = paste("g[12] ==", round(f12, 2))
p12 = pdp12$plot(rug = FALSE) + geom_contour(aes(z = .value), color = "black") +
  #scale_fill_continuous("value", low = "blue",  high = "yellow") +
  scale_fill_gradient2(expression("value"), midpoint = f12, low = 3, mid = "white",
    high = 4, space = "Lab") +
  ggtitle(expression(paste(g[12], " interaction between ", X[1], " and ", X[2]))) +
  scale_x_continuous(latex2exp::TeX(r'(Feature $x_1$)')) +
  scale_y_continuous(latex2exp::TeX(r'(Feature $x_2$)')) +
  geom_vline(xintercept = x1val, lty = 2, col = 2) +
  geom_hline(yintercept = x2val, lty = 2, col = 2) +
  geom_label(aes(x = x1val, y = x2val, label = lab12),
    col = 2, hjust = 1.1, vjust = 1.1, parse = TRUE)

(p1 / p2) | p12
#(p1 + p2) / p12 + patchwork::plot_layout(heights = c(1,2.5))

ggsave("slides/intro/figure/interaction2.pdf", height = 3, width = 8,
  ((p1 / p2) | p12) + patchwork::plot_layout(heights = c(1,2)))
