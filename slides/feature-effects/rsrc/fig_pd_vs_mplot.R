# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(mvtnorm)
library(patchwork)
library(iml)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# DATA -------------------------------------------------------------------------

set.seed(1)
sig = matrix(c(1,0.9,0.9,1), byrow = TRUE, ncol = 2)
df_observed = as.data.frame(rmvnorm(500, sigma = sig))
df_observed$y = - 1*df_observed$V1 + 2*df_observed$V2 + rnorm(500)

mod = lm(y ~ V1 + V2, data = df_observed)
pred = Predictor$new(mod, data = df_observed, y = df_observed$y)
pdp = FeatureEffect$new(pred, feature = "V1", method = "pdp")


# FUNCTION ---------------------------------------------------------------------

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
mpl = mplot(df_observed, "V1", target = "y", eps = 0.5)

# PLOT -------------------------------------------------------------------------

p1 = ggplot(data = df_observed, aes(V1, V2)) +
  geom_point() +
  ggpubr::stat_cor(method = "pearson", aes(label = gsub("R", "Cor(x[1],x[2])", ..r.label..))) +
  labs(x = expression(x[1]), y = expression(x[2])) +
  theme_minimal()

p2 = ggplot() +
  stat_function(data = NULL, fun = function(x) -1*x, mapping = aes(col = "function f(x) = -x")) +
  geom_line(data = pdp$results, aes(V1, .value, col = "PD plot")) +
  geom_line(data = mpl, aes(x, mplot, col = "M-plot")) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(colour = "Method", x = expression(x[1]), y = expression("Marginal Effect "~hat(f)[1](x[1]))) +
  theme_minimal() + theme(legend.position="bottom")

ggsave("../figure/pd_vs_mplot.pdf", p1 + p2, width = 10/1.25, height = 4/1.25)
