# PREREQ -----------------------------------------------------------------------
library(ALEPlot)
library(mlr)
library(iml)
library(ggplot2)
library(patchwork)
library(devtools)
#source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# DATA -------------------------------------------------------------------------

simData = function(n) {
  x = runif(n, min = 0, max = 1)
  x1 = x + rnorm(n, 0, 0.05)
  x2 = x + rnorm(n, 0, 0.05)
  y = x1 + x2^2 + rnorm(n, 0, 0.2)
  return(data.frame(y, x1, x2))
}

set.seed(1)
n = 5000
DAT = simData(n)

#test.ind = sample(1:n, size = 0.5*n)
test = simData(100000) #[test.ind, ]
#DAT = DAT[-test.ind, ]

#breaks = c(-Inf, seq(min(DAT$y), max(DAT$y), by = 0.5), Inf)
breaks = c(-Inf, seq(0, 2, by = 0.5), Inf)

tsk = makeRegrTask(data = DAT, target = "y")

# NN MODEL ------------------------------------------------------------------------

lrn.nn = makeLearner("regr.nnet", skip = F, size = 10,
  decay = 0.0001, maxit = 1000, trace = F)
mod.nn = mlr::train(lrn.nn, tsk)

nn.eval = performance(predict(mod.nn, newdata = test))

# lrn.list = list(lrn,
#   makeLearner("regr.featureless"),
#   makeLearner("regr.lm"),
#   makeLearner("regr.ranger"))
# bench = benchmark(lrn.list, tsk, resamplings = cv3)
# bench
#m = mod.nn$learner.model
#m$call$formula = y~x1+x2
#plot(m)


pred = Predictor$new(mod.nn, data = DAT)
# plotLearnerPrediction(lrn, task = tsk)

df = expand.grid(
  x1 = seq(min(DAT$x1), max(DAT$x1), length = 50),
  x2 = seq(min(DAT$x2), max(DAT$x2), length = 50)
)
df$y = predict(mod.nn, newdata = df)$data$response

# Create NN PLOT -------------------------------------------------------------------------

surf.nn = ggplot(data = df, aes(x = x1, y = x2, z = y)) +
  geom_contour_filled(breaks = breaks) +
  geom_point(data = DAT, aes(x1, x2)) +
  ggtitle(paste0("Model: ", "nnet", ", MSE: ", round(nn.eval, 5))) +
  NULL

fnames = c("x1", "x2")
ale = lapply(fnames, function(x)
  FeatureEffect$new(pred, feature = x, grid.size = 20)
)
pdp = lapply(fnames, function(x) {
  eff = FeatureEffect$new(pred, feature = x, grid.size = 20, method = "pdp")
  # center like aleplots, taken from ALEPlot package
  xmin = min(DAT[[x]])
  xgridval = eff$results[,1]
  a = cut(DAT[[x]], breaks = c(xmin - (xgridval[2] - xgridval[1]), xgridval),
    include.lowest = TRUE)
  b = as.numeric(table(a))
  eff$results$.value = eff$results$.value - sum(eff$results$.value * b)/sum(b)

  #eff$results$.value = eff$results$.value - mean(eff$results$.value)
  return(eff)
}
)
# pdpcenter = lapply(fnames, function(x) {
#   eff = FeatureEffect$new(pred, feature = x, grid.size = 20, method = "pdp")
#   eff$results$.value = eff$results$.value - mean(eff$results$.value)
#   return(eff)
# }
# )

ale1 = ale[[1]]$plot() + ylab("") +
  geom_function(fun = function(x) x - 0.5, col = "red") +
  ggtitle("ALE x1")

ale2 = ale[[2]]$plot() + ylab("") +
  geom_function(fun = function(x) x^2 - (1/3+0.05^2), col = "red") +
  ggtitle("ALE x2")

pdp1 = pdp[[1]]$plot() + geom_line(aes(col = "estimated")) + ylab("") +
  geom_function(fun = function(x) x - 0.5, aes(col = "true")) +
  ggtitle("PDP x1") + labs(color = "Marginal Effect") +
  scale_color_manual(values = c("estimated" = "black", "true" = "red"))

# + ylab("") +
#   geom_function(fun = function(x) x - 0.5, col = "red") +
#   ggtitle("PDP x1")

pdp2 = pdp[[2]]$plot() + ylab("") +
  geom_function(fun = function(x) x^2 - (1/3+0.05^2), col = "red") +
  ggtitle("PDP x2")

#pp = FeatureEffect$new(pred, feature = c("x1", "x2"), grid.size = 20, method = "pdp")
#pp$plot() + geom_point(data = DAT, aes(x = x1, y = x2))

p = surf.nn | ((ale1 + ale2) / (pdp1 + pdp2)) #surf.nn / (ale1 + pdp1) / (ale2 + pdp2)

res.nn = p + plot_layout(heights = c(3, 2, 2), guides = "collect") & theme(legend.position = 'bottom') #& guides(fill=guide_legend(nrow = 1, byrow = TRUE))

# Create RF MODEL ------------------------------------------------------------------------

#lrn.rf = makeLearner("regr.ranger")
#mod.rf = mlr::train(lrn.rf, tsk)

lrn.lm = makeLearner("regr.lm")
mod.lm = mlr::train(lrn.lm, tsk)
mod.lm$learner.model = lm(y ~ x1 + poly(x2, degree = 2, raw = TRUE), data = DAT)

lm.eval = performance(predict(mod.lm, newdata = test))

pred = Predictor$new(mod.lm, data = DAT)

df = expand.grid(
  x1 = seq(min(DAT$x1), max(DAT$x1), length = 50),
  x2 = seq(min(DAT$x2), max(DAT$x2), length = 50)
)
df$y = predict(mod.lm, newdata = df)$data$response


# RF PLOT -------------------------------------------------------------------------

surf.lm = ggplot(data = df, aes(x = x1, y = x2, z = y)) +
  geom_contour_filled(breaks = breaks) +
  geom_point(data = DAT, aes(x1, x2)) +
  ggtitle(paste0("Model: ", "correct specified polynomial", ", MSE: ", round(lm.eval, 5))) +
  NULL

ale = lapply(fnames, function(x)
  FeatureEffect$new(pred, feature = x, grid.size = 20)
)

pdp = lapply(fnames, function(x) {
  eff = FeatureEffect$new(pred, feature = x, grid.size = 20, method = "pdp")
  # center like aleplots, taken from ALEPlot package
  xmin = min(DAT[[x]])
  xgridval = eff$results[,1]
  a = cut(DAT[[x]], breaks = c(xmin - (xgridval[2] - xgridval[1]), xgridval),
    include.lowest = TRUE)
  b = as.numeric(table(a))
  eff$results$.value = eff$results$.value - sum(eff$results$.value * b)/sum(b)

  #eff$results$.value = eff$results$.value - mean(eff$results$.value)
  return(eff)
}
)

ale1 = ale[[1]]$plot() + ylab("") +
  geom_function(fun = function(x) x - 0.5, col = "red") +
  ggtitle("ALE x1")

ale2 = ale[[2]]$plot() + ylab("") +
  geom_function(fun = function(x) x^2 - (1/3+0.05^2), col = "red") +
  ggtitle("ALE x2")#ggtitle(expression("ALE"~x[2]))

pdp1 = pdp[[1]]$plot() + geom_line(aes(col = "estimated")) + ylab("") +
  geom_function(fun = function(x) x - 0.5, aes(col = "true")) +
  ggtitle("PDP x1") + labs(color = "Marginal Effect") +
  scale_color_manual(values = c("estimated" = "black", "true" = "red"))

pdp2 = pdp[[2]]$plot() + ylab("") +
  geom_function(fun = function(x) x^2 - (1/3+0.05^2), col = "red") +
  ggtitle("PDP x2")

p = surf.lm | ((ale1 + ale2) / (pdp1 + pdp2)) #surf / (ale1 + pdp1) / (ale2 + pdp2)
res.lm = p + plot_layout(heights = c(3, 2, 2), guides = "collect") & theme(legend.position = 'bottom') #& guides(fill=guide_legend(nrow = 1, byrow = TRUE))

# Save Plots ------------------------------------------------------------------------

surf = (surf.nn | surf.lm) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave("slides/feature-effects/figure/ale_vs_pdp_surf.png", surf, width = 9.4, height = 5.5, dpi = 150)

ggsave("slides/feature-effects/figure/ale_vs_pdp_nn.png", res.nn, width = 9.4, height = 5.5, dpi = 150)

ggsave("slides/feature-effects/figure/ale_vs_pdp_lm.png", res.lm, width = 9.4, height = 5.5, dpi = 150)
