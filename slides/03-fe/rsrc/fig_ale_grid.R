# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(mlr3)
library(mlr3learners)
library(iml)
library(patchwork)
theme_set(theme_bw())
source("ale_dat.R")

# DATA -------------------------------------------------------------------------

task = TaskRegr$new("regr_tsk", backend = DAT, target = "y")
ranger = lrn("regr.ranger")
ranger$train(task)
mseval = ranger$predict_newdata(test)$response

df = expand.grid(
  x1 = seq(min(test$x1), max(test$x1), length = 50),
  x2 = seq(min(test$x2), max(test$x2), length = 50)
)
df$y = ranger$predict_newdata(df)$response

pred = Predictor$new(ranger, data = test)
fnames = c("x1", "x2")

ale = lapply(fnames, function(x)
  FeatureEffect$new(pred, feature = x, grid.size = 30)
)

pdp = lapply(fnames, function(x) {
  eff = FeatureEffect$new(pred, feature = x, grid.size = 30, method = "pdp")
  # center like aleplots, taken from ALEPlot package
  xmin = min(test[[x]])
  xgridval = eff$results[,1]
  a = cut(test[[x]], breaks = c(xmin - (xgridval[2] - xgridval[1]), xgridval),
          include.lowest = TRUE)
  b = as.numeric(table(a))
  eff$results$.value = eff$results$.value - sum(eff$results$.value * b)/sum(b)
  

  return(eff)
}
)

# PLOT -------------------------------------------------------------------------

surf = ggplot(data = df, aes(x = x1, y = x2, z = y)) +
  geom_contour_filled() +
  geom_point(data = test, aes(x1, x2)) +
  ggtitle(paste0("performance: ", mseval))

ale1 = ale[[1]]$plot() + ylab("") +
  geom_function(fun = function(x) x - 0.5, col = "red") +
  ggtitle("ALE x1")

ale2 = ale[[2]]$plot() + ylab("") +
  geom_function(fun = function(x) x^2 - (1/3+0.05^2), col = "red") +
  ggtitle("ALE x2")

pdp1 = pdp[[1]]$plot() + ylab("") +
  geom_function(fun = function(x) x - 0.5, col = "red") +
  ggtitle("PDP x1")

pdp2 = pdp[[2]]$plot() + ylab("") +
  geom_function(fun = function(x) x^2 - (1/3+0.05^2), col = "red") +
  ggtitle("PDP x2")

p = surf | ((ale1 + ale2) / (pdp1 + pdp2))
p_final = p + plot_layout(heights = c(3, 2, 2), guides = "collect") & theme(legend.position = 'bottom')

ggsave("../figure/ale_grid.pdf", p_final, width = 10/1.25, height = 4/1.25)
