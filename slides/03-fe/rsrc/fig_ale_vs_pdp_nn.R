source("ale_vs_pdp_data.R")

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

# Save Plots ------------------------------------------------------------------------

ggsave("../figure/ale_vs_pdp_nn.png", res.nn, width = 9.4, height = 5.5, dpi = 150)