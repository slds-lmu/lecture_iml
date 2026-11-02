source("ale_vs_pdp_data.R")

# Create NN PLOT ---------------------------------------------------------------
surf.nn = ggplot(data = df, aes(x = x1, y = x2, z = y)) +
  geom_contour_filled(breaks = breaks) +
  geom_point(data = DAT, aes(x1, x2)) +
  ggtitle(paste0("Model: ", "nnet", ", MSE: ", round(nn.eval, 5))) +
  NULL

fnames = c("x1", "x2")
ale = lapply(fnames, function(x)
  FeatureEffect$new(pred, feature = x, grid.size = 20)
)

# RF PLOT -------------------------------------------------------------------------

surf.lm = ggplot(data = df, aes(x = x1, y = x2, z = y)) +
  geom_contour_filled(breaks = breaks) +
  geom_point(data = DAT, aes(x1, x2)) +
  ggtitle(paste0("Model: ", "correct specified polynomial", ", MSE: ", round(lm.eval, 5))) +
  NULL

ale = lapply(fnames, function(x)
  FeatureEffect$new(pred, feature = x, grid.size = 20)
)

# Save Plots -------------------------------------------------------------------
surf = (surf.nn | surf.lm) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave("../figure/ale_vs_pdp_surf.png", surf, width = 9.4, height = 5.5, dpi = 150)
