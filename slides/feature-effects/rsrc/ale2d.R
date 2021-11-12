# PREREQ -----------------------------------------------------------------------

source(textConnection(
  readLines("slides/feature-effects/rsrc/ale1d.R")[1:17]
))

# DATA -------------------------------------------------------------------------

pdp = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "pdp", grid.size = 10)
ale = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "ale", grid.size = 10)

# PLOT -------------------------------------------------------------------------

pdp_plot <- pdp$plot() + theme(legend.position = "top", legend.key.width = unit(1.5,"cm")) + ggtitle("Bivariate PD")
ale_plot <- ale$plot() + theme(legend.position = "top", legend.key.width = unit(1.5,"cm")) + ggtitle("Second order ALE")
ale2d = gridExtra::grid.arrange(pdp_plot, ale_plot, nrow = 1, ncol = 2)

ggsave("slides/feature-effects/figure/ale2d.pdf", ale2d, width = 8, height = 4)
