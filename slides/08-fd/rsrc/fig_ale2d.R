# Same plot as in chapter 4, section on ALE

# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(patchwork)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))
source("anova_bike.R")

# DATA -------------------------------------------------------------------------

ale = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "ale", grid.size = 20)

# PLOT -------------------------------------------------------------------------

ale_plot <- ale$plot() + theme(legend.position = "top", legend.key.width = unit(1.5,"cm")) + ggtitle("Second order ALE") + scale_fill_viridis_b("ALE")

ggsave("../figure/ale2d.pdf", ale_plot, width = 4, height = 4)
