# Same plot as in chapter 4, section on ALE

# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(patchwork)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))
source("anova_bike.R")

# DATA -------------------------------------------------------------------------

ale = FeatureEffect$new(pred.bike, "hum", method = "ale", grid.size = 50)
alet = FeatureEffect$new(pred.bike, "temp", method = "ale", grid.size = 50)

# PLOT -------------------------------------------------------------------------

ale_plot <- ale$plot() + scale_y_continuous('First order ALE of hum') +
  xlab("hum: Humidity")
ale_plot_t <- alet$plot() + scale_y_continuous('First order ALE of temp') +
  xlab("temp: Temperature")

ggsave("../figure/ale1d.pdf",
  ale_plot / ale_plot_t,
  width = 4, height = 4)
