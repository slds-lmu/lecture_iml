# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(patchwork)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))
source("anova_bike.R")

# DATA -------------------------------------------------------------------------

pdp = FeatureEffect$new(pred.bike, "hum", method = "pdp", grid.size = 50)
pdpt = FeatureEffect$new(pred.bike, "temp", method = "pdp", grid.size = 50)
ale = FeatureEffect$new(pred.bike, "hum", method = "ale", grid.size = 50)
alet = FeatureEffect$new(pred.bike, "temp", method = "ale", grid.size = 50)

# PLOT -------------------------------------------------------------------------

pdp_plot <- pdp$plot() + scale_y_continuous('Univariate PD of hum') +
  xlab("hum: Humidity")
pdp_plot_t <- pdpt$plot() + scale_y_continuous('Univariate PD of temp') +
  xlab("temp: Temperature")
ale_plot <- ale$plot() + scale_y_continuous('First order ALE of hum') +
  xlab("hum: Humidity")
ale_plot_t <- alet$plot() + scale_y_continuous('First order ALE of temp') +
  xlab("temp: Temperature")

ggsave("../figure/ale1d.pdf",
  (pdp_plot + pdp_plot_t) / (ale_plot + ale_plot_t),
  width = 8, height = 4)
