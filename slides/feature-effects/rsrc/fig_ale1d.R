# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(patchwork)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))
source("slides/feature-effects/rsrc/anova_bike.R")

# DATA -------------------------------------------------------------------------

pdp = FeatureEffect$new(pred.bike, "hum", method = "pdp", grid.size = 50)
ale = FeatureEffect$new(pred.bike, "hum", method = "ale", grid.size = 50)

# PLOT -------------------------------------------------------------------------

pdp_plot <- pdp$plot() + scale_y_continuous('Univariate PD on humidity')
ale_plot <- ale$plot() + scale_y_continuous('First order ALE of humidity')

ggsave("slides/feature-effects/figure/ale1d.pdf", pdp_plot + ale_plot, width = 8, height = 4)
