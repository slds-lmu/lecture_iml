# PREREQ -----------------------------------------------------------------------

library(ggplot2)
theme_set(theme_bw())
source("anova_bike.R")

# DATA -------------------------------------------------------------------------

pdp = FeatureEffect$new(pred.bike, "temp", method = "pdp+ice")

# PLOT -------------------------------------------------------------------------

p1 = pdp$plot() +
  scale_x_continuous('Temperature', limits = c(0, NA)) +
  scale_y_continuous('Predicted number of bike rentals') +
  xlim(range(bike$temp))

ggsave("../figure/pdp_bike.pdf", height = 3, width = 6, p1)
