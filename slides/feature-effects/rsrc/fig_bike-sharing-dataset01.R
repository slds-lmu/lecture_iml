# PREREQ -----------------------------------------------------------------------

library(ggplot2)
source("slides/feature-effects/rsrc/anova_bike.R")

# DATA -------------------------------------------------------------------------

pdp = FeatureEffect$new(pred.bike, "temp", method = "pdp+ice")

# PLOT -------------------------------------------------------------------------

p1 = pdp$plot() + 
  scale_x_continuous('Temperature', limits = c(0, NA)) +
  scale_y_continuous('Predicted number of bike rentals') +
  xlim(range(bike$temp))

ggsave("slides/feature-effects/figure/bike-sharing-dataset01.png", p1)
