# PREREQ -----------------------------------------------------------------------

library(ggplot2)
source("slides/feature-effects/rsrc/anova_bike.R")

# DATA -------------------------------------------------------------------------

set.seed(123)
pdp.2feature = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "pdp")

# PLOT -------------------------------------------------------------------------

pdp2d_bike = pdp.2feature$plot() +
  geom_point(data = bike, mapping = aes(x = temp, y = hum), alpha = 0.5)

ggsave("slides/feature-effects/figure/pdp2d_bike.jpg", pdp2d_bike)
