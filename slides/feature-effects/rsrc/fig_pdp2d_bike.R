# PREREQ -----------------------------------------------------------------------

library(ggplot2)
theme_set(theme_bw())
source("slides/feature-effects/rsrc/anova_bike.R")

# DATA -------------------------------------------------------------------------

set.seed(123)
pdp.2feature = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "pdp")

# PLOT -------------------------------------------------------------------------

pdp2d_bike = pdp.2feature$plot() +
  geom_point(data = bike, mapping = aes(x = temp, y = hum), alpha = 0.5) +
  ylab("Humidity") + xlab("Temperature")

ggsave("slides/feature-effects/figure/pdp2d_bike.pdf", pdp2d_bike)
