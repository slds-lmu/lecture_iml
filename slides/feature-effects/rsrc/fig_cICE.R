# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(patchwork)
source("slides/feature-effects/rsrc/anova_bike.R")

# DATA -------------------------------------------------------------------------

pdp = FeatureEffect$new(pred.bike, "hum", method = "pdp+ice")

# PLOT -------------------------------------------------------------------------

p = pdp$plot() + scale_x_continuous('Humidity in % (hum)') + scale_y_continuous('Predicted bike rentals')
pdp = FeatureEffect$new(pred.bike, "hum", method = "pdp+ice", center.at = min(bike$hum))
pmin = pdp$plot() + scale_x_continuous('Humidity in % (hum)') + scale_y_continuous('Predicted bike rentals')

pres = p + ggtitle("ICE plot") +
  pmin + ggtitle("c-ICE plot at x* = min(hum)") +
  scale_y_continuous('Predicted bike rentals centered at x*')

ggsave("slides/feature-effects/figure/cICE.pdf", pres, width = 8, height = 3)
