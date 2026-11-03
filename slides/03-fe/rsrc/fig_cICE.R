# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(patchwork)
library(ggrastr)
source("anova_bike.R")
theme_set(theme_bw())

# DATA -------------------------------------------------------------------------

pdp = FeatureEffect$new(pred.bike, "hum", method = "pdp+ice")
pdpt = FeatureEffect$new(pred.bike, "temp", method = "pdp+ice")

# PLOT -------------------------------------------------------------------------

p = pdp$plot() + scale_x_continuous('Humidity in % (hum)') + scale_y_continuous('Predicted bike rentals')
pdp = FeatureEffect$new(pred.bike, "hum", method = "pdp+ice", center.at = min(bike$hum))
pmin = pdp$plot() + scale_x_continuous('Humidity in % (hum)') + scale_y_continuous('Predicted bike rentals')

pt = pdpt$plot() + scale_x_continuous('Temperature in °C (temp)') + scale_y_continuous('Predicted bike rentals')
pdpt = FeatureEffect$new(pred.bike, "temp", method = "pdp+ice", center.at = min(bike$temp))
pmint = pdpt$plot() + scale_x_continuous('Temperature in °C (temp)') + scale_y_continuous('Predicted bike rentals')

pres = p + ggtitle("ICE plot (hum)") +
  pmin + ggtitle("c-ICE plot at x' = min(hum)") +
  scale_y_continuous("Predicted bike rentals \ncentered at x'")
prest = pt + ggtitle("ICE plot (temp)") +
  pmint + ggtitle("c-ICE plot at x' = min(temp)") +
  scale_y_continuous("Predicted bike rentals \ncentered at x'")

pres = rasterise(pres, dev = "ragg")
prest = rasterise(prest, dev = "ragg")

ggsave("../figure/cICE.pdf", pres / prest, width = 8, height = 5)
