# PREREQ -----------------------------------------------------------------------

library(ggplot2)
source("slides/feature-effects/rsrc/anova_bike.R")

# DATA -------------------------------------------------------------------------

set.seed(123)
pred.sub = Predictor$new(mod, data = bike[721:731,], y = bike$cnt[721:731])
pdp = FeatureEffect$new(pred.sub, "temp", method = "ice")

# PLOT -------------------------------------------------------------------------

p1 = pdp$plot() + scale_x_continuous('Temperature') + scale_y_continuous('Predicted bike rentals', limits = c(0, 4000))
ggsave("slides/feature-effects/figure/ice_bike10obs.jpg", p1)
