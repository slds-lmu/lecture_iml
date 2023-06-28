# PREREQ -----------------------------------------------------------------------

library(ggplot2)
theme_set(theme_bw())
source("anova_bike.R")

# DATA -------------------------------------------------------------------------

pdp12 = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "pdp")

pdp1uncentered = FeatureEffect$new(pred.bike, feature = "temp", method = "pdp")
pdp2uncentered = FeatureEffect$new(pred.bike, feature = "hum", method = "pdp")

yhatnew = merge(pdp12$results, pdp2uncentered$results[, 1:2], by = "hum")
yhatnew = merge(yhatnew, pdp1uncentered$results[, 1:2], by = "temp")
yhatnew$.y.hat.x = yhatnew$.value.x - yhatnew$.value.y - yhatnew$.value
yhatnew = yhatnew[1:4]
names(yhatnew) = c("temp", "hum", ".value", ".type")
pdp12$results = yhatnew

# PLOT -------------------------------------------------------------------------

p12 = pdp12$plot() +
  scale_x_continuous('Temperature', limits = c(0, NA))+
  scale_y_continuous('Humidity', limits = c(0, NA))

ggsave("../figure/anova_bike12.jpg", plot = p12, width = 6.5, height = 5)
