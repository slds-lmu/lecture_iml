# PREREQ -----------------------------------------------------------------------

library(ggplot2)
theme_set(theme_bw())
source("anova_bike.R")

# DATA -------------------------------------------------------------------------

pdp1 = FeatureEffect$new(pred.bike, feature = "temp", method = "pdp", center.at = mean.pred)

# PLOT -------------------------------------------------------------------------

p1 = pdp1$plot() + xlab('Temperature') + ylab('Predicted number of bike rentals')
p1 = p1 + xlim(range(bike$temp))

ggsave("../figure/anova_bike1.jpg", plot = p1, width = 6.5, height = 5)
