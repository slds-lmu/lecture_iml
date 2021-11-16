# PREREQ -----------------------------------------------------------------------

library(patchwork)
library(ggplot2)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))
source("slides/feature-effects/rsrc/anova_bike.R")

# DATA -------------------------------------------------------------------------

H.global = Interaction$new(pred.bike)
H.twoway = Interaction$new(pred.bike, feature = "temp")

# PLOT -------------------------------------------------------------------------

p1 = H.global$plot() + ggtitle("Overall interactions")
p2 = H.twoway$plot() + scale_x_continuous("2-way interaction strength") +
  ggtitle("2-way interactions with 'temp'")

ggsave("slides/feature-effects/figure/h-statistic.pdf", p1 + p2, width = 8, height = 3)
