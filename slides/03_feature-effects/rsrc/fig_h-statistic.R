# PREREQ -----------------------------------------------------------------------

library(patchwork)
library(ggplot2)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))
source("anova_bike.R")

# DATA -------------------------------------------------------------------------

H.global = Interaction$new(pred.bike, grid.size = 10)
H.twoway = Interaction$new(pred.bike, feature = "temp", grid.size = 10)

# PLOT -------------------------------------------------------------------------

p1 = H.global$plot() + ggtitle("Overall interactions")
p2 = H.twoway$plot() + scale_x_continuous("2-way interaction strength") +
  ggtitle("2-way interactions with 'temp'")

ggsave("../figure/h-statistic.pdf", p1 + p2, width = 8, height = 3)
