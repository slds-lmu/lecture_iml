# PREREQ -----------------------------------------------------------------------
library(ggplot2)
library(xtable)

# DATA -------------------------------------------------------------------------
set.seed(1234L)
x1 = seq(-1, 1, 0.2)
x2 = x1^2 + rnorm(length(x1), sd = 0.04)
y = 5 * x1 + -2 * x2 + rnorm(length(x1))
d = data.frame(y, x1, x2)

# PLOT -------------------------------------------------------------------------
ggplot(d, aes(x = x1, y = x2)) +
  geom_point() +
  theme_bw()

ggsave("exercises/feature-effects/figure/add_Points_x1_x2_sol.pdf", width = 3, height = 2)
