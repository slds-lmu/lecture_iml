# PREREQ -----------------------------------------------------------------------
library(ggplot2)
library(xtable)
# DATA -------------------------------------------------------------------------
set.seed(1234L)
x1 = round(seq(-1, 1, 0.2), 2)
x2 = round(x1^2+rnorm(length(x1), sd = 0.04), 2)
y = round(5*x1 + -2*x2 + rnorm(length(x1)), 2)
d = data.frame(y, x1, x2)

# PLOT -------------------------------------------------------------------------
ggplot(d, aes(x = x1, y = x2)) + 
  theme_bw()

ggsave('exercises/feature-effects/figure/add_Points_x1_x2.pdf',width=3,height=2)
