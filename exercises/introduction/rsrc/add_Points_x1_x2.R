# PREREQ -----------------------------------------------------------------------
library(ggplot2)
library(xtable)
# DATA -------------------------------------------------------------------------
set.seed(1234L)
x1 = round(seq(-1, 1, 0.25), 2)
x2 = round(x1^2+rnorm(length(x1), sd = 0.04), 2)
y = round(5*x1 + -2*x2 + rnorm(length(x1)), 2)
d = data.frame(y, x1, x2)

# tables
d[length(x1)+1,] = colSums(d)
xtable(t(d), )
xtable(t(data.frame(x1-mean(x1),x2-mean(x2))), )

# PLOT -------------------------------------------------------------------------
ggplot(d, aes(x1,x2)) + 
  theme_bw()

ggsave('exercises/01_introduction/figure/add_Points_x1_x2.pdf',width=3,height=2)
