# PREREQ -----------------------------------------------------------------------
library(ggplot2)
library(xtable)
library(mgcv)
# DATA -------------------------------------------------------------------------
set.seed(1234L)
x1 = round(seq(-1, 1, 0.2), 2)
x2 = round(x1^2+rnorm(length(x1), sd = 0.04), 2)
y = round(5*x1 + -2*x2 + rnorm(length(x1)), 2)
d = data.frame(y, x1, x2)

mod <- gam(x2 ~ s(x1), data = d) 

s <- summary(mod)

capture.output(s, file = "GAM_Output.txt")
