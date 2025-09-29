# PREREQ -----------------------------------------------------------------------
library(mlr3)
library(iml)
library(ggplot2)

# DATA -------------------------------------------------------------------------
set.seed(1000)
n = 700
x1 = runif(n, -1, 1)
x2 = rbinom(n, size = 1, prob = 0.5)
y = - 8*x1 + 0.2*x2 + ifelse(x2 == 1, 16*x1, 0) 
dat = data.frame(x1, x2, y)

# fit linear model
mod = lm(y~x1+x2+x1:x2, data = dat)
pred = Predictor$new(mod, data = dat[-which(names(dat) == "y")], y = dat$y)

# PLOT -------------------------------------------------------------------------
# pdp
pdp = FeatureEffect$new(pred, "x1", method = "ice")
p1 = pdp$plot()
p1 = p1 + scale_y_continuous(name = expression(hat(f)[S])) +
  theme_bw() +
  scale_x_continuous(name = expression(x[1])) + 
  geom_line(data = dat[dat$x2 == 1,], aes(x = x1, y = y), color = "lightgreen", lwd =2) + 
  geom_line(data = dat[dat$x2 == 0,], aes(x = x1, y = y), color = "lightblue", lwd = 2) 

ggsave('exercises/feature-effects/figure/pdpinteraction_ICE_curve.pdf',width=3,height=2)
