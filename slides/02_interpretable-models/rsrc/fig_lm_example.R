library(ggplot2)
library(gridExtra)
set.seed(1)
x = runif(50, min = 0, 10)
xnoise = rnorm(50, mean = 0, sd = 5)
y = 6*x + 0.1*x^3 + 0.2*xnoise^3 + rnorm(50, sd = 20) + 5
d = data.frame(y = y, x = x)

lm1 = lm(y ~ x, data = d)

d$y.hat = fitted(lm1)
d$resid.hat = residuals(lm1)
d$xend = d$x
gg1 = ggplot(d, aes(x = x, y = y)) + 
  geom_segment(data=d, aes(x=x, xend=xend, y=y, yend=y.hat), color="red") + 
  geom_abline(intercept=coef(lm1)[1], slope=coef(lm1)[2], col="#A2CD5A", lwd = 2) + 
  geom_point() + 
  xlab("x") + ylab("y") + theme_bw()
d$y.hat2 = coef(lm1)[1]/2 + coef(lm1)[2]*1.5*d$x
d$y.resid.hat = d$y - d$y.hat2
gg2 = ggplot(d, aes(x = x, y = y)) + 
  geom_segment(data=d, aes(x=x, xend=xend, y=y, yend=y.hat2), color="red") + 
  geom_abline(intercept=coef(lm1)[1]/2, slope=coef(lm1)[2]*1.5, col="#A2CD5A", lwd = 2) + 
  geom_point() + 
  xlab("x") + ylab("y") + theme_bw()

res = grid.arrange(gg1, gg2, ncol = 2) 


ggsave("../figure/lm_example.pdf", gg1, width = 5, height = 3)
