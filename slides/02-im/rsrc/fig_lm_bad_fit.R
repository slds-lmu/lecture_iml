library(ggplot2)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

set.seed(123)

x1 = runif(50, 0, 10)
x2 = (x1-5)^2+rnorm(50)
df = data.frame(x1,x2)

p = ggplot(data = df, aes(x1, x2), size = 1.5) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggsave("../figure/lm_bad_fit.pdf", p,
       width = 5, height = 3)
