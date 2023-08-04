# correlation
n = 1000
x1 = rnorm(n, 0, 1)
x2 = x1 + rnorm(n, 0, 1)
df = data.frame(x1, x2)
library(ggplot2)
p = ggplot(df, aes(x1, x2)) +
  geom_point(pch = 21, color = "steelblue") +
  theme_bw()

ggsave(filename = "../figure/correlation.png", p)
