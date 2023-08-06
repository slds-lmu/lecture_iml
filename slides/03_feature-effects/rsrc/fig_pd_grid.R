# PREREQ -----------------------------------------------------------------------

library(gridExtra)
library(ggplot2)

# DATA -------------------------------------------------------------------------

set.seed(10)
x1 = runif(20, -5, 5)
x2 = x1 + rnorm(20, 0, 1)
df_observed = data.frame(x1, x2)
df_permuted = data.frame(expand.grid(x1, x2))
names(df_permuted) = c("x1", "x2")

dens = density(x2)
d = data.frame(x = dens$x, y = dens$y)

# PLOT -------------------------------------------------------------------------

p1 = ggplot() +
  geom_point(data = df_observed, aes(x1, x2),
             shape = 3, color = "red", size = 2, stroke = 2) +
  theme_bw() + ylim(range(d$x))

p2 = ggplot() +
  geom_point(data = df_permuted, aes(x1, x2),
             shape = 3, color = "green", size = 2, stroke = 1) +
  geom_point(data = df_observed, aes(x1, x2),
             shape = 3, color = "red", size = 2, stroke = 2) +
  theme_bw() + ylim(range(d$x))

p2 = p2 + geom_line(data = d, aes(x = sort(x1)[19] + 5*y, y = x), alpha = 0.25, color = "red")
grid = grid.arrange(p1, p2, ncol = 2, respect = TRUE)

ggsave("../figure/pd_grid.pdf", grid, width = 8, height = 4)
