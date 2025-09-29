# PREREQ -----------------------------------------------------------------------

library(ggplot2)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# DATA -------------------------------------------------------------------------

set.seed(10)

# Generate Pseudo Random Variables
x1 = runif(50, -5, 5)
x2 = x1 + rnorm(50, 0, 1)
df_observed = data.frame(x1, x2)

# PLOT -------------------------------------------------------------------------

p = ggplot() +
  geom_point(data = df_observed, aes(x1, x2), size = 1) +
  labs(x = expression(X[1]), y = expression(X[2])) + 
  ylim(c(-10,10))

saveRDS(object = p, "../figure/ale_scatter.RDS")
ggsave("../figure/ale_scatter.pdf", p, width = 5.5,
       height = 4)
