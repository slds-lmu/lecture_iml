# PREREQ -----------------------------------------------------------------------

library(ggplot2)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# DATA -------------------------------------------------------------------------

set.seed(10)

# Generate Pseudo Random Variables
x1 = rnorm(50, 173, 13)
x2 = (x1-mean(x1))/5 + 39.3 + rnorm(50, 0, 1)
df_observed = data.frame(x1, x2)

# PLOT -------------------------------------------------------------------------

p = ggplot() +
  geom_point(data = df_observed, aes(x1, x2), size = 1) +
  labs(x = expression(X[1]), y = expression(X[2])) 

saveRDS(object = p, "ale_scatter.RDS")
ggsave("ale_scatter.pdf", p, width = 5.5,
       height = 4)
