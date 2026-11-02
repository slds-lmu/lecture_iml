# PREREQ -----------------------------------------------------------------------

library(ggplot2)
p = readRDS("../figure/ale_scatter.RDS")
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# DATA -------------------------------------------------------------------------
set.seed(10)

# Generate Pseudo Random Variables
x1 = runif(50, -5, 5)
x2 = x1 + rnorm(50, 0, 1)
df_observed = data.frame(x1, x2)

df_permuted = data.frame(expand.grid(x1, x2))
names(df_permuted) = c("x1", "x2")

# PLOT -------------------------------------------------------------------------

pgrid = p + ylim(-10, 10) +
  geom_point(data = df_permuted, aes(x1, x2, col = "artificial (grid points)"), 
             alpha = 0.3, size = 1, shape = 16) +
  geom_point(data = df_observed, aes(x1, x2, col = "observed data points"),
             size = 1.5) +
  scale_color_manual(values = c(2,1)) +
  labs(colour = "Data Points") +
  theme(legend.position = c(0.2, 0.875), legend.title = element_blank())

ggsave("../figure/ale_scatter_grid.pdf", pgrid, width = 5.5,
       height = 4)
