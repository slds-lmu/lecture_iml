# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(patchwork)
p = readRDS("slides/feature-effects/figure/ale_scatter.RDS")
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# DATA -------------------------------------------------------------------------

set.seed(10)

# Generate Pseudo Random Variables
x1 = runif(50, -5, 5)
x2 = x1 + rnorm(50, 0, 1)
df_observed = data.frame(x1, x2)

df_permuted = data.frame(expand.grid(x1 = seq(min(x1), max(x1), length = 10),
  x2 = seq(min(x2), max(x2), length = 10))) #data.frame(expand.grid(x1, x2))
  #  sort(x2)[c(1:2, (length(x2)-1):length(x2))])
names(df_permuted) = c("x1", "x2")

# PLOT -------------------------------------------------------------------------

pgrid = p + ylim(-10, 10) +
  geom_point(data = df_observed, aes(x1, x2, col = "observed data points"),
             size = 1.5) +
  geom_point(data = df_permuted, aes(x1, x2, col = "artificial data points \n(created by equidistant grid)"),
    alpha = 1, size = 1, shape = 16) +
  # geom_rect(mapping = aes(ymax = median(x2)-0.5, ymin = min(x2)-0.5,
  #   xmax = max(x1)+0.25, xmin = median(x1) + 0.75), alpha = 0, size = 0.5,
  #   colour = "red", fill = "red") +
  geom_polygon(aes(x = c(-3,5,5), y = c(-7,2.5,-7)), colour = "red", fill = NA) +
  geom_text(aes(x = -3 + (8/2), y = -7), nudge_y = -1,
    label = paste0("extrapolation area"), parse = F, col = "red") +
  scale_color_manual(values = c(2,1)) +
  labs(colour = "Data Points") +
  theme(legend.position = c(0.32, 0.85), legend.title = element_blank())

ppermute = p + ylim(-10, 10) +
  geom_point(data = df_observed, aes(x1, x2, col = "observed data points"),
    size = 1.5) +
  geom_point(data = df_observed, aes(x1, sample(x2),
    col = "artificial data points \n(created by permuting X2)"),
    alpha = 1, size = 1, shape = 16) +
  # geom_rect(mapping = aes(ymax = median(x2)-0.5, ymin = min(x2)-0.5,
  #   xmax = max(x1)+0.25, xmin = median(x1) + 0.75), alpha = 0, size = 0.5,
  #   colour = "red", fill = "red") +
  geom_text(aes(x = -3 + (8/2), y = -7), nudge_y = -1,
    label = paste0("extrapolation area"), parse = F, col = "red") +
  geom_polygon(aes(x = c(-3,5,5), y = c(-7,2.5,-7)), colour = "red", fill = NA) +
  scale_color_manual(values = c(2,1)) +
  labs(colour = "Data Points") +
  theme(legend.position = c(0.3, 0.85), legend.title = element_blank())

ggsave("slides/feature-effects/figure/extrapolation.pdf", pgrid + ppermute,
  width = 8, height = 3)
