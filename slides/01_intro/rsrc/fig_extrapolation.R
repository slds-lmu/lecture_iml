# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(patchwork)
p = readRDS("ale_scatter.RDS")
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# DATA -------------------------------------------------------------------------

set.seed(10)

# Generate Pseudo Random Variables
x1 = rnorm(50, 173, 13)
x2 = (x1-mean(x1))/5 + 39.3 + rnorm(50, 0, 1)
df_observed = data.frame(x1, x2)

df_permuted = data.frame(expand.grid(x1 = seq(min(x1), max(x1), length = 8),
  x2 = seq(min(x2), max(x2), length = 10))) #data.frame(expand.grid(x1, x2))
  #  sort(x2)[c(1:2, (length(x2)-1):length(x2))])
names(df_permuted) = c("x1", "x2")

# PLOT -------------------------------------------------------------------------

pgrid = p + ylim(31.7, 44) +
  geom_point(data = df_observed, aes(x1, x2, col = "observed data points"),
             size = 1.5) +
  geom_point(data = df_permuted, aes(x1, x2, col = "artificial data points \n(created by equidistant grid)"),
    alpha = 1, size = 1, shape = 16) +
  # geom_rect(mapping = aes(ymax = median(x2)-0.5, ymin = min(x2)-0.5,
  #   xmax = max(x1)+0.25, xmin = median(x1) + 0.75), alpha = 0, size = 0.5,
  #   colour = "red", fill = "red") +
  geom_polygon(aes(x = c(145,189,189), y = c(32.6,41,32.6)), colour = "#56B4E9", fill = NA) +
  geom_polygon(aes(x = c(142.5,142.5,180), y = c(36.7,44,44)), colour = "#56B4E9", fill = NA) +
  geom_text(aes(x = 150 + (37.5/2), y = 32.8), nudge_y = -1,
    label = paste0("extrapolation area"), parse = F, col = "#56B4E9") +
  scale_color_manual(values = c(2,1)) +
  labs(x="height", y= "shoe size", colour = "Data Points") +
  theme(legend.position = "bottom", legend.title = element_blank())

df_observed$x2_perm = sample(df_observed$x2)

df_long = rbind(cbind(df_observed[,c("x1","x2")], group = 1:nrow(df_observed)), cbind(setNames(df_observed[,c("x1","x2_perm")], c("x1", "x2")), group = 1:nrow(df_observed)))

ppermute = p + ylim(31.7, 44) +
  geom_point(data = df_observed, aes(x1, x2, col = "observed data points"),
    size = 1.5) +
  geom_point(data = df_observed, aes(x1, x2_perm,
    col = "artificial data points \n(created by permuting X2)"),
    alpha = 1, size = 1, shape = 16) +
  geom_line(data = df_long, aes(x1, x2, group = group), alpha = 0.2, col = "red", lty = 2) +
  # geom_rect(mapping = aes(ymax = median(x2)-0.5, ymin = min(x2)-0.5,
  #   xmax = max(x1)+0.25, xmin = median(x1) + 0.75), alpha = 0, size = 0.5,
  #   colour = "red", fill = "red") +
  geom_polygon(aes(x = c(145,189,189), y = c(32.6,41,32.6)), colour = "#56B4E9", fill = NA) +
  geom_polygon(aes(x = c(142.5,142.5,180), y = c(36.7,44,44)), colour = "#56B4E9", fill = NA) +
  geom_text(aes(x = 150 + (37.5/2), y = 32.8), nudge_y = -1,
            label = paste0("extrapolation area"), parse = F, col = "#56B4E9") +
  scale_color_manual(values = c(2,1)) +
  labs(x="height", y= "shoe size", colour = "Data Points") +
  theme(legend.position =  "bottom", legend.title = element_blank()) 

ggsave("../figure/extrapolation.pdf", pgrid + ppermute,
  width = 10, height = 3)
