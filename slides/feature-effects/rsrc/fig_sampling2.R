# PREREQ -----------------------------------------------------------------------

library(ggplot2)
theme_set(theme_bw())

# DATA -------------------------------------------------------------------------

set.seed(1234)
n = 100
ngrid = 11
x1 = c(runif(n-2, 0, 400)/100, 3.86, 6.78)
x2 = c(runif(n-2, 0, 460)/100, 4.61, 8.31)

# grid
grid.x1 = seq(min(x1), max(x1), length = ngrid+1)
grid.x2 = seq(min(x2), max(x2), length = ngrid+1)

const.x2 = rep(sort(x2, decreasing = T)[2], length = ngrid+1)
data1.const = expand.grid(x1 = grid.x1, x2 = const.x2)
data1.const$method = "equidistant grid"


data1 = expand.grid(x1 = grid.x1, x2 = grid.x2)
data1$method = "equidistant grid"

# subsample
grid.x1 = sample(x1, size = ngrid+1)
grid.x2 = sample(x2, size = ngrid+1)

data2.const = expand.grid(x1 = grid.x1, x2 = const.x2)
data2.const$method = "randomly sampled grid"

data2 = expand.grid(x1 = grid.x1, x2 = grid.x2)
data2$method = "randomly sampled grid"
# quantile
grid.x1 = quantile(x1, 0:(ngrid)/(ngrid), type = 1)
grid.x2 = quantile(x2, 0:(ngrid)/(ngrid), type = 1)

data3.const = expand.grid(x1 = grid.x1, x2 = const.x2)
data3.const$method = "quantile grid"

data3 = expand.grid(x1 = grid.x1, x2 = grid.x2)
data3$method = "quantile grid"

data = rbind(data1, data2, data3)
data$method = factor(data$method, levels = c("equidistant grid", "randomly sampled grid", "quantile grid"))

data.const = rbind(data1.const, data2.const, data3.const)
data.const$method = factor(data.const$method, levels = c("equidistant grid", "randomly sampled grid", "quantile grid"))

ind = which(x2 == sort(x2, decreasing = TRUE)[2])

# PLOT -------------------------------------------------------------------------

p = ggplot(data = data, aes(x1, x2)) +
  geom_point(data = data.frame(x1 = x1[ind], x2 = x2[ind]), aes(x1, x2), size = 3, col = "blue") +
  geom_point(data = data.frame(x1 = x1, x2 = x2), aes(x1, x2), alpha = 0.25) +
  geom_rug(data = data.frame(x1 = x1, x2 = x2), aes(x = x1), alpha = 0.25, sides = "b") +
  geom_point(data = data.const, aes(x1, x2), shape = 4, alpha = 0.5, col = "red") +
  facet_grid(~ method) +
  xlab("Feature"~X[S]) +
  ylab("Feature"~X[-S]) +
  ggtitle("Grid points for"~X[S]~"(red) for highlighted observation (blue)")


ann_text = data.frame(x1 = max(x1), x2 = x2[ind], diff = max(diff(sort(x1))), #lab = "Text",
                      method = factor("equidistant grid", levels = levels(data$method)))
p = p + 
  geom_text(data = ann_text, aes(x = x1-diff/2, y = x2), nudge_y = -1, label = paste0("unrealistic?"), parse = F, col = "red") +
  geom_rect(data = ann_text, mapping = aes(ymax = x2 + 0.5, ymin = x2 - 0.5,
                                           xmax = x1 - 0.3, xmin = (x1-diff) + 0.3), alpha = 0, size = 0.5,
            colour = "red", fill = "red") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("slides/03_feature-effects/figure/sampling2.pdf", p, height = 2.5, width = 7.5)
