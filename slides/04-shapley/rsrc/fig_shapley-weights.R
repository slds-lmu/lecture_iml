# =============================================================================
# Visualize the Shapley weights by coalition size
# =============================================================================
library("ggplot2")
theme_set(theme_bw())
# Number of players / features
p = 5

#' Compute Shapley weight based on coalition size s
coal_size = function(s, p){
  # weight in Shapley formula (numerator)
  num = factorial(s) * factorial(p - s - 1)
  # weight in Shapley formula (denominator)
  den = factorial(p)
  weight = num / den

  weight
}

plt_data = data.frame(s = 0:(p - 1))
plt_data$weight = sapply(plt_data$s, function(x) coal_size(x, p))

ggplot(plt_data) +
  geom_line(aes(x = s, y = weight)) +
  scale_x_continuous("Coalition size |S|", breaks = 0:p) +
  scale_y_continuous("Shapley weight: |S|!(|P| - |S| - 1)/|P|!")
ggsave(file = "../figure/shapley-weights.pdf", height = 3, width = 4)

