# =============================================================================
# Visualize the SHAP kernel by feature and coalition size
# =============================================================================
library("ggplot2")

calc_kernel = function(coal_size, M = 20) {
  (M - 1) / (choose(M, coal_size) * coal_size * (M - coal_size))
}
kernel10_df = data.frame(coalition_size = 0:10, kernel_weight = sapply(0:10, calc_kernel, M = 10), 
                         z = c(Inf, rep(NA, times =9), Inf))

ggplot(kernel10_df) +
  geom_line(aes(x = coalition_size, y = kernel_weight)) +
  scale_x_continuous("coalition size", breaks = 0:(nrow(kernel10_df)-1)) +
  scale_y_continuous("kernel weight") +
  coord_cartesian(clip = "off") +
  theme_bw() +
  geom_label(aes(x = coalition_size, y = kernel_weight,label=z), fill = "black", colour = "white")
  
ggsave(file = "../figure_man/kernel-weights.pdf", height = 3, width = 4)


