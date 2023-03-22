library(ggplot2)

t = 2.922
p = round(1 - pt(t, df = 9), 4)

x = seq(-4, 4, length.out = 100)
y = dt(x, df = 9)
plot.data = data.frame(
  x = x,
  y = y)

x.poly = seq(t, 4, length.out = 100)
poly.data = data.frame(
  x = c(x.poly, min(x.poly)),
  y = c(dt(x.poly, df = 9), 0)
)

pv = ggplot() +
  geom_polygon(data = poly.data, aes(x, y), color = NA, fill = "blue", alpha = 0.5) +
  geom_polygon(data = poly.data, aes(-x, y), color = NA, fill = "blue", alpha = 0.5) +
  geom_line(data = plot.data, aes(x, y)) + 
  geom_segment(aes(x = t, xend = t, y = 0, yend = 0.3), size = 1.2, color = "red") +
  geom_text(aes(x = t, y = 0.35, label = paste0("t == ", t)), parse = TRUE, color = "red") + 
  geom_segment(aes(x = -t, xend = -t, y = 0, yend = 0.1), size = 1.2, color = "red") +
  # geom_text(aes(x = -t, y = 0.35, label = "t[0] == -2.92"), parse = TRUE, color = "red") + 
  geom_text(aes(x = 3.3, y = 0.15, label = paste0("p/2 = ", p)), color = "dark blue") + 
  geom_text(aes(x = -3.3, y = 0.15, label = paste0("p/2 = ", p)), color = "dark blue") +
  geom_text(aes(x = 0, y = 0.1, label = paste0("phantom(0) %=>% p ==", 2*p)), color = "dark blue", parse = TRUE) +
  ggtitle("two-sided") +
  xlab("t") + ylab("Density of T")

ggsave("../figure/p-value.pdf", pv, width = 6, height = 3.5)
