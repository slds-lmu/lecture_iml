library(ggplot2)

theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

t = 1.96
df = 731
p = round(1 - pt(t, df = df), 3)

x = seq(-4, 4, length.out = 100)
y = dt(x, df = df)
plot.data = data.frame(
  x = x,
  y = y)

x.poly = seq(t, 4, length.out = 100)
poly.data = data.frame(
  x = c(x.poly, min(x.poly)),
  y = c(dt(x.poly, df = df), 0)
)

pv = ggplot() +
  geom_polygon(data = poly.data, aes(x, y), color = NA, fill = "blue", alpha = 0.5) +
  geom_polygon(data = poly.data, aes(-x, y), color = NA, fill = "blue", alpha = 0.5) +
  geom_line(data = plot.data, aes(x, y)) + 
  geom_segment(aes(x = t, xend = t, y = 0, yend = 0.3), size = 1.2, color = "red") +
  geom_text(aes(x = t, y = 0.35, label = paste0("t == ", t)), parse = TRUE, color = "red") + 
  geom_segment(aes(x = -t, xend = -t, y = 0, yend = 0.3), size = 1.2, color = "red") +
  geom_text(aes(x = -t, y = 0.35, label = paste0("t == ", -t)), parse = TRUE, color = "red") +
  # geom_text(aes(x = -t, y = 0.35, label = "t[0] == -2.92"), parse = TRUE, color = "red") + 
  geom_text(aes(x = t, y = 0.15, label = paste0("p/2 = ", p)), color = "dark blue", angle=90, vjust = 1.5) + 
  geom_text(aes(x = -t, y = 0.15, label = paste0("p/2 = ", p)), color = "dark blue", angle=90, vjust = -0.5) +
  geom_text(aes(x = 0, y = 0.1, label = paste0("phantom(0) %=>% p ==", 2*p)), color = "dark blue", parse = TRUE) +
  ggtitle(expression(H[0]:~theta[j]~"="~0~"vs."~H[1]:~theta[j]!=0~"(two-sided)")) +
  xlab("t") + ylab("Density of t-stat.") +  theme(plot.title = 
      element_text(hjust = 0.5))

ggsave("../figure/p-value.pdf", pv, width = 3.75, height = 2.5)
