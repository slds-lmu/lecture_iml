# PREREQ -----------------------------------------------------------------------
source("lime_data.R")

# PLOT -----------------------------------------------------------------------
fig0 <- ggplot(data, aes(x = x, y = y)) + 
  # geom_smooth(se = FALSE, col = "black") + 
  geom_ribbon(aes(ymin = 0,ymax = predict(loess(y ~ x))),
              alpha = 0.6,fill = 'lightgray') + 
  geom_ribbon(aes(ymin = predict(loess(y ~ x)), ymax = 8),
              alpha = 0.9,fill = 'darkgray') + 
  theme_bw() + ylim(c(0, 8)) +
  ylab("x2") + xlab("x1") 
fig0
ggsave("../figure/lime1.pdf", plot = fig0, width = 4, height = 3)