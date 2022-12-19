source("r_squared_main.R")

p = ggplot(bike, aes(x=temp, y=cnt)) + geom_point() +
  #geom_point(aes(x = temp_F), colour = "gray") +
  geom_smooth(method = "lm", colour = "#00BFC4", se = FALSE) +
  #geom_smooth(method = "lm", aes(x = temp_F), colour = "blue", se = FALSE) +
  #stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, aes(label = ..rr.label..)) +
  labs(x = expression(X[1]~": Temperature in °C"), y = expression(X[2]~": Number of bike rentals"))+ 
  xlim(-10, 90)
p

ggsave("../figure/r_squared_compare.pdf", width=7, height=5.5)