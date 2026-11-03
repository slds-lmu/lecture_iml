source("r_squared_main.R")

p = ggplot(bike, aes(x=temp_F, y=cnt)) + geom_point() +
  geom_smooth(method = "lm", colour = "blue", se = FALSE) +
  #stat_cor(aes(label = ..rr.label..)) +
  labs(x = expression(X[1]~": Temperature in °F"), y = expression(X[2]~": Number of bike rentals")) +
  guides(colour = FALSE) + xlim(-10, 90)
p

ggsave("../figure/r_squared_Fahrenheit.pdf", width=7, height=5.5)