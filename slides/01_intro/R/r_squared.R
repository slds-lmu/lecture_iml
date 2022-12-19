source("r_squared_main.R")

p = ggplot(bike, aes(x=temp, y=cnt)) + geom_point() +
  geom_smooth(method = "lm", aes(colour = Model[1]), se = FALSE) +
  geom_rect(data = bike[ind,], 
            aes(xmax = temp, ymax = cnt, xmin = temp,
                colour = Model, fill = Model),
            ymin = ym, linewidth = 2) +#,
  #colour = c("blue", "red")) +
  geom_point(data = bike[ind,], aes(colour = Model), size = 6) +
  geom_line(aes(y = mean.cnt, colour = Model[2]), size = 1.5) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, aes(label = ..rr.label..)) +
  annotate("label", x = bike$temp[ind],
           y = (bike$cnt[ind] + mean.cnt)/2,
           label = ylab, parse = T, colour = "black", hjust = 0.9) +
  labs(x = expression(X[1]~": Temperature in °C"), y = expression(X[2]~": Number of bike rentals")) +
  guides(colour = FALSE) 
p

ggsave("../figure/r_squared.pdf", width=7, height=5.5)