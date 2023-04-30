source("r_squared_main.R")

bike[ind[2],c("cnt")] = l$fitted.values[ind[2]]
b = bike[ind,c("temp", "cnt")]
dat = rbind(b, c(b[1,1]+0.6, b[1,2]))
dat[2,1] = dat[1,1]

ggplot(bike, aes(x=temp, y=cnt)) + geom_point() +
  geom_smooth(method = "lm", aes(colour = SSE[1]), se = FALSE) +
  geom_rect(data = dat, 
            aes(xmax = temp, ymax = cnt, xmin = temp,
                colour = SSE, fill = SSE),
            ymin = ym, linewidth = 2) +#,
  #colour = c("blue", "red")) +
  geom_point(data = dat, aes(colour = SSE), size = 1.5) +
  geom_line(aes(y = mean.cnt, colour = SSE[2]), size = 1.5) +
  #stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, aes(label = ..rr.label..)) +
  #annotate("label", x = dat$temp,
  #         y = (dat$cnt + mean.cnt)/2,
  #         label = ylab, parse = T, colour = "black", hjust = 0.9) +
  labs(x = expression(X[1]~": Temperature in °C"), y = expression(X[2]~": Number of bike rentals")) + 
  guides(col = FALSE) 
p

ggsave("../figure/r_squared.pdf", width=7, height=5.5)