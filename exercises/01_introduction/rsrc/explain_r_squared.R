source("r_squared_main.R")

bike[ind[2],c("cnt")] = l$fitted.values[ind[2]]
b = bike[ind,c("temp", "cnt")]
dat = rbind(b, c(b[1,1]+0.6, b[1,2]))
dat[2,1] = dat[1,1]

p = ggplot(bike, aes(x=temp, y=cnt)) + geom_point() +
  geom_smooth(method = "lm", color = "darkgray", se = FALSE, size = 1.5) +
  geom_rect(data = dat, 
            aes(xmax = temp, ymax = cnt, xmin = temp,
                colour = SSE, fill = SSE),
            ymin = ym, linewidth = 2) +#,
  #colour = c("blue", "red")) +
  geom_point(data = dat, aes(colour = SSE), size = 1.5) +
  geom_line(aes(y = mean.cnt),  color = "darkgray", size = 1.5) +
  #stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, aes(label = ..rr.label..)) +
  annotate("label", x = c(36,36),
           y = c(ym[2], max(l$fitted.values)),
           label = ylab, parse = T, colour = "black", hjust = 0.9) +
  labs(x = expression(X~": Temperature in °C"), 
       y = expression(Y~": Number of bike rentals")) 
p

ggsave("../figure/r_squared.pdf", width=7, height=5.5)
