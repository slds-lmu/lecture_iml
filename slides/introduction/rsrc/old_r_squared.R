# PREREQ -----------------------------------------------------------------------
library(patchwork)
library(ggplot2)
library(ggpubr)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# DATA -------------------------------------------------------------------------

negcol = "#F8766D"
poscol = "#00BFC4"
set.seed(123)
load("../../../data/bike.RData")
mean.temp = mean(bike$temp)
mean.cnt = mean(bike$cnt)
# Skaleninvarianz
bike$temp_F = bike$temp * 1.8 + 32
mean.temp_F = mean(bike$temp_F)

ind = c(653,251)
Model <- c("LM", "const.")

xlab = ylab = c("x[2]^{(i)} - hat(f)[LM]", "x[2]^{(j)} - bar(x)[2]")

l = lm(cnt~temp, data = bike)
ym = l$fitted.values[ind]
ym[2] <- mean.cnt

l1 = lm(cnt~temp_F, data = bike)

p = ggplot(bike, aes(x=temp_F, y=cnt)) + geom_point() +
  geom_smooth(method = "lm", colour = "blue", se = FALSE) +
  #stat_cor(aes(label = ..rr.label..)) +
  labs(x = expression(X[1]~": Temperature in °F"), y = expression(X[2]~": Number of bike rentals")) +
  guides(colour = FALSE) + xlim(-10, 90)
p

ggsave("../figure/r_squared_Fahrenheit.pdf", width=7, height=5.5)

######


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

######


p = ggplot(bike, aes(x=temp, y=cnt)) + geom_point() +
  #geom_point(aes(x = temp_F), colour = "gray") +
  geom_smooth(method = "lm", colour = "#00BFC4", se = FALSE) +
  #geom_smooth(method = "lm", aes(x = temp_F), colour = "blue", se = FALSE) +
  #stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, aes(label = ..rr.label..)) +
  labs(x = expression(X[1]~": Temperature in °C"), y = expression(X[2]~": Number of bike rentals"))+ 
  xlim(-10, 90)
p

ggsave("../figure/r_squared_compare.pdf", width=7, height=5.5)
