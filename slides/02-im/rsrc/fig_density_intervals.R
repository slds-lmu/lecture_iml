library(ggplot2)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

set.seed(123)

load("../../../data/bike.RData")

head(bike)
dat <- data.frame(x=bike$temp, y=bike$cnt)

## breaks: where you want to compute densities
breaks <- seq(-6, max(dat$x), len=5)
dat$section <- cut(dat$x, breaks)

## Get the residuals
dat$res <- residuals(lm(y ~ x, data=dat))

## Compute densities for each section, and flip the axes, and add means of sections
## Note: the densities need to be scaled in relation to the section size (2000 here)
dens <- do.call(rbind, lapply(split(dat, dat$section), function(x) {
  d <- density(x$res, n=50)
  res <- data.frame(x=max(x$x)- d$y*14000, y=d$x+mean(x$y))
  res <- res[order(res$y), ]
  ## Get some data for normal lines as well
  xs <- seq(min(x$res), max(x$res), len=50)
  res <- rbind(res, data.frame(y=xs + mean(x$y),
                               x=max(x$x) - 14000*dnorm(xs, 0, sd(x$res))))
  res$type <- rep(c("empirical", "normal"), each=50)
  res
}))

dens$section <- rep(levels(dat$section), each=100)

## Plot both empirical and theoretical
p = ggplot(dat, aes(x, y)) +
  geom_point(lwd = 1) +
  geom_smooth(method="lm", fill=NA, lwd=1.5) +
  geom_path(data=dens, aes(x, y, group=interaction(section,type), color=type), lwd=1.1) +
  geom_vline(xintercept=breaks, lty=2) +
  labs(x = "Temperature in °C", y = "Number of bike rentals per day")

p
ggsave("../figure/density_intervals.pdf", p,
       width = 6, height = 3)
