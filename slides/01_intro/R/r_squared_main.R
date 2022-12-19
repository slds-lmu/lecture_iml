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