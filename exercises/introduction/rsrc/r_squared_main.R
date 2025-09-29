# PREREQ -----------------------------------------------------------------------
library(patchwork)
library(ggplot2)
library(ggpubr)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# DATA -------------------------------------------------------------------------

negcol = "#F8766D"
  poscol = "#00BFC4"
  xcol = "#587246"
    set.seed(123)
    load("../../../data/bike.RData")
    mean.temp = mean(bike$temp)
    mean.cnt = mean(bike$cnt)
    # Skaleninvarianz
    bike$temp_F = bike$temp * 1.8 + 32
    mean.temp_F = mean(bike$temp_F)
    
    ind = c(601,169)
    SSE <- c("SSE[LM]", "SSE[LM-c]", "SSE[c]")
    
    xlab = ylab = c("c == bar(y)", "hat(f)[LM] == hat(y)")
    
    l = lm(cnt~temp, data = bike)
    ym = l$fitted.values[ind]
    ym[2] <- mean.cnt
    ym = c(ym, ym[2])
    
    l1 = lm(cnt~temp_F, data = bike)
    