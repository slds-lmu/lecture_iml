# PREREQ -----------------------------------------------------------------------
library(mgcv)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(psycModel)
library(xtable)
library(pagedown)
# DATA -------------------------------------------------------------------------
set.seed(1234L)
x1 = round(seq(-1, 1, 0.2), 2)
x2 = round(x1^2+rnorm(length(x1), sd = 0.04), 2)
y = round(5*x1 + -2*x2 + rnorm(length(x1)), 2)
d = data.frame(y, x1, x2)

# tables
dat = d
dat[length(x1)+1,] = colSums(dat)
xtable(t(dat), )

mod <- gam(x2 ~ s(x1), data = d) 
mod2 <- lm(x2 ~ x1, data = d)

tab_model(mod2, mod, transform = NULL, auto.label = FALSE,  
          p.style = "scientific", dv.labels = c("LM", "GAM"), 
          file = "../figure/GAM_LM_Output.html")
html_to_pdf(file_path = "../figure/GAM_LM_Output.html")


pdf("../figure/gam.pdf", width = 6, height = 5)
par(mar = c(4,4,1,1))
plot(mod, pages=1, shade = TRUE)
dev.off()