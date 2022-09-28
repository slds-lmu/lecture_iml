# DATA -----------------------------------------------------------------------
set.seed(1234L)
x = data.frame(sex = "m", age = 50, salary = 1300, savings = 500, credit_amount = 5000, credit = "no")
print(xtable::xtable(x = x, digits = 0), include.rownames = FALSE)

cf = data.frame(sex = c("f", "f", "m", "m", "m", "m", "m", "m", "f"), 
                age = c(50, 45, 50, 51, 52, 50, 49, 51, 50), 
                salary = c(1300, 1300, 1300, 1300, 1400, 1400, 1390, 1800, 1301),
                savings = c(500, 500, 500, 500, 500, 250, 700, 500, 570), 
                credit_amount = c(5000, 500, 2600, 4800, 5000, 5000, 5000, 5100, 5019),
                credit = rep("yes", 9))
# PLOT -------------------------------------------------------------------------
xtable::xtable(x = cf, digits = 0)