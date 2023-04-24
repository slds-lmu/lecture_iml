# PREREQ -----------------------------------------------------------------------
library(xtable)
# DATA -----------------------------------------------------------------------
set.seed(1234L)
x = data.frame(sex = "m", age = 50, salary = 1300, savings = 500, credit_amount = 5000, credit = "no")

# PLOT -------------------------------------------------------------------------
print(xtable::xtable(x = x, digits = 0), include.rownames = FALSE,
      file="exercises/local-explanations/rsrc/table_customer_inquiry.txt")
