credit = data.frame(read.csv("code/datasets/credit.csv"))
source("code/sol_catale.R")

p <- order_levels(credit, "personal_status_sex")

capture.output(p, file = "run_order_levels.txt")