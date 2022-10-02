credit = data.frame(read.csv("exercises/feature-effects/rsrc/datasets/credit.csv"))
source("exercises/feature-effects/rsrc/sol_catale.R")

p <- order_levels(credit, "personal_status_sex")

capture.output(p, file = "exercises/feature-effects/rsrc/run_order_levels.txt")