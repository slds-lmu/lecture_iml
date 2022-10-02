credit = data.frame(read.csv("exercises/feature-effects/rsrc/datasets/credit.csv"))
source("exercises/feature-effects/rsrc/get_diff_cat().R")

p <- get_diff_cat(feature.k = credit[,"employment_duration"], 
                  feature.j = credit[,"personal_status_sex"])

capture.output(p, file = "exercises/feature-effects/rsrc/run_get_diff_cat.txt")
