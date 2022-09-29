credit = data.frame(read.csv("code/datasets/credit.csv"))
source("get_diff_cat().R")

p <- get_diff_cat(feature.k = credit[,"employment_duration"], 
                  feature.j = credit[,"personal_status_sex"])

capture.output(p, file = "run_get_diff_cat.txt")
