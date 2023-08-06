# PREREQ -----------------------------------------------------------------------
source("exercises/local-explanations/rsrc/helper_functions_whatif.R")
source("exercises/local-explanations/rsrc/data_whatif.R")

# plot -----------------------------------------------------------------------
# Compute counterfactual for first observation
x_interest = df[1,]
x_interest
print(xtable::xtable(x_interest),
      file="exercises/local-explanations/rsrc/x_interest.txt")

cf = generate_whatif(x_interest = x_interest, model = mod, dataset = df)
print(xtable::xtable(cf),
      file="exercises/local-explanations/rsrc/cf.txt")


evaluate_counterfactual(counterfactual = cf, x_interest = x_interest, model = mod)