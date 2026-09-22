# PREREQ -----------------------------------------------------------------------
source("exercises/local_explanations_lime/rsrc/helper_functions_plot_lime.R")

# plot -----------------------------------------------------------------------
print("Run `sample_points` ...")
samp = sample_points(model = mod, dataset = dataset, num_points = n_points)

print("Run `plot_points_in_grid` ...")
plot = plot_points_in_grid(plt = plot, df = samp, size = .5)
plot

ggsave('exercises/local_explanations_lime/figure/sample_points_plot_lime.pdf',width=3.5,height=2.5)
