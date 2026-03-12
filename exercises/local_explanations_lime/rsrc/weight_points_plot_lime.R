# PREREQ -----------------------------------------------------------------------
source("exercises/local_explanations_lime/rsrc/sample_points_plot_lime.R")

# plot -----------------------------------------------------------------------
print("Run `weight_points` ...")
w = weight_points(x_interest = x_interest, df = samp, kernel_width = 0.2)

print("Run `plot_points_in_grid` ...")
plot = plot_points_in_grid(plt = plot, df = samp, weights = w, 
                    x_interest = x_interest)
plot

ggsave('exercises/local_explanations_lime/figure/weight_points_plot_lime.pdf',width=3.5,height=2.5)
