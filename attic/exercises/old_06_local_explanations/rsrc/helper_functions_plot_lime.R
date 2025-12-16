# PREREQ -----------------------------------------------------------------------
source("exercises/local-explanations/rsrc/helper_functions_lime.R")
source("exercises/local-explanations/rsrc/data_lime.R")

# plot -----------------------------------------------------------------------
print("Run 'get_grid' ...")
grid = get_grid(model = mod, dataset = dataset, 
                points_per_feature = points_per_feature)

print("Run `plot_grid` ...")
plot = plot_grid(grid)
plot

ggsave('exercises/local-explanations/figure/helper_functions_plot_lime.pdf',width=3.5,height=2.5)