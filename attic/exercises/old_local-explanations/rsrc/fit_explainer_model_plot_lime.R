# PREREQ -----------------------------------------------------------------------
source("exercises/local-explanations/rsrc/weight_points_plot_lime.R")

# plot -----------------------------------------------------------------------
print("Run `fit_explainer_model` ...")
explainer = fit_explainer_model(df = samp, weights = w)

print("Compare models ...")
plt1 = plot_points_in_grid(plt = plot, df = samp,
                           x_interest = x_interest, size = .5)
grid2 = get_grid(model = explainer, dataset = dataset,
                 points_per_feature = points_per_feature)
plot2 = plot_grid(grid2)
plt2 = plot_points_in_grid(plt = plot2, df = samp, 
                           x_interest = x_interest, size = .5)

plt1 = plt1 + ggplot2::ggtitle("SVM")
plt2 = plt2 + ggplot2::ggtitle("Decision Tree Explainer")

pdf(file="exercises/local-explanations/figure/fit_explainer_model_plot_lime.pdf",height=2.75,width=7)

plot = grid.arrange(plt1, plt2, ncol = 2L)
plot

dev.off()
