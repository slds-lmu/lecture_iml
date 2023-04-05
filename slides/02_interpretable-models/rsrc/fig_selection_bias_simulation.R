library(rpart)
library(rpart.plot)
library(partykit)

set.seed(1234)

n = 200
y = rnorm(n, 0, 1)
x3 = apply(rmultinom(n, 1,  rep(0.125, 8)), 2, which.max)
x = data.frame(x1 = round(rnorm(n, 0, 1), 3),
  x2 = as.factor(rbinom(n, 1, prob = c(0.5,0.5))),
  x3 = as.factor(x3))

## normal tree
tree = rpart(y~x1+x2+x3, data = x)
pdf("../figure/selection_bias_simulation_tree.pdf", width = 11, height = 6)
#plot(as.party(tree))
ggparty(as.party(tree),
  terminal_space = 0.3) +
  geom_edge(size = 1.5) +
  geom_edge_label(colour = "black", size = 3, shift = 0.5) +
  geom_node_plot(gglist = list(geom_boxplot(aes(y = y), alpha = 0.5),
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.text.y = element_text(angle = 90, hjust = 0.5))),
    scales = "fixed",
    id = "terminal",
    shared_axis_labels = T,
    shared_legend = T#,
    #legend_separator = T#,
    #predict = "temp",
    #predict_gpar = list(col = "blue", size = 1.2)
  ) +
  geom_node_label(aes(col = splitvar),
    line_list = list(aes(label = paste("Node", id)),
      aes(label = splitvar)),
    line_gpar = list(list(size = 8, col = "black", fontface = "bold"),
      list(size = 10)),
    ids = "inner") +
  geom_node_label(aes(label = paste0("Node ", id, ", N = ", nodesize)),
    fontface = "bold",
    ids = "terminal",
    size = 2.5, 
    nudge_y = 0.01) +
  theme(legend.position = "none")
dev.off()

## ctree
c_tree = ctree(y~x1+x2+x3, data = x)
pdf("../figure/selection_bias_simulation_ctree.pdf", width = 3, height = 2)
par(mar = c(0,0,0,0))
plot(c_tree)
dev.off()
