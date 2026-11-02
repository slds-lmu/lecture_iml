library(rpart)
library(rpart.plot)
library(partykit)
library(ggparty)

#load("../../../data/bike.RData")

set.seed(1234)

# Data prep
source("bike_example_Data.R")


## ctree
c_tree = ctree(y~., data = dat, 
               control = ctree_control(maxdepth = 3, maxsurrogate = 0))
pdf("../figure/bike_ctree.pdf", width = 9, height = 4.5)
#plot(c_tree)
ggparty(c_tree,
  terminal_space = 0.3,
  add_vars = list(p.value = "$node$info$p.value")) +
  geom_edge(size = 1.5) +
  geom_edge_label(colour = "grey", size = 3, shift = 0.6) +
  geom_node_plot(gglist = list(geom_boxplot(aes(y = y), alpha = 0.5),
    scale_y_continuous(breaks = c(0, 4000, 8000)),
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
      aes(label = splitvar),
      aes(label = paste("p =", formatC(p.value, format = "e", digits = 2)))),
    line_gpar = list(list(size = 8, col = "black", fontface = "bold"),
      list(size = 10),
      list(size = 8)),
    ids = "inner") +
  geom_node_label(aes(label = paste0("Node ", id, ", N = ", nodesize)),
    fontface = "bold",
    ids = "terminal",
    size = 2, 
    nudge_y = 0.01) +
  theme(legend.position = "none")

dev.off()

# rmse
pred = predict(c_tree, newdata = dat)
rmse = mean((pred - dat$y)^2)

## mob
mob_tree = lmtree(y ~ temp | ., data = dat, maxdepth = 4)

pdf("../figure/bike_mob.pdf", width = 9, height = 4.5)
ggparty(mob_tree,
  terminal_space = 0.3,
  add_vars = list(p.value = "$node$info$p.value")) +
  geom_edge(size = 1.5) +
  geom_edge_label(colour = "grey", size = 3, shift = 0.6) +
  geom_node_plot(gglist = list(geom_point(aes(x = temp, y = y), alpha = 0.5),
    scale_y_continuous(breaks = c(0, 5000, 10000)),
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5))),
    scales = "fixed",
    id = "terminal",
    shared_axis_labels = T,
    shared_legend = T,
    #legend_separator = T,
    predict = "temp",
    predict_gpar = list(col = "blue", size = 1.2)
  ) +
  geom_node_label(aes(col = splitvar),
    line_list = list(aes(label = paste("Node", id)),
      aes(label = splitvar),
      aes(label = paste("p =", formatC(p.value, format = "e", digits = 2)))),
    line_gpar = list(list(size = 8, col = "black", fontface = "bold"),
      list(size = 10),
      list(size = 8)),
    ids = "inner") +
  geom_node_label(aes(label = paste0("Node ", id, ", N = ", nodesize)),
    fontface = "bold",
    ids = "terminal",
    size = 2, 
    nudge_y = 0.01) +
  theme(legend.position = "none")
dev.off()

# rmse
pred2 = predict(mob_tree, newdata = dat)
rmse2 = mean((pred2 - dat$y)^2)
