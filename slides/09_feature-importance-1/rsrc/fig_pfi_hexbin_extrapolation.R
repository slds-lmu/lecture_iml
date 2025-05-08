# PREREQ -----------------------------------------------------------------------
library("iml")
library("mlr3")
library("mlr3verse")
library("ggplot2")
library("iml")
library("dplyr")
library(patchwork)
theme_set(theme_bw())


# DATA -----------------------------------------------------------------------
# extrapolation

n = 1000
ntrain = 0.7 * n

x1 = rnorm(n)
x2 = x1 + rnorm(n, sd=0.01)
x3 = rnorm(n)
x4 = rnorm(n)
y = x3 + rnorm(n, sd=0.1)

data = data.frame(x1=x1, x2=x2, x3=x3, x4=x4, y=y)
task = TaskRegr$new(id='correlated', backend=data, target='y')
learner = lrn('regr.lm')

train_set = sample(task$nrow, ntrain)
test_set = setdiff(seq_len(task$nrow), train_set)

learner$train(task, row_ids = train_set)
learner$model

predictor_test = Predictor$new(learner, data[test_set,], y='y')

imp_test <- FeatureImp$new(predictor_test,loss = "mae", n.repetitions = 10, compare='difference')

p_pfi = plot(imp_test)
p_pfi

p2 = ggplot(data, aes(x=x1, y=x2)) + geom_hex(bins = 20) + scale_fill_viridis_c() #+ theme_bw()
p2


data_perm = data.frame(data)
data_perm$x1 = data_perm$x1[sample(nrow(data_perm))]

p3 = ggplot(data_perm, aes(x=x1, y=x2)) +
  geom_hex(bins = 20) + scale_fill_viridis_c() #+ theme_bw()
p3

# PLOT -----------------------------------------------------------------------
res = p2 + p3 + p_pfi & theme(aspect.ratio=1)#, legend.position = "bottom") #+ plot_layout(guides = 'collect')

ggsave(plot = res, "../figure_man/pfi_hexbin_extrapolation.pdf", width=10, height=3)
