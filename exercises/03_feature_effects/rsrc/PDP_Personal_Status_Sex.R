# PREREQ -----------------------------------------------------------------------
library(ranger)
library(iml)
library(ggpubr)
library(mlr3learners)
library(xtable)

# DATA -------------------------------------------------------------------------
task = tsk("german_credit")
task$select(cols = c("age", "amount", "duration", "credit_history", "employment_duration", "personal_status_sex", "purpose"))
set.seed(1234)
learn = lrn("classif.ranger")
learn$predict_type = "prob"
learn$train(task)
pred = Predictor$new(learn, data = task$data(), y = "credit_risk")

# PLOT -------------------------------------------------------------------------
pdf(file = "exercises/feature-effects/figure/PDP_Personal_Status_Sex.pdf",width=8,height=4) 
plot(iml::FeatureEffect$new(predictor = pred, feature = "personal_status_sex", method = "pdp")) + 
  theme(axis.text.x = element_text(angle = 50, hjust=1))

dev.off()
