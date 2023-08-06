# Prerequisits ------------------------------------------------------------------------
import xgboost
import numpy as np
import shap
import pyreadr
import matplotlib.pyplot as plt
import pandas as pd



bike_dict = pyreadr.read_r('../../../data/bike.RData')
X, y = bike_dict['bike'].drop(columns = 'cnt'), bike_dict['bike']['cnt']
cols = ['season', 'holiday', "mnth", 'weekday', 'workingday', 'weathersit']
X[cols] = X[cols].astype('category')
X["yr"] = pd.to_numeric(X["yr"], errors="coerce")

# train a model with single tree
Xd = xgboost.DMatrix(X, label=y, enable_categorical=True)
model = xgboost.train({
    'eta':1, 'max_depth':10, 'base_score': 0, "lambda": 0
}, Xd, 1)
print("Model error =", np.linalg.norm(y-model.predict(Xd)))

pred = model.predict(Xd, output_margin=True)
explainer = shap.TreeExplainer(model)
shap_values = explainer.shap_values(Xd)
np.abs(shap_values.sum(1) + explainer.expected_value - pred).max()


# requires  matplotlib version == 3.4.0
inds = shap.approximate_interactions("temp", shap_values, X)

shap.dependence_plot("temp", shap_values, X, interaction_index=None, show=False)
plt.savefig('../figure_man/global_shap_depend.pdf', bbox_inches='tight')
plt.close()
