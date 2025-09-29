# Prerequisits ------------------------------------------------------------------------
from sklearn.ensemble import RandomForestClassifier
import numpy as np
import shap
import pyreadr
import matplotlib.pyplot as plt

# Data -----------------------------------------------------------------------------
bike_dict = pyreadr.read_r('../../../data/bike.RData')
X, y = bike_dict['bike'][['temp', 'windspeed', 'hum']], bike_dict['bike']['cnt']
clf = RandomForestClassifier(max_depth=10, random_state=0)
clf.fit(X, y)

# SHAP -----------------------------------------------------------------------------
explainer = shap.KernelExplainer(clf.predict, X)
shap_values = explainer.shap_values(X.iloc[414,:])
#shap.force_plot(explainer.expected_value, shap_values, X.iloc[414,:], matplotlib=True)
shap.force_plot(4515.048, np.array([-1653.68534, -322.72443, 34.03142]), np.round(X.iloc[414,:], 2), matplotlib=True, show=False)
plt.savefig('../figure_man/exSHAP.png')
plt.close()


