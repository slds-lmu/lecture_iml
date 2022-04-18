# Prerequisits ------------------------------------------------------------------------
import math
import sklearn
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
import numpy as np
import shap
import time
import pyreadr
import matplotlib.pyplot as plt
# Data -----------------------------------------------------------------------------
def print_accuracy(f):
    print("RMSE = {0}".format(math.sqrt(np.sum((f(X_test) - y_test)^2)/len(y_test))))


bike_dict = pyreadr.read_r('slides/shapley/rsrc/bike.RData')
X, y = bike_dict['bike'][['temp', 'windspeed', 'hum', 'yr']], bike_dict['bike']['cnt']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)
shap.initjs()
clf = RandomForestClassifier(max_depth=10, random_state=0)
clf.fit(X_train, y_train)
# print_accuracy(clf.predict)

# SHAP -----------------------------------------------------------------------------

explainer = shap.KernelExplainer(clf.predict, X_train)
shap_values = explainer.shap_values(X_test.iloc[0,:])
shap.force_plot(explainer.expected_value, shap_values, X_test.iloc[0,:], matplotlib=True)
plt.savefig('exSHAP.png')
