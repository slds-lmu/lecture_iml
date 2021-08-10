import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from sklearn import linear_model
from sklearn.metrics import mean_squared_error, r2_score

import rfi.examples.chains as chains
from rfi.explainers.explainer import Explainer
from rfi.samplers.gaussian import GaussianSampler
from rfi.decorrelators.gaussian import NaiveGaussianDecorrelator

import logging

logging.basicConfig(level=logging.INFO)

reg_lin = linear_model.LinearRegression()
savepath = '~/university/phd/2021/teaching/lecture_iml/slides/feature-importance/pysrc/'

# datasets to use
data = pd.read_csv(savepath + 'extrapolation.csv')

data = data[['x1', 'x2', 'x3', 'x4', 'y']]
ntrain = int(0.7 * data.shape[0])

xcolumns = ['x1', 'x2', 'x3', 'x4']
ycolumn = ['y']
df_train, df_test = data.iloc[0:ntrain,], data.iloc[ntrain:,]
X_train, y_train = df_train[xcolumns], df_train[ycolumn]
X_test, y_test = df_test[xcolumns], df_test[ycolumn]

# fit models

reg_lin.fit(X_train, y_train)
reg_lin.coef_[0, 0] = 0.3
reg_lin.coef_[0, 1] = -0.3

scoring = [mean_squared_error, r2_score]
names = ['MSE', 'r2_score']
models = [reg_lin]
m_names = ['LinearRegression']

for kk in range(len(models)):
    model = models[kk]
    print('Model: {}'.format(m_names[kk]))
    for jj in np.arange(len(names)):
        print('{}: {}'.format(names[jj],
                              scoring[jj](y_test, model.predict(X_test))))

# explain model

sampler = GaussianSampler(X_train)
decorrelator = NaiveGaussianDecorrelator(X_train)
fsoi = X_train.columns
ordering = [tuple(fsoi)]

wrk = Explainer(reg_lin.predict, fsoi, X_train,
                loss=mean_squared_error, sampler=sampler,
                decorrelator=decorrelator)


ex_cfi = wrk.ais_via_contextfunc(fsoi, X_test, y_test, context='remainder', marginalize=False)
ex_cfi.hbarplot()
plt.show()

df_cfi = ex_cfi.fi_means_quantiles()
df_cfi['type'] = 'cfi'

ex_pfi = wrk.dis_from_baselinefunc(fsoi, X_test, y_test, baseline='remainder', marginalize=False)
ex_pfi.hbarplot()
plt.show()

df_pfi = ex_pfi.fi_means_quantiles()
df_pfi['type'] = 'pfi'

df_res = pd.concat([df_pfi, df_cfi]).reset_index()
df_res.to_csv(savepath+'df_res.csv')

