import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from sklearn import linear_model
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.model_selection import train_test_split

import rfi.examples.chains as chains
from rfi.explainers.explainer import Explainer
from rfi.explanation.explanation import Explanation
from rfi.explanation.decomposition import DecompositionExplanation
from rfi.samplers.simple import SimpleSampler
from rfi.samplers.gaussian import GaussianSampler
from rfi.decorrelators.gaussian import NaiveGaussianDecorrelator
from rfi.decorrelators.naive import NaiveDecorrelator

import logging
import math

import statsmodels.api as sm
import statsmodels.formula.api as smf

logging.basicConfig(level=logging.INFO)

savepath = '~/university/phd/2021/teaching/lecture_iml/slides/feature-importance/pysrc/'
rpath = '~/university/phd/2021/teaching/lecture_iml/slides/feature-importance/rsrc/'

# Example 1

reg_lin = linear_model.LinearRegression()

# datasets to use
n_train, n_test = 10 ** 6 * 4, 10 ** 3

simulations = [chains.chain3]

simulation_id = 0

ex_name = simulations[simulation_id].name  #+ ex_identifier
xcolumns = simulations[simulation_id].sem.dag.var_names[:-1]
ycolumn = [simulations[simulation_id].sem.dag.var_names[-1]]
data = simulations[simulation_id].get_train_test_data(xcolumns, ycolumn,
                                                      n_train=n_train,
                                                      n_test=n_test,
                                                      as_dataframes=True)
df_train, df_test = data
X_train, y_train = df_train[xcolumns], df_train[ycolumn]
X_test, y_test = df_test[xcolumns], df_test[ycolumn]

# fit models

reg_lin.fit(X_train, y_train)
reg_lin.coef_[0, 1] = reg_lin.coef_[0, 1] + 0.05
reg_lin.coef_[0, 2] = reg_lin.coef_[0, 2] - 0.05

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

ex_sage = wrk.ais_via_contextfunc(fsoi, X_test, y_test, context='empty', marginalize=True)
ex_sage.hbarplot()
plt.show()

df_sage = ex_sage.fi_means_quantiles()
df_sage['type'] = 'conditional v(j)'

ex_sage2 = wrk.ais_via_contextfunc(fsoi, X_test, y_test, context='remainder', marginalize=True)
ex_sage2.hbarplot()
plt.show()

df_sage2 = ex_sage2.fi_means_quantiles()
df_sage2['type'] = 'conditional v(-j u j) - v(-j)'

ex_sage_m = wrk.dis_from_baselinefunc(fsoi, X_test, y_test, baseline='empty', marginalize=True)
ex_sage_m.hbarplot()
plt.show()

df_sage_m = ex_sage_m.fi_means_quantiles()
df_sage_m['type'] = 'marginal v(j)'

ex_sage_m2 = wrk.dis_from_baselinefunc(fsoi, X_test, y_test, baseline='remainder', marginalize=True)
ex_sage_m2.hbarplot()
plt.show()

df_sage_m2 = ex_sage_m2.fi_means_quantiles()
df_sage_m2['type'] = 'marginal v(-j u j) - v(-j)'


# saving overall result

df_res2 = pd.concat([df_sage, df_sage2, df_sage_m, df_sage_m2]).reset_index()
df_res2.to_csv(savepath+'df_res2.csv')



## INTERACTIONS EXAMPLE

df = pd.read_csv(rpath + 'df_interactions.csv')
df = df.drop(columns='Unnamed: 0')

xcolumns = df.columns[:-1]
ycolumns = [df.columns[-1]]

ntrain = math.floor(0.7*df.shape[0])

df_train, df_test = train_test_split(df, train_size=ntrain)
X_train, y_train = df_train[xcolumns], df_train[ycolumns]
X_test, y_test = df_test[xcolumns], df_test[ycolumns]
reg_lin = smf.ols('y ~ x1*x2 + x3', data=df_train).fit()
print(reg_lin.summary())

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

sampler = SimpleSampler(X_train)
decorrelator = NaiveDecorrelator(X_train, sampler)
fsoi = X_train.columns
ordering = [tuple(fsoi)]

wrk = Explainer(reg_lin.predict, fsoi, X_train,
                loss=mean_squared_error, sampler=sampler,
                decorrelator=decorrelator)

ex_pfi = wrk.dis_from_baselinefunc(fsoi, X_test, y_test, baseline='remainder', marginalize=False)
ex_pfi.ex_name = 'pfi'
ex_pfi.to_csv(savepath=savepath, filename='ex_pfi.csv')
ex_pfi = Explanation.from_csv(savepath + 'ex_pfi.csv')

ex_pfi.hbarplot()
plt.show()

df_pfi = ex_pfi.fi_means_quantiles()
df_pfi['type'] = 'pfi'


ex_msage, orderings = wrk.sage(X_test, y_test, ordering)
ex_msage.ex_name = 'msage'
ex_msage.to_csv(savepath=savepath, filename='ex_msage.csv')
ex_msage = Explanation.from_csv(savepath+'ex_msage.csv')

ex_msage.hbarplot()
plt.show()

df_msage = ex_msage.fi_means_quantiles()
df_msage['type'] = 'mSAGE'

df_interactions_res = pd.concat([df_pfi, df_msage]).reset_index()
df_interactions_res.to_csv(savepath+'df_interactions_res.csv')

print(model.coef_, model.intercept_)