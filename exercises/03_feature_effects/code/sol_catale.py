import pandas as pd
import numpy as np
from sklearn.model_selection import ParameterGrid
import statsmodels.api as sm 
from sklearn import manifold


def order_levels(data, feature_name):
  """
  Orders levels of a nominal scaled feature according to other features in a dataset.
  
  Parameters:
    
    data (np.array with shape (num_instances, num_features)): Input data.
    feature_name (char): name of categorical feature which shoul be ordered
   
  Returns: np.array (char) of ordered class labels
  """
  feature = data[feature_name].values
  others = list(data.columns)
  others.remove(feature_name)
  feature_lev = np.unique(feature)
  
  dists = []
  
  # Iterate over other features, sum up distances
  for i in range(len(others)): 
    k = others[i]
    feature_k = data[k].values
    if (feature_k.dtype.kind !='i') & (feature_k.dtype.kind != 'f'):
     ds = get_diff_cat(feature_k, feature)
    else: 
     ds = get_diff_numeric(feature_k, feature)
    
    if i == 0: 
      dists = ds
    else: 
      dists['dist'] = dists['dist'] + ds['dist']
      
  # Create a matrix of distances
  dists = dists.pivot(index='class1', columns='class2', values='dist')
  mds = manifold.MDS(1)
  scaled = mds.fit_transform(dists)
  
  order = feature_lev[scaled.flatten().argsort()]
  
  return(order)

def ecdf(data):
    """ Compute ECDF """
    x = np.sort(data)
    n = x.size
    y = np.arange(1, n+1) / n
    return(x,y)

def get_diff_numeric(feature_k, feature_j):
  """
  Calculates the pairwise distances of classes of j of a numeric feature k 
  
  Parameters:
    
    feature_k (char): np.array with values of numeric feature for which relative frequencies per class are calculated 
    feature_j (char): np.array with values of categorical feature for which similarity based on feature k should be assessed. 
    
  Returns: pd.Dataframe with three columns: 
    class1: name of the first class
    class2: name of the second class 
    dist: the distributional distance between the two classes for feature k
  """
  
  # set up data.frame which we will fill later
  feature_lev = np.unique(feature_j)
  param_grid = {'class1': feature_lev, 'class2' : feature_lev}
  dists = pd.DataFrame(ParameterGrid(param_grid))
  
  # get decentiles: 
  quants = np.quantile(feature_k, q = np.linspace(0, 1, num = 10))
  
  # derive empirical distribution function for each category
  ecdfs = {}
  for i in range(len(feature_lev)):
    lev = feature_lev[i]
    ecdfsx = sm.distributions.ECDF(feature_k[feature_j == lev])(quants)
    ecdfs[lev] = ecdfsx
  
  # get pairwise absolute distances of empirical distribution function 
  # between the different categories
  ecdf_dists = []
  for i1, i2 in zip(dists.class1.values, dists.class2.values): 
    ecdf_dists.append(max(abs(ecdfs[i1] - ecdfs[i2])))

  dists["dist"] = ecdf_dists
  # get maximum distance over decentiles for each pair of categories
  return(dists)


def get_diff_cat(feature_k, feature_j):
  """
  Calculates the pairwise distances of classes of j of a categorical feature k 

  feature_k (char): np.array of values of categorical feature for which relative frequencies per class are calculated 
  feature_j (char): np.array of values of categorical feature for which similarity based on feature k should be assessed. 
  
  Returns:
    np.array with three columns: 
      * class1: name of the first class
      * class2: name of the second class 
      * dist: the distributional distance between the two classes for feature k
  """
  
  # set up data.frame which we will fill later
  feature_lev = np.unique(feature_j)
  param_grid = {'class1': feature_lev, 'class2' : feature_lev}
  dists = pd.DataFrame(ParameterGrid(param_grid))
  
  # get relative frequency table
  xcount = np.unique(feature_j, return_counts = True)[1]
  A = pd.crosstab(feature_k, feature_j)/xcount
  
  # compute pairwise absolute distances 
  ecdf_dists = []
  for i1, i2 in zip(dists.class1.values, dists.class2.values): 
    print(i1, i2)
    ecdf_dists.append(sum(abs(A[i1] - A[i2])))
  dists["dist"] = ecdf_dists
   
  return(dists)



if __name__ == "__main__":
  
  credit = pd.read_csv('datasets/credit.csv')
    
  ## This should already work WITHOUT get_diff_cat()
  credit_sub = credit[["age", "employment_duration", "personal_status_sex"]]
  order_levels(data = credit_sub, feature_name = "personal_status_sex")
  get_diff_numeric(feature_k = credit_sub["age"], feature_j = credit_sub["personal_status_sex"])
  # to see what the methods does step by step use debug(order_levels) or debug(get_diff_numeric)
  
  ## This should return non-0 dists AFTER you have implemented get_diff_cat()
  order_levels(credit, "personal_status_sex")
  get_diff_cat(feature_k = credit["employment_duration"], feature_j = credit["personal_status_sex"])

