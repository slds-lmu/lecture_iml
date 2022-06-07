import gower
import numpy as np

def generate_whatif(x_interest, model, dataset) : 
  
  """
  Computes whatif counterfactuals for binary classification models, 
  i.e., the closest data point with a different prediction.
  Parameter: 
    x_interest (np.array with shape (1, num_features)): Datapoint of interest.
    model: Binary classifier which has a predict method.
    dataset (np.array with shape (?, num_features)): Input data
    from which a counterfactual is selected from.

  Returns:
    counterfactual (np.array with shape (1, num_features)): the closest 
    observation/row to x_interest of the input dataset with a different prediction 
    than x_interest. 
  """

  return None


def evaluate_counterfactual(counterfactual, x_interest, model) :
  """
   Evaluates if counterfactuals are minimal, i.e., if setting one feature to 
   the value of x_interest still results in a different prediction than for x_interest.
   
   Parameter: 
   counterfactual (np.array with shape (1, num_features)): Counterfactual of `x_interest`. 
   x_interest (np.array with shape (1, num_features)): Datapoint of interest. 
   model: Binary classifier which has a predict method.
  
   Returns: 
   List with indices of features that if set for the counterfactual to the value of 
   `x_interest`, still leads to a different prediction than for x_interest. 
  """
  
  return None


if __name__ == "__main__":
  from sklearn import ensemble
  from utils.dataset import Dataset
  
  dataset = Dataset("wheat_seeds", range(0, 7), [7], normalize=True, categorical=True)
    
  # Create a binary classification task
  y = dataset.y
  y[y == 0] = 1
  
  # Reserve first row of dataset for x_interest
  X = dataset.X
  x_interest = X[0,:].reshape(1, -1)
  X = np.delete(X, (0), axis = 0)
  y = np.delete(y, (0), axis = 0)
  
  # Fit a random forest to the data
  model = ensemble.RandomForestClassifier(random_state=0)
  model.fit(X, y)
  
  # Define x_interest, remove it from dataset 
  model.predict(x_interest)
  
  # Compute counterfactual for first observation
  cf = generate_whatif(x_interest = x_interest, model = model, dataset = X)
  evaluate_counterfactual(counterfactual = cf, x_interest = x_interest, model = model)
  

