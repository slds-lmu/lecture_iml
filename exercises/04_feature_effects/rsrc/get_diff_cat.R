get_diff_cat <- function(feature.k, feature.j) {
  
  #'  Calculates the pairwise distances of classes of j of a categorical feature k 
  #'  
  #'  @param feature.k (character|factor): vector of values of categorical 
  #'  feature for which relative frequencies per class are calculated 
  #'  @param feature.j (character|factor): vector of values of categorical 
  #'  feature for which similarity based on feature k should be assessed. 
  #'      
  #'  Returns:
  #'      a data.frame with three columns: 
  #'      * class1: name of the first class
  #'      * class2: name of the second class 
  #'      * dist: the distributional distance between the two classes for feature k
  
  # set up data.frame which we will fill later
  dists <- expand.grid(unique(feature.j), unique(feature.j))
  colnames(dists) <- c("class1", "class2")
  # get relative frequency table
  x.count <- as.numeric(table(feature.j))
  A <- table(feature.j, feature.k) / x.count
  dists$dist <- rowSums(abs(A[dists[, "class1"], ] - A[dists[, "class2"], ])) / 2
  return(dists)
}
