order_levels = function(data, feature.name) {
  #'  Orders levels of a nominal scaled feature according to other features in 
  #'  a dataset.
  #'  
  #'  @param data (data.frame): data which contains feature which 
  #'  should be ordered but also other features used for ordering.
  #'  @param feature.name (character(1)): name of categorical feature which shoul 
  #'  be ordered
  #'      
  #'  Returns: the vector of ordered class labels
  feature = data[, feature.name]
  others = setdiff(colnames(data), feature.name)
  feature.lev = unique(feature)
  
  # Iterate over other features 
  dists = lapply(others, function(k) {
    feature.k = data[, k]
    if (inherits(feature.k, "factor") | inherits(feature.k, "character")) {
      ############ NEEDS TO BE IMPLEMENTED (SEE BELOW)! ##########
      dists = get_diff_cat(feature.k, feature)
    } else {
      dists = get_diff_numeric(feature.k, feature)
    }
    dists
  })
  dists.cumulated.long = as.data.frame(Reduce(function(d1, d2) {
    d1$dist = d1$dist + d2$dist
    d1
  }, dists))
  # Create a matrix of distances
  dists.cumulated = reshape2::dcast(dists.cumulated.long, class1 ~ class2, value.var = "dist")[, -1]
  rownames(dists.cumulated) = colnames(dists.cumulated)
  # conduct multi-dimensional scaling (here:principal coordinates analysis)
  # based on dissimilarity matrix it assigns to each item a location in a low dimensional space 
  # the closer the location, the more similar the items are
  scaled = cmdscale(dists.cumulated, k = 1)
  feature.lev[order(scaled)]
}

get_diff_numeric = function(feature.k, feature.j) {
  #'  Calculates the pairwise distances of classes of j of a numeric feature k 
  #'  
  #'  @param feature.k (character|factor): vector of values of numeric feature for which relative 
  #'  frequencies per class are calculated 
  #'  @param feature.j (character|factor): vector of values of categorical feature for which similarity 
  #'  based on feature k should be assessed. 
  #'      
  #'  Returns:
  #'      a data.frame with three columns: 
  #'      * class1: name of the first class
  #'      * class2: name of the second class 
  #'      * dist: the distributional distance between the two classes for feature k
  
  # set up data.frame which we will fill later
  dists = expand.grid(unique(feature.j), unique(feature.j))
  colnames(dists) = c("class1", "class2")
  # get decentiles
  quants = quantile(feature.k, probs = seq(0, 1, length.out = 10), na.rm = TRUE, names = FALSE)
  # derive empirical distribution function for each category
  ecdfs = data.frame(lapply(unique(feature.j), function(lev) {
    x.ecdf = ecdf(feature.k[feature.j == lev])(quants)
    return(x.ecdf)
  }))
  colnames(ecdfs) = unique(feature.j)
  # get pairwise absolute distances of empirical distribution function 
  # between the different categories
  ecdf.dists.all = abs(ecdfs[, dists$class1] - ecdfs[, dists$class2])
  # get maximum distance over decentiles for each pair of categories
  dists$dist = apply(ecdf.dists.all, 2, max)
  return(dists)
}

get_diff_cat = function(feature.k, feature.j) {
  
  #'  Calculates the pairwise distances of classes of j of a categorical feature k 
  #'  
  #'  @param feature.k (character|factor): vector of values of categorical feature for which relative 
  #'  frequencies per class are calculated 
  #'  @param feature.j (character|factor): vector of values of categorical feature for which similarity 
  #'  based on feature k should be assessed. 
  #'      
  #'  Returns:
  #'      a data.frame with three columns: 
  #'      * class1: name of the first class
  #'      * class2: name of the second class 
  #'      * dist: the distributional distance between the two classes for feature k
  
  # set up data.frame which we will fill later
  dists = expand.grid(unique(feature.j), unique(feature.j))
  colnames(dists) = c("class1", "class2")
  
  # get relative frequency table
  ############### YOUR TODO ###################
  
  # compute pairwise absolute distances 
  ############### YOUR TODO ###################
  
}

if (FALSE) {
  credit = read.csv("datasets/credit.csv")
  
  ## This should already work WITHOUT get_diff_cat()
  credit.sub = credit[, c("age", "personal_status_sex")]
  order_levels(credit.sub, "personal_status_sex")
  get_diff_numeric(feature.k = credit.sub[,"age"], feature.j = credit.sub[,"personal_status_sex"])
  # to see what the methods does step by step use debug(order_levels) or debug(get_diff_numeric)
  
  ## This should work AFTER you have implemented get_diff_cat()
  order_levels(credit, "personal_status_sex")
  get_diff_cat(feature.k = credit[,"employment_duration"], feature.j = credit[,"personal_status_sex"])
}
