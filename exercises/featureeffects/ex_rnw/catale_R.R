order_levels <- function(dat, feature.name) {
  feature <- dat[, feature.name, with = FALSE][[1]]
  x.count <- as.numeric(table(dat[, feature.name, with = FALSE]))
  x.prob <- x.count / sum(x.count)
  K <- nlevels(dat[, feature.name, with = FALSE])
  
  dists <- lapply(setdiff(colnames(dat), feature.name), function(x) {
    browser()
    feature.x <- dat[, x, with = FALSE][[1]]
    dists <- expand.grid(levels(feature), levels(feature))
    colnames(dists) <- c("from.level", "to.level")
    if (inherits(feature.x, "factor")) {
      ## Derive! 
      A <- table(feature, feature.x) / x.count
      dists$dist <- rowSums(abs(A[dists[, "from.level"], ] - A[dists[, "to.level"], ])) / 2
    } else {
      # get decentiles
      quants <- quantile(feature.x, probs = seq(0, 1, length.out = 10), na.rm = TRUE, names = FALSE)
      # derive empirical distribution function for each category
      ecdfs <- data.frame(lapply(levels(feature), function(lev) {
        x.ecdf <- ecdf(feature.x[feature == lev])(quants)
        return(x.ecdf)
      }))
      colnames(ecdfs) <- levels(feature)
      # get pairwise absolute distances of empirical distribution function 
      # between the different categories
      ecdf.dists.all <- abs(ecdfs[, dists$from.level] - ecdfs[, dists$to.level])
      # get maximum distance over decentiles for each pair of categories
      dists$dist <- apply(ecdf.dists.all, 2, max)
    }
    dists
  })
  browser()
  dists.cumulated.long <- as.data.table(Reduce(function(d1, d2) {
    d1$dist <- d1$dist + d2$dist
    d1
  }, dists))
  dists.cumulated <- data.table::dcast(dists.cumulated.long, from.level ~ to.level, value.var = "dist")[, -1]
  diag(dists.cumulated) <- 0
  scaled <- cmdscale(dists.cumulated, k = 1)
  order(scaled)
}



get_diff_cat <- function(feature.k, feature.j) {
  
  #'  Calculates the pairwise distances of classes of j of a feature k 
  #'  
  #'  @param feature.k (character|factor): vector of values of categorical feature for which relative 
  #'  frequencies per class are calculated 
  #'  @param feature.j (character|factor: vector of values of categorical feature for which similarity 
  #'  based on feature k should be assessed. 
  #'      
  #'  Returns:
  #'      a data.frame with three columns: 
  #'      * column1: name of the first class
  #'      * column2: name of the second class 
  #'      * dist: the distributional distance between the two classes for feature k
  x.count <- as.numeric(table(feature.j))
  dists <- expand.grid(unique(feature.j), unique(feature.j))
  colnames(dists) <- c("class1", "class2")
  A <- table(feature.j, feature.k) / x.count
  dists$dist <- rowSums(abs(A[dists[, "class1"], ] - A[dists[, "class2"], ])) / 2
  return(dists)
}
data = data.frame(task$data())
dfc = get_diff_cat(feature.k = data[,"employment_duration"], feature.j = data[,"personal_status_sex"])

get_diff_numeric <- function(k, j, data) {
  feature.k <- data[, k]
  feature.j <- data[, j]
  x.count <- as.numeric(table(data[, j]))
  dists <- expand.grid(levels(feature.j), levels(feature.j))
  colnames(dists) <- c("from.level", "to.level")
  # get decentiles
  quants <- quantile(feature.k, probs = seq(0, 1, length.out = 10), na.rm = TRUE, names = FALSE)
  # derive empirical distribution function for each category
  ecdfs <- data.frame(lapply(levels(feature.j), function(lev) {
    x.ecdf <- ecdf(feature.k[feature.j == lev])(quants)
    return(x.ecdf)
  }))
  colnames(ecdfs) <- levels(feature.j)
  # get pairwise absolute distances of empirical distribution function 
  # between the different categories
  ecdf.dists.all <- abs(ecdfs[, dists$from.level] - ecdfs[, dists$to.level])
  # get maximum distance over decentiles for each pair of categories
  apply(ecdf.dists.all, 2, max)
}
