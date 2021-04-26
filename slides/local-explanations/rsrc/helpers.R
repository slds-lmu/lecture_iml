
get_ice_curve_area = function (instance, features, predictor, param.set, grid.size) {
  # get feature ranges
  min = apply(credit[, features], MARGIN = 2L, min)
  max = apply(credit[, features], MARGIN = 2L, max)
  
  # get grid of potential values for features
  expgrid = expand.grid(x = seq(from = min[1], to = max[1], length.out = grid.size), 
    y = seq(from = min[2], to = max[2], length.out = grid.size))
  colnames(expgrid) = features
  
  # create new instances
  instance = instance[, !names(instance) %in% features]
  instance.df = instance[rep(row.names(instance), nrow(expgrid)), ]
  
  grid.df = cbind.data.frame(instance.df, expgrid)
  
  # predict outcomes
  pred = predictor$predict(newdata = grid.df)[[1]]
  cbind(expgrid, pred)
}

plot_ice_curve_area = function(grid, predictor, instance = NULL, x.interest) {
  nams = names(grid)[1:2]
  train.data = data.frame(predictor$data$get.x())
  
  if (all(predictor$data$feature.types[nams] %in% "numerical") ||
      all(predictor$data$feature.types[nams] %in% "categorical")) {
    p = ggplot(train.data, mapping = aes_string(x = nams[1],
      y = nams[2]))  +
      geom_point(color = "white") +
      theme_bw() +
      geom_tile(mapping = aes_string(x = nams[1], y = nams[2], fill = "pred"), data = grid) +
      stat_contour(mapping = aes_string(x = nams[1], y = nams[2], z = "pred"), data = grid, colour = "white") +
      metR::geom_text_contour(mapping = aes_string(x = nams[1], y = nams[2], z = "pred"),
        data = grid, colour = "white")
    if (nrow(instance) > 0) {
      p = p + geom_point(data = instance, aes_string(x=nams[1], y=nams[2]), colour = "black")
    }
    p = p + geom_point(data = x.interest, aes_string(x = nams[1], y = nams[2]), colour = "white")
    p = ggExtra::ggMarginal(p, type = "histogram")
  } else {
    train.data$pred = predictor$predict(train.data)[,1]
    categorical.feature = nams[predictor$data$feature.types[nams] == "categorical"]
    numerical.feature = setdiff(nams[1:2], categorical.feature)
    p = ggplot(train.data, mapping = aes_string(x = numerical.feature, y = "pred")) +
      geom_point(color = "white") +
      geom_line(data = grid, mapping = aes_string(x = numerical.feature,
        y = "pred", group = categorical.feature, color = categorical.feature)) +
      # geom_text(label = "counterfactual", aes(x = x.interest[1,numerical.feature]+8, y = x.interest$pred),
      #   colour = "gray") +
      theme_bw()
    if (nrow(instance) > 0) {
      p = p +  geom_point(data = instance, aes_string(x = numerical.feature, y = "pred"), colour = "black")
    }
    p = p + geom_point(aes(x = x.interest[1,numerical.feature], y = x.interest$pred), colour = "gray")
    p = ggExtra::ggMarginal(p, type = "histogram", margins = "x")
  }
  return(p)
}

