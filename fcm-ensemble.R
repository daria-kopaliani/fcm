source("fcm.R")

fcm.ensemble.online.run <- function(ensemble = NULL, data, n_clusters, initial_centers = NULL) {
  
  n_fcms <- 3

  if (is.null(ensemble)) {
    ensemble <- list("fcms" = list())
  } 
  
  for (i in 1 : n_fcms) {
    if (i > length(ensemble$fcms)) {
      if (!is.null(initial_centers)) {
        centers <- inital_centers
      } else {
        centers <- matrix(runif(n_clusters * ncol(data)), nrow = n_clusters, ncol = ncol(data))
        print(paste("null centers for", i))  
      }
    } else {
      centers = ensemble$fcms[[i]]$centers  
    }
    ensemble$fcms[[i]] <- fcm.online.run(data, n_clusters, fuzzifier = 1 + i, centers)
  }
  
  ensemble
}