source("fcm.R")

fcm.ensemble.init <- function(n_clusters, pattern_length, initial_data, n_fcms = 4) {
 
  ensemble <- list("fcms" = list(), "best_fcm" = NULL)
  centers <- fcm.batch.run(initial_data, n_clusters, fuzzifier)$centers
  for (i in 1 : n_fcms) {
    ensemble$fcms[[i]] <- fcm.init(n_clusters, fuzzifier = 2 *i + 1, pattern_length, inital_centers = centers)
  }
  
  ensemble
}

fcm.ensemble.online.run <- function(ensemble, data, k = 0) {

  best_PC <- 0
  for (i in 1 : length(ensemble$fcms)) {
    fcm <- ensemble$fcms[[i]]
    fcm <- fcm.online.run(fcm, data)
    fcm$PC <- PC(fcm, data, i + k)
    ensemble$fcms[[i]] <- fcm
    if (fcm$PC > best_PC) {
      best_PC <- fcm$PC
      ensemble$best_fcm <- fcm
    }
  }
  
  ensemble
}

PC <- function(fcm, data, k) {
  
  membership_values <- fcm.membership.values(data, fcm$centers, fcm$fuzzifier)
  t <- 0
  for (i in 1 : nrow(fcm$centers)) {
    t <- t + membership_values[i] ^ 2 - fcm$PC
  }
  
  fcm$PC + t / (k + 1)
}
