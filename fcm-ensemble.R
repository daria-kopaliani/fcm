source("fcm.R")

fcm.ensemble.init <- function(pattern_length, initial_data, n_fcms = 2, n_cascades = 2) {
 
  ensemble <- list("cascades" = list(), "best_fcm" = NULL)
  for (j in 1 : n_cascades) {
    cascade <- list("fcms" = list(), "best_fcm" = NULL)
    centers <- fcm.batch.run(initial_data, 1 + j, fuzzifier)$centers
    for (i in 1 : n_fcms) {
      cascade$fcms[[i]] <- fcm.init(n_clusters = 1 + j, fuzzifier = 2 *i + 1, pattern_length, inital_centers = centers)
    }
    ensemble$cascades[[j]] <- cascade  
  }
  
  ensemble
}

fcm.ensemble.online.run <- function(ensemble, data, k = 0) {
  
  for (i in 1 : nrow(data)) {
    sample <- data[i,]
    for (cascade_index in 1 : length(ensemble$cascades)) {
      cascade <- ensemble$cascades[[cascade_index]]
      best_PC <- 0
      for (fcm_index in 1 : length(cascade$fcms)) {
        fcm <- cascade$fcms[[fcm_index]]
        fcm <- fcm.online.run(fcm, sample)
        fcm$PC <- PC(fcm, sample, i + k)
        cascade$fcms[[fcm_index]] <- fcm
        if (fcm$PC > best_PC) {
          best_PC <- fcm$PC
          cascade$best_fcm <- fcm
        }
      }
      ensemble$cascades[[cascade_index]] <- cascade
    } 
  }
  
  ensemble
}

PC <- function(fcm, pattern, k) {
  
  membership_values <- fcm.membership.values(pattern, fcm$centers, fcm$fuzzifier)
  t <- 0
  for (i in 1 : nrow(fcm$centers)) {
    t <- t + membership_values[i] ^ 2
  }
  
  fcm$PC + ((1 + k)^(-1)) * (t - fcm$PC)
}

XB <- function(fcm, pattern, k) {
  
  membership_values <- fcm.membership.values(pattern, fcm$centers, fcm$fuzzifier)
  t <- 0
  for (i in 1 : ncol(membership_values)) {
    membership_values[i] ^ 2 * (vector.norm(pattern - fcm$centers[i,]))^2
  }
  NXB <- (membership_values ^ 2)  * (vector.norm(pattern - fcm$centers)^2) - NXB
}
