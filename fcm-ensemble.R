source("fcm.R")

fcm.ensemble.init <- function(pattern_length, initial_data, n_fcms = 2, n_cascades = 2) {
 
  ensemble <- list(cascades = list(), best_fcm = NULL)
  for (j in 1 : n_cascades) {
    cascade <- list(fcms = list(), best_fcm = NULL)
    centers <- fcm.batch.run(initial_data, 1 + j, 2)$centers
    for (i in 1 : n_fcms) {
      cascade$fcms[[i]] <- fcm.init(n_clusters = 1 + j, fuzzifier = i + 1, pattern_length, inital_centers = centers)
    }
    ensemble$cascades[[j]] <- cascade  
  }
  
  ensemble
}

fcm.ensemble.online.run <- function(ensemble, data, k = 0) {
  
  for (i in 1 : nrow(data)) {
    if (i %% 10 == 0) {
      print(i)  
    }
    
    sample <- data[i,]
    best_cascadeXB <- .Machine$integer.max
    best_cascadePC <- 0
    for (cascade_index in 1 : length(ensemble$cascades)) {
      cascade <- ensemble$cascades[[cascade_index]]
      best_PC <- 0
      best_XB <- .Machine$integer.max
      for (fcm_index in 1 : length(cascade$fcms)) {
        fcm <- cascade$fcms[[fcm_index]]
        fcm <- fcm.online.run(fcm, sample)
        
        if (fcm$PC > best_PC) {
          best_PC <- fcm$PC
#           cascade$best_fcm <- fcm
        }
        if (fcm$XB < best_XB) {
          best_XB <- fcm$XB
          cascade$best_fcm <- fcm
        } 
        cascade$fcms[[fcm_index]] <- fcm
      }
      
      #determining cascade with the Xie-Beni index (for the winner neuron)
      
      if (cascade$best_fcm$PC > best_cascadePC) {
        best_cascadePC <- cascade$best_fcm$PC
#         ensemble$best_fcm <- cascade$best_fcm 
      }
      if (cascade$best_fcm$XB < best_cascadeXB) {
        best_cascadeXB <- cascade$best_fcm$XB
        ensemble$best_fcm <- cascade$best_fcm 
      }


      ensemble$cascades[[cascade_index]] <- cascade
    } 
  }
  
  ensemble
}

# PC <- function(fcm, pattern, k) {
#   
#   membership_values <- fcm.membership.values(pattern, fcm$centers, fcm$fuzzifier)
#   t <- 0
#   for (i in 1 : nrow(fcm$centers)) {
#     t <- t + membership_values[i] ^ 2
#   }
#   
#   fcm$PC + ((1 + k)^(-1)) * (t - fcm$PC)
# }

NXB <- function(fcm, pattern, k) {
  
  membership_values <- fcm.membership.values(pattern, fcm$centers, fcm$fuzzifier)
  t <- 0
  for (i in 1 : ncol(membership_values)) {
    t <- t + membership_values[i] ^ 2 * (vector.norm(pattern - fcm$centers[i,]))^2
  }
  
  fcm$NXB + (t - fcm$NXB) / (k + 1)
}

DXB <- function(fcm) {

  DXB <- .Machine$integer.max
  for (j in 1 : nrow(fcm$centers)) {
    center <- fcm$centers[j,]
    for (i in 1 : nrow(fcm$centers)) {
      if (j != i) {
        dist <- vector.norm(fcm$centers[j,] - fcm$centers[i,])^2
        if (dist < DXB) {
          DXB <- dist
        }
      }
    }
  }
  
  DXB
}

