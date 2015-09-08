vector.norm <- function(x) {
  
  sqrt(sum(x^2))
}

fcm.PC <- function(fcm) {
  
  PC <- 0
  for (i in 1 : nrow(fcm$membership.values)) {
    for (j in 1 : ncol(fcm$membership.values)) {
      PC <- PC + fcm$membership.values[i, j]^2
    }
  }
  
  PC / nrow(fcm$membership.values)
}

fcm.XB <- function(fcm, data) {
  
  NXB <- 0 
  U <- fcm.membership.values(data, fcm$centers, fcm$fuzzifier)
  for (i in 1 : nrow(data)) {
    for (j in 1 : ncol(fcm$membership.values)) {
      NXB <- NXB + U[i, j]^(fcm$fuzzifier) * vector.norm(data[i,] - fcm$centers[j,])
    }
  }
  NXB <- NXB / nrow(data)
  
  DXB <- .Machine$integer.max
  for (i in 1 : nrow(fcm$centers)) {
    for (j in 1 : nrow(fcm$centers)) {
      if (i != j) {
        dist <- vector.norm(fcm$centers[i,] - fcm$centers[j])
        if (dist < DXB) {
          DXB <- dist
        }
      }
    }
  }
  
  NXB/DXB
}

fcm.membership.values <- function(data, centers, fuzzifier) {
  
  sample <- as.matrix(data)
  membership.values <- matrix(0, nrow = nrow(sample), ncol = nrow(centers))
  for (i in 1 : nrow(sample)) {
    for (j in 1 : nrow(centers)) {
      membership.values[i, j] <- vector.norm(sample[i,] - centers[j,])^(2 / (1 - fuzzifier))
      z <- 0
      for (k in 1 : nrow(centers)) {
        z <- z + vector.norm(sample[i,] - centers[k,])^(2 / (1 - fuzzifier))
      }
      membership.values[i, j] <- membership.values[i, j] / z
    }
  }
  membership.values
}

# only neccessary for online clustering
fcm.init <- function(n_clusters, fuzzifier, pattern_length, initial_data = NULL, inital_centers = NULL) {

  if (!is.null(inital_centers)) {
    centers <- inital_centers
  } else if (!is.null(initial_data)) {
    centers <- fcm.batch.run(initial_data, n_clusters, fuzzifier)$centers
  } else {
    centers <- matrix(runif(pattern_length * n_clusters, 0, 1), ncol = pattern_length, nrow = n_clusters)
  }
  
  list(centers = centers, fuzzifier = fuzzifier, PC = 1, XB = 1, NXB = 1, membership.values = NULL)
}
  
fcm.online.run <- function(fcm, data) {
  
  for (i in 1 : nrow(data)) {
    sample <- as.matrix(data[i,])
    ss <- 0.6 * exp(-(i/nrow(data)))
    u <- fcm.membership.values(sample, fcm$centers, fcm$fuzzifier)
    for (j in 1 : nrow(fcm$centers))  {
      fcm$centers[j,] <- fcm$centers[j,] + ss * (u[1, j] ^ fcm$fuzzifier) * (sample - fcm$centers[j,])
    }
  }
  fcm$membership.values <- fcm.membership.values(data, fcm$centers, fcm$fuzzifier)  
  fcm
}

fcm.batch.run <- function(data, nclusters, fuzzifier = 2, e = 0.01, max.epoch = 100) {
  
  centers <- matrix(runif(ncol(data) * nclusters, 0, 1), ncol = ncol(data), nrow = nclusters)
  membership.values <- matrix(0, nrow = nrow(data), ncol = nclusters)
  for (k in 1 : max.epoch) {
    prev.membership.values <- membership.values
    membership.values <- fcm.membership.values(data, centers, fuzzifier)    
    for (j in 1 : nclusters) {
      centers[j,] <- apply((membership.values[,j] ^ fuzzifier) * data, 2, sum) / sum(membership.values[,j] ^ fuzzifier)
    }
    if (max(abs(prev.membership.values - membership.values)) < e)  {
      break
    }
  }
  
  list(centers = centers, fuzzifier = fuzzifier, membership.values = membership.values)
}

fcm.cluster <- function(fcm, sample) {
  
  data <- as.matrix(sample)
  if (nrow(sample) == 1) {
    data <- t(data)
  } 
  membership.values <- fcm.membership.values(data, fcm$centers, fcm$fuzzifier) 
  as.matrix(apply(membership.values, 1, which.max))
}