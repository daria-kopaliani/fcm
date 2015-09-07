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
  for (i in 1 : nrow(data)) {
    for (j in 1 : ncol(fcm$membership.values)) {
      NXB <- fcm$U[i, j]^(fcm$fuzzifier) * vector.norm(data[i,] - fcm$centers[j,])
    }
  }
  NXB <- NXB / nrow(data)
  
  DXB <- .Machine$integer.max
  for (i in 1 : nrow(fcm$centers)) {
    for (i in 1 : nrow(fcm$centers)) {
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

fcm.online.run <- function(data, nclusters, fuzzifier = 2, centers = NULL) {
  
  if (is.null(centers)) {
    centers <- matrix(runif(ncol(data) * nclusters, 0, 1), ncol = ncol(data), nrow = nclusters)
  } 
  fcm <- list(centers = centers, fuzzifier = fuzzifier, membership.values = NULL)  
  for (i in 1 : nrow(data)) {
    sample <- as.matrix(data[i,])
    ss <- 0.6 * exp(-(i/nrow(data)))
    u <- fcm.membership.values(sample, fcm$centers, fuzzifier)
    for (j in 1 : nclusters)  {
      fcm$centers[j,] <- fcm$centers[j,] + ss * (u[1, j] ^ fuzzifier) * (sample - fcm$centers[j,])
    }
  }
  fcm$membership.values <- fcm.membership.values(data, fcm$centers, fuzzifier)  

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