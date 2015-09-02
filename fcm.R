vector.norm <- function(x) {
  
  sqrt(sum(x^2))
}

fcm.PC <- function(fcm) {
  
  PC <- 0
  for (i in 1 : nrow(fcm$U)) {
    for (j in 1 : ncol(fcm$U)) {
      PC <- PC + fcm$U[i, j]^2
    }
  }
  
  PC / nrow(fcm$U)
}

fcm.XB <- function(fcm, data) {
  
  NXB <- 0 
  for (i in 1 : nrow(data)) {
    for (j in 1 : ncol(fcm$U)) {
      NXB <- fcm$U[i, j]^(fcm$fuzzifier) * vector.norm(data[i,] - fcm$centers[j,])
    }
  }
  NXB <- NXB / nrow(data)
  
  DXB <- 1000
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

fcm.membership.values <- function(data0, centers, fuzzifier) {
  
  u <- matrix(0, nrow = nrow(data0), ncol = nrow(centers))
  for (i in 1 : nrow(data0)) {
    for (j in 1 : nrow(centers)) {
      u[i, j] <- vector.norm(data0[i,] - centers[j,])^(2 / (1 - fuzzifier))
      z <- 0
      for (k in 1 : nrow(centers)) {
        z <- z + vector.norm(data0[i,] - centers[k,])^(2 / (1 - fuzzifier))
      }
      u[i, j] <- u[i, j] / z
    }
  }
  
  u
}

fcm.online.run <- function(data, nclusters, m = 2, ee = 0.01, centers = NULL) {
  
  if (is.null(centers)) {
    centers <- cbind(runif(nclusters, 0, 1), runif(nclusters, 0, 1))  
  } 
  fcm <- list(centers = centers, fuzzifier = m, U = matrix(0, nrow = nrow(data), ncol = nclusters))  
  for (i in 1 : nrow(data)) {
    ss <- 0.6 * exp(-(i/nrow(data)))
    u <- fcm.membership.values(data[i,], fcm$centers, m)  
    for (j in 1 : nclusters) {
      fcm$centers[j,] <- as.numeric(fcm$centers[j,] + ss * (u[j] ^ m) * (data[i,] - fcm$centers))
    }
    
  }
  fcm$U <- fcm.membership.values(data, centers, m)
  
  fcm
}

# fcm.online.run.visualize <- function(data, nclusters, m = 2, ee = 0.01, centers = NULL) {
#   
#   fcm <- fcm.online.run(data, nclusters, m, ee, FALSE, centers)
#   visualize(fcm, data)
# }

fcm.batch.run <- function(data, nclusters, m = 2, centers = NULL, ee = 0.01) {
  
  if (is.null(centers)) {
    centers <- cbind(runif(nclusters, 0, 1), runif(nclusters, 0, 1))  
  }
  U <- matrix(0, nrow = nrow(data), ncol = nclusters)
  
  for (k in 1 : 100) {
    prevU <- U
    for (i in 1 : nrow(data)) {
      for (j in 1 : nclusters) {
        z <- 0
        for (k in 1 : nclusters) {
          z <- z + (vector.norm(data[i, ] - centers[j, ]) / vector.norm(data[i, ] - centers[k, ])) ^ (m - 1)  
        }
        U[i, j] <- (1 / z)      
      }
    }
    
    for (j in 1 : nclusters) {
      temp <- numeric(ncol(centers))
      temp1 <- 0
      for (i in 1 : nrow(data)) {
        temp <- temp + ((U[i, j]) ^ m) * data[i,]
        temp1 <- temp1 + (U[i, j]) ^ m
      }
      centers[j,] <- as.numeric(temp / temp1)
    }
  
    if (max(abs(prevU - U)) < ee)  {
      break
    }
  }
  
  list(centers = centers, fuzzifier = m, U = U)
}