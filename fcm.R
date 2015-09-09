# only neccessary for online clustering
fcm.init <- function(n_clusters, fuzzifier, pattern_length, initial_data = NULL, inital_centers = NULL) {

  if (!is.null(inital_centers)) {
    centers <- inital_centers
  } else if (!is.null(initial_data)) {
    centers <- fcm.batch.run(initial_data, n_clusters, fuzzifier)$centers
  } else {
    centers <- matrix(runif(pattern_length * n_clusters, 0, 1), ncol = pattern_length, nrow = n_clusters)
  }
  
  list(centers = centers, fuzzifier = fuzzifier, PC = 0, XB = 1, NXB = 1)
}
  
fcm.online.run <- function(fcm, data, time_offset = 0) {
  
  if (class(data) == "numeric") {
   data <- t(as.matrix(data)) 
  }
  for (i in 1 : nrow(data)) {
    pattern <- as.matrix(data[i,])
    ss <- 0.6 * exp(-(i/nrow(data)))
    u <- fcm.membership.values(pattern, fcm$centers, fcm$fuzzifier)
    for (j in 1 : nrow(fcm$centers))  {
      fcm$centers[j,] <- fcm$centers[j,] + ss * (u[1, j] ^ fcm$fuzzifier) * (pattern - fcm$centers[j,])
    }
    fcm$PC <- fcm.online.PC(fcm, pattern, time_offset + i)
    fcm$NXB <- fcm.online.NXB(fcm, pattern, time_offset + i)
    fcm$XB <- fcm$NXB / fcm.DXB(fcm)
  }
  
  fcm
}

fcm.batch.run <- function(data, n_clusters, fuzzifier = 2, e = 0.01, max.epoch = 100) {
  
  centers <- matrix(runif(ncol(data) * n_clusters, 0, 1), ncol = ncol(data), nrow = n_clusters)
  U <- matrix(0, nrow = nrow(data), ncol = n_clusters)
  for (epoch in 1 : max.epoch) {
#     if (epoch %% 10 == 0) {
#       print(paste("epoch:", epoch))  
#     }
    
    prev.U <- U
    U <- fcm.membership.values(data, centers, fuzzifier)    
    for (j in 1 : n_clusters) {
      centers[j,] <- apply((U[,j] ^ fuzzifier) * data, 2, sum) / sum(U[,j] ^ fuzzifier)
    }
    if (max(abs(U - prev.U)) < e)  {
      break
    }
  }
  
  fcm <- list(centers = centers, fuzzifier = fuzzifier)
  fcm$PC <- fcm.batch.PC(fcm, data)
  fcm$XB <- fcm.batch.XB(fcm, data)
  fcm
}

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
fcm.cluster <- function(fcm, sample) {
  
  data <- as.matrix(sample)
  if (nrow(sample) == 1) {
    data <- t(data)
  } 
  membership.values <- fcm.membership.values(data, fcm$centers, fcm$fuzzifier) 
  as.matrix(apply(membership.values, 1, which.max))
}

vector.norm <- function(x) {
  
  sqrt(sum(x^2))
}

fcm.batch.PC <- function(fcm, data) {
  
  PC <- 0
  U <- fcm.membership.values(data, fcm$centers, fcm$fuzzifier)
  for (i in 1 : nrow(U)) {
    for (j in 1 : ncol(U)) {
      PC <- PC + U[i, j]^2
    }
  }
  
  PC / nrow(U)
}

fcm.online.PC <- function(fcm, pattern, k) {
  
  U <- fcm.membership.values(pattern, fcm$centers, fcm$fuzzifier)
  t <- 0
  for (i in 1 : nrow(fcm$centers)) {
    t <- t + U[i] ^ 2
  }
  
  fcm$PC + (t - fcm$PC) / (k + 1)
}

fcm.online.NXB <- function(fcm, pattern, k) {
  
  U <- fcm.membership.values(pattern, fcm$centers, fcm$fuzzifier)
  t <- 0
  for (i in 1 : ncol(U)) {
    t <- t + U[i] ^ 2 * (vector.norm(pattern - fcm$centers[i,]))^2
  }
  
  fcm$NXB + (t - fcm$NXB) / (k + 1)
}

fcm.DXB <- function(fcm) {
  
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
  
fcm.batch.XB <- function(fcm, data) {
  
  NXB <- 0 
  U <- fcm.membership.values(data, fcm$centers, fcm$fuzzifier)
  for (i in 1 : nrow(U)) {
    for (j in 1 : ncol(U)) {
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

fcm.membership.values <- function(sample, centers, fuzzifier) {
  
  sample <- as.matrix(sample)
  U <- matrix(0, nrow = nrow(sample), ncol = nrow(centers))
  for (i in 1 : nrow(sample)) {
    for (j in 1 : nrow(centers)) {
      norm <- vector.norm(sample[i,] - centers[j,])
      if (norm != 0) {
        U[i, j] <- norm^(2 / (1 - fuzzifier))
      }
      z <- 0
      for (k in 1 : nrow(centers)) {
        norm <- vector.norm(sample[i,] - centers[k,])
        if (norm != 0) {
          z <- z + norm^(2 / (1 - fuzzifier))  
        }
      }
      
      U[i, j] <- U[i, j] / z
    }
  }
  U
}