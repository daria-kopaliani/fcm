fcm.ensemble.online.run <- function(data, nclusters, fuzzifier = 2, centers = NULL) {
  
  fcms <- 
   if (is.null(centers)) {
    centers <- cbind(runif(nclusters, 0, 1), runif(nclusters, 0, 1))  
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


list1 <- list(x = "x", y = "y")