source("fcm.R")
fcm.ensemble.online.run <- function(data, n_clusters, centers = NULL) {
  
   if (is.null(centers)) {
    centers <- cbind(runif(nclusters, 0, 1), runif(nclusters, 0, 1))  
  }
  fcms <- list()
  n_fcms <- 3
  for (i in 1 : n_fcms) {
    fcms[[i]] <- fcm.online.run(data, n_clusters, fuzzifier = 1 + i, centers)
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


list1 <- list(x = "x1", y = "y1")
list2 <- list(x = "x2", y = "y2")
lists <- mget(paste0("list", 1:2))

lists <- list()
for (i in 1:3) {
 aList <- list(x = paste("x", i), y = paste("y", i))
 lists[[i]] <- aList
#  lists <- list(lists, aList)
 
}
