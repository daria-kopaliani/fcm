source("fcm-ensemble.R")
source("fcm-visualizer.R")


m0 <- cbind(runif(25, 0, 5), runif(25, 0, 3))
m1 <- cbind(runif(25, 10, 12), runif(25, 8, 11))
matrix <- rbind(m0, m1)
data <- data.frame(apply(matrix, 2, function(x){(x-min(x))/(max(x)-min(x))}))
colnames(data) <- c("x", "y")
data <- data[sample(nrow(data)),]
plot(data)

n_clusters <- 2 

initial_centers <- fcm.batch.run(data[1:20, ], 2, 2)$centers
ensemble <- NULL
for (i in 1 : nrow(data)) {
  if (is.null(ensemble)) {
    ensemble <- fcm.ensemble.online.run(ensemble, data[i,], n_clusters, initial_centers = initial_centers) 
  } else {
    ensemble <- fcm.ensemble.online.run(ensemble, data[i,], n_clusters)
  }
  
  old.par <- par(mfrow=c(1, 3))
  visualize.progress(ensemble$fcms[[1]], data[1:i,])
  visualize.progress(ensemble$fcms[[2]], data[1:i,])
  visualize.progress(ensemble$fcms[[3]], data[1:i,])
  par(old.par)
  readline(prompt="Press [enter] to continue")
}


fcm <- NULL
for (i in 1 : nrow(data)) {
  fcm <- fcm.online.run(data[i,], 2, 2, centers = fcm$centers)
  visualize.progress(fcm, data[1:i,])
  readline(prompt="Press [enter] to continue")
}

