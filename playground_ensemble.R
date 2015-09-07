source("fcm-ensemble.R")
source("fcm-visualizer.R")


m0 <- cbind(runif(10, 0, 5), runif(10, 0, 3))
m1 <- cbind(runif(10, 10, 12), runif(10, 8, 11))
m2 <- cbind(runif(45, 0, 12), runif(45, 0, 11))
matrix <- rbind(m0, m1, m2)
data <- data.frame(apply(matrix, 2, function(x){(x-min(x))/(max(x)-min(x))}))
colnames(data) <- c("x", "y")
data <- data[sample(nrow(data)),]
plot(data)


ensemble <- fcm.ensemble.init(n_clusters = 3, pattern_length = ncol(data), initial_data = data[1:20,], n_fcm = 3)

for (i in 1 : nrow(data)) {
  ensemble <- fcm.ensemble.online.run(ensemble, data[i,], k = i)
  old.par <- par(mfrow=c(1, 3))
  visualize.progress(ensemble$fcms[[1]], data[1:i,])
  visualize.progress(ensemble$fcms[[2]], data[1:i,])
  visualize.progress(ensemble$fcms[[3]], data[1:i,])
  par(old.par)
  readline(prompt="Press [enter] to continue")
}




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

