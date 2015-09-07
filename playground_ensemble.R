source("fcm-ensemble.R")
source("fcm-visualizer.R")


m0 <- cbind(runif(5, 0, 5), runif(5, 0, 3))
m1 <- cbind(runif(5, 10, 12), runif(5, 8, 11))
matrix <- rbind(m0, m1)
data <- data.frame(apply(matrix, 2, function(x){(x-min(x))/(max(x)-min(x))}))
colnames(data) <- c("x", "y")
data <- data[sample(nrow(data)),]
plot(data)

fcms <- list()
n_fcms <- 3
centers <- cbind(runif(nclusters, 0, 1), runif(nclusters, 0, 1))  
for (i in 1 : n_fcms) {
  fcms[[i]] <- fcm.online.run(data, n_clusters, fuzzifier = 1 + i, centers)
}