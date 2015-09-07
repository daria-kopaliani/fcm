source("fcm.R")
source("fcm-visualizer.R")

m0 <- cbind(runif(27, 0, 5), runif(27, 0, 3))
m1 <- cbind(runif(27, 10, 12), runif(27, 8, 11))
m2 <- cbind(runif(25, 4, 8), runif(25, 5, 8))
m3 <- cbind(runif(25, 0, 4), runif(25, 5, 10))
matrix <- rbind(m0, m1, m2, m3)
data <- data.frame(apply(matrix, 2, function(x){(x-min(x))/(max(x)-min(x))}))
colnames(data) <- c("x", "y")
data <- data[sample(nrow(data)),]
plot(data$x, data$y)

#####
fcm <- fcm.batch.run(data, 4, 5)
visualize.clusters(fcm, data)

#####
fcm1 <- fcm.init(3, 2, ncol(data), data[1:20,])
fcm.online.run(fcm1, data)
visualize.clusters(fcm1, data)

#####
fcm2 <- fcm.init(4, 2, ncol(data), data[1:20,])
fcm.online.run(fcm2, data)
visualize.clusters(fcm2, data)

#####
fcm3 <- fcm.init(4, 2, ncol(data), data[1:20,])
for (k in 1 : nrow(data)) {
  fcm3 <- fcm.online.run(fcm3, data[k,])
  visualize.clusters(fcm3, data[1:k, ])
  readline(prompt="Press [enter] to continue")
}