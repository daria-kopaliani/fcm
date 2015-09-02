source("fcm.R")
source("fcm-visualizer.R")

m0 <- cbind(runif(17, 0, 5), runif(17, 0, 3))
m1 <- cbind(runif(17, 10, 12), runif(17, 8, 11))
m2 <- cbind(runif(15, 4, 8), runif(15, 5, 8))
m3 <- cbind(runif(15, 0, 4), runif(15, 5, 10))
matrix <- rbind(m0, m1, m2, m3)
data <- data.frame(apply(matrix, 2, function(x){(x-min(x))/(max(x)-min(x))}))
colnames(data) <- c("x", "y")
data <- data[sample(nrow(data)),]
plot(data$x, data$y)


fcm <- fcm.batch.run(data, 3)
visualize(fcm, data)

fcm1 <- fcm.online.run(data, 3)
visualize(fcm1, data)

for (k in 1 : nrow(data)) {
  fcm <- fcm.online.run(data[k, ], 3, centers = fcm$centers)
  visualize(fcm, data[1:k, ])
}