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
fcm1 <- fcm.online.run(data, 3)
visualize.clusters(fcm1, data)

#####
fcm2_0 <- fcm.batch.run(data[1:20,], 3)
fcm2 <- fcm.online.run(data, 3, centers = fcm2_0$centers)
visualize.clusters(fcm2, data)

#####
fcm3_0 <- fcm.batch.run(data[1:20,], 4)
fcm3 <- fcm.online.run(data, 4, centers = fcm3_0$centers, fuzzifier = 3)
visualize.clusters(fcm3, data)


fcm3 <- fcm.online.run(data, 4, centers = fcm3_0$centers, fuzzifier = 3)
visualize.clusters(fcm3, data)

visualize.lattice(fcm3)

#####
centers <- fcm.batch.run(data[1:20, ], 4)$centers
for (k in 1 : nrow(data)) {
  fcm <- fcm.online.run(data[k, ], 4, centers = centers)
  visualize.clusters(fcm, data[1:k, ])
  centers <- fcm$centers
  readline(prompt="Press [enter] to continue")
}

#####
irisData <- iris[, 1:4]
irisData <- apply(irisData, 2, function(x){(x-min(x))/(max(x)-min(x))})
order <- sample(nrow(irisData))
irisData <- irisData[order,]

fcm4 <- fcm.batch.run(irisData, 3, 4)
clusters <- fcm.cluster(fcm4, irisData)
clustering.accuracy(clusters, iris[order, 5])

fcm5 <- fcm.batch.run(irisData, 6, 2)
clusters <- fcm.cluster(fcm5, irisData)
clustering.accuracy(clusters, iris[order, 5])

clustering.accuracy <- function(clusters, labels) {
  
  accuracy <- 0
  
  clusters_data <- factor(clusters)
  for (cluster in levels(clusters_data)) {
    cc <- labels[(clusters == cluster)]
    max_occur <- 0
    class <- levels(cc)[1]
    for (label in levels(cc)) {
      if (sum(cc == label) > max_occur) {
        max_occur <- sum(cc == label)
        class <- label
      }
    }
    accuracy <- accuracy + (sum(cc == class) / length(cc)) / length(levels(clusters_data))
  }
  
  accuracy
}
