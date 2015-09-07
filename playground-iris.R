source("fcm.R")

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


irisData <- iris[, 1:4]
irisData <- apply(irisData, 2, function(x){(x-min(x))/(max(x)-min(x))})
order <- sample(nrow(irisData))
irisData <- irisData[order,]

n_clusters <- 6
fuzzifier <- 4
centers <- fcm.batch.run(irisData[1:20,], n_clusters, fuzzifier)$centers
fcm <- fcm.online.run(irisData, n_clusters, fuzzifier = fuzzifier, centers = centers)
clustering.accuracy(fcm.cluster(fcm, irisData), iris[order, 5])


