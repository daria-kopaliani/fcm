source("fcm.R")
source("fcm-ensemble.R")

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

n_clusters <- 3
fuzzifier <- 2
fcm <- fcm.init(n_clusters, fuzzifier, ncol(irisData), irisData[1:20,])
fcm <- fcm.online.run(fcm, irisData)
print(fcm.batch.XB(fcm))
clustering.accuracy(fcm.cluster(fcm, irisData), iris[order, 5])

fcm001 <- fcm.init(2, 2, ncol(irisData), irisData[1:20,])
fcm001 <- fcm.online.run(fcm001, irisData)
fcm002 <- fcm.init(3, 2, ncol(irisData), irisData[1:20,])
fcm002 <- fcm.online.run(fcm002, irisData)

ensemble <- fcm.ensemble.init(ncol(irisData), irisData[1:20,], 8, 6)
ensemble <- fcm.ensemble.online.run(ensemble, irisData)

fcm1 <- ensemble$cascades[[1]]$fcms[[1]]
fcm2 <- ensemble$cascades[[2]]$fcms[[1]]
fcm3 <- ensemble$cascades[[5]]$fcms[[7]]

fcm1_1 <- fcm.batch.run(irisData, 2)
fcm2_1 <- fcm.batch.run(irisData, 3)