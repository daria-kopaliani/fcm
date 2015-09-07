library("HDclassif")
source("fcm.R")

benchmark <- wine
data <- apply(benchmark[, -1], 2, function(x){(x-min(x))/(max(x)-min(x))})
order <- sample(nrow(data))
data <- data[order,]

classes <- matrix("class1", nrow = nrow(wine))
classes[wine[,1] == 2,] <- "class2"
classes[wine[,1] == 3,] <- "class3"
benchmark1 <- cbind(classes, wine)

n_clusters <- 7
fuzzifier <- 2
centers <- fcm.batch.run(data[1:20,], n_clusters, fuzzifier)$centers
fcm <- fcm.online.run(data, n_clusters, fuzzifier = fuzzifier, centers = centers)
clustering.accuracy(fcm.cluster(fcm, data), benchmark1[order, 1])

clusters <- fcm.cluster(fcm, data)
labels <- benchmark[order, 1]

tt <- cbind(clusters, as.matrix(labels))