library("HDclassif")
source("fcm.R")

data(wine)
benchmark <- wine
data <- apply(benchmark[, -1], 2, function(x){(x-min(x))/(max(x)-min(x))})
order <- sample(nrow(data))
data <- data[order,]

classes <- matrix("class1", nrow = nrow(wine))
classes[wine[,1] == 2,] <- "class2"
classes[wine[,1] == 3,] <- "class3"
benchmark1 <- cbind(classes, wine)

n_clusters <- 7
fuzzifier <- 3
fcm <- fcm.init(n_clusters, fuzzifier, ncol(data), data[1:20,])
fcm <- fcm.online.run(fcm, data)
clustering.accuracy(fcm.cluster(fcm, data), benchmark1[order, 1])