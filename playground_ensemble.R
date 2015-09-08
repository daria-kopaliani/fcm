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

#simple data
m0 <- cbind(runif(120, 0, 5), runif(120, 0, 3))
m1 <- cbind(runif(120, 10, 12), runif(120, 8, 11))
matrix <- rbind(m0, m1)
data <- data.frame(apply(matrix, 2, function(x){(x-min(x))/(max(x)-min(x))}))
colnames(data) <- c("x", "y")
data <- data[sample(nrow(data)),]
plot(data)



ensemble <- fcm.ensemble.init(pattern_length = ncol(data), initial_data = data[1:20,], n_fcm = 2, n_cascades = 3)
ensemble <- fcm.ensemble.online.run(ensemble, data)
old.par <- par(mfrow=c(3, 2))
visualize.progress(ensemble$cascades[[1]]$fcms[[1]], data)
visualize.progress(ensemble$cascades[[1]]$fcms[[2]], data)
visualize.progress(ensemble$cascades[[2]]$fcms[[1]], data)
visualize.progress(ensemble$cascades[[2]]$fcms[[2]], data)
visualize.progress(ensemble$cascades[[3]]$fcms[[1]], data)
visualize.progress(ensemble$cascades[[3]]$fcms[[2]], data)
par(old.par)

#single fcm
data <- data[1:20,]
ensemble <- fcm.ensemble.init(pattern_length = ncol(data), initial_data = data[1:20,], n_fcm = 1, n_cascades = 1)
ensemble <- fcm.ensemble.online.run(ensemble, data)
visualize.progress(ensemble$cascades[[1]]$fcms[[1]], data)
fcm <- ensemble$cascades[[1]]$fcms[[1]]

fcm.PC(fcm)


# one step visualization
ensemble <- fcm.ensemble.init(pattern_length = ncol(data), initial_data = data[1:20,], n_fcm = 2, n_cascades = 3)
for (i in 1 : nrow(data)) {
  ensemble <- fcm.ensemble.online.run(ensemble, data[i,], k = i)
  old.par <- par(mfrow=c(3, 2))
  visualize.progress(ensemble$cascades[[1]]$fcms[[1]], data[1:i,])
  visualize.progress(ensemble$cascades[[1]]$fcms[[2]], data[1:i,])
  visualize.progress(ensemble$cascades[[2]]$fcms[[1]], data[1:i,])
  visualize.progress(ensemble$cascades[[2]]$fcms[[2]], data[1:i,])
  visualize.progress(ensemble$cascades[[3]]$fcms[[1]], data[1:i,])
  visualize.progress(ensemble$cascades[[3]]$fcms[[2]], data[1:i,])
  par(old.par)
  readline(prompt="Press [enter] to continue")
}

