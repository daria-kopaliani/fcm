source("fcm-ensemble.R")
source("fcm-visualizer.R")


m0 <- cbind(runif(15, 0, 5), runif(15, 0, 5))
m1 <- cbind(runif(15, 8, 12), runif(15, 8, 11))
m2 <- cbind(runif(30, 4, 9), runif(30, 4, 9))
matrix <- rbind(m0, m1, m2)
data <- data.frame(apply(matrix, 2, function(x){(x-min(x))/(max(x)-min(x))}))
colnames(data) <- c("x", "y")
data <- data[sample(nrow(data)),]
plot(data)

# #simple data
# m0 <- cbind(runif(120, 0, 5), runif(120, 0, 3))
# m1 <- cbind(runif(120, 10, 12), runif(120, 8, 11))
# # m0 <- cbind(runif(45, 0, 12), runif(45, 0, 11))
# # m1 <- cbind(runif(45, 0, 12), runif(45, 0, 11))
# matrix <- rbind(m0, m1)
# data <- data.frame(apply(matrix, 2, function(x){(x-min(x))/(max(x)-min(x))}))
# colnames(data) <- c("x", "y")
# data <- data[sample(nrow(data)),]
# plot(data)



ensemble <- fcm.ensemble.init(pattern_length = ncol(data), initial_data = data[1:20,], n_fcm = 5, n_cascades = 6)
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
ensemble <- fcm.ensemble.init(pattern_length = ncol(data), initial_data = data[1:20,], n_fcm = 3, n_cascades = 3)
ensemble <- fcm.ensemble.online.run(ensemble, data)
visualize.progress(ensemble$best_fcm, data)

#best fcm
ensemble <- fcm.ensemble.init(pattern_length = ncol(data), initial_data = data[1:20,], n_fcm = 1, n_cascades = 5)
ensemble <- fcm.ensemble.online.run(ensemble, data)
visualize.progress(ensemble$best_fcm, data)

fcm2 <- fcm.online.run(fcm.init(2, 2, 2, data[1:20,]), data)
visualize.progress(fcm2, data)
fcm.PC(fcm2, data)

fcm3 <- fcm.online.run(fcm.init(3, 2, 2, data[1:20,]), data)
visualize.progress(fcm3, data)
fcm.PC(fcm3, data)

fcm4 <- fcm.online.run(fcm.init(3, 2, 2, data[1:20,]), data)
visualize.progress(fcm4, data)
fcm.PC(fcm4, data)

test <- function(n) {
  start.time <- Sys.time()
  
  fcm <- fcm.init(n, 2, 2, data[1:20,])
  fcm <- fcm.online.run(fcm, data)
  visualize.progress(fcm, data)
  print(paste("PC:", fcm.PC(fcm, data)))
  print(paste("Xie-Beni:", fcm.XB(fcm, data)))
  
  end.time <- Sys.time()
  print(end.time - start.time)
}



fcm2 <- fcm.init(2, 2, 2, data[1:20,])
fcm3 <- fcm.init(3, 2, 2, data[1:20,])
fcm2 <- fcm.online.run(fcm2, data)
fcm3 <- fcm.online.run(fcm3, data)



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

