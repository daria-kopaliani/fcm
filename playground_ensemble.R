source("fcm-ensemble.R")
source("fcm-visualizer.R")


#fuzzy data
m0 <- cbind(runif(15, 0, 5), runif(15, 0, 5))
m1 <- cbind(runif(15, 8, 12), runif(15, 8, 11))
m2 <- cbind(runif(30, 4, 9), runif(30, 4, 9))
fuzzy_data <- data.frame(apply(rbind(m0, m1, m2), 2, function(x){(x-min(x))/(max(x)-min(x))}))
colnames(fuzzy_data) <- c("x", "y")
fuzzy_data <- fuzzy_data[sample(nrow(fuzzy_data)),]
plot(fuzzy_data)

#crisp data
m0 <- cbind(runif(20, 0, 5), runif(20, 0, 3))
m1 <- cbind(runif(20, 10, 12), runif(20, 8, 11))
m2 <- cbind(runif(20, 5, 7), runif(20, 5, 7))
crisp_data <- data.frame(apply(rbind(m0, m1, m2), 2, function(x){(x-min(x))/(max(x)-min(x))}))
colnames(crisp_data) <- c("x", "y")
crisp_data <- crisp_data[sample(nrow(crisp_data)),]
plot(crisp_data)


#extra fuzzy data
m0 <- cbind(runif(40, 0, 9), runif(40, 0, 9))
m1 <- cbind(runif(40, 4, 12), runif(40, 4, 12))
extra_fuzzy_data <- data.frame(apply(rbind(m0, m1), 2, function(x){(x-min(x))/(max(x)-min(x))}))
colnames(extra_fuzzy_data) <- c("x", "y")
extra_fuzzy_data <- extra_fuzzy_data[sample(nrow(extra_fuzzy_data)),]
plot(extra_fuzzy_data)

# best fcm
# data <- fuzzy_data
# data <- extra_fuzzy_data
data <- crisp_data
ensemble <- fcm.ensemble.init(pattern_length = ncol(data), initial_data = data[1:20,], n_fcm = 5, n_cascades = 5)
ensemble <- fcm.ensemble.online.run(ensemble, data)
visualize.progress(ensemble$best_fcm, data)

visualize.progress(ensemble$cascades[[1]]$fcms[[1]], data)
print(paste("online PC", ensemble$cascades[[1]]$fcms[[1]]$PC))
print(paste("online XB", ensemble$cascades[[1]]$fcms[[1]]$XB))
print(paste("batch PC", fcm.batch.PC(ensemble$cascades[[1]]$fcms[[1]], data)))
print(paste("batch XB", fcm.batch.XB(ensemble$cascades[[1]]$fcms[[1]], data)))

visualize.progress(ensemble$cascades[[2]]$fcms[[1]], data)
print(paste("online PC", ensemble$cascades[[2]]$fcms[[1]]$PC))
print(paste("online XB", ensemble$cascades[[2]]$fcms[[1]]$XB))
print(paste("batch PC", fcm.batch.PC(ensemble$cascades[[2]]$fcms[[1]], data)))
print(paste("batch XB", fcm.batch.XB(ensemble$cascades[[2]]$fcms[[1]], data)))



test.batch <- function(n, f = 2) {
  fcm <- fcm.batch.run(data, n, f)
  visualize.progress(fcm, data)
  print(paste("PC:", fcm.PC(fcm, data)))
  print(paste("Xie-Beni:", fcm.XB(fcm, data)))
}

test.online <- function(n, f = 2) {
  fcm <- fcm.init(n, f, 2, data[1:20,])
  fcm <- fcm.online.run(fcm, data)
  visualize.progress(fcm, data)
  print(paste("PC:", fcm.PC(fcm, data)))
  print(paste("Xie-Beni:", fcm.XB(fcm, data)))
}



# # one step visualization
# ensemble <- fcm.ensemble.init(pattern_length = ncol(data), initial_data = data[1:20,], n_fcm = 2, n_cascades = 3)
# for (i in 1 : nrow(data)) {
#   ensemble <- fcm.ensemble.online.run(ensemble, data[i,], k = i)
#   old.par <- par(mfrow=c(3, 2))
#   visualize.progress(ensemble$cascades[[1]]$fcms[[1]], data[1:i,])
#   visualize.progress(ensemble$cascades[[1]]$fcms[[2]], data[1:i,])
#   visualize.progress(ensemble$cascades[[2]]$fcms[[1]], data[1:i,])
#   visualize.progress(ensemble$cascades[[2]]$fcms[[2]], data[1:i,])
#   visualize.progress(ensemble$cascades[[3]]$fcms[[1]], data[1:i,])
#   visualize.progress(ensemble$cascades[[3]]$fcms[[2]], data[1:i,])
#   par(old.par)
#   readline(prompt="Press [enter] to continue")
# }
# 
