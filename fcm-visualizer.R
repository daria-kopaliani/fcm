visualize <- function(fcm, data) {
  
  colors <- topo.colors(nrow(fcm$centers))
  
  title <- paste("PC:", fcm.PC(fcm), "Xie-Beni:", fcm.XB(fcm, data))
  plot(1, type = "n", xlim=c(0, 1), ylim=c(0, 1), main = title, yaxt="n", ylab="", xaxt="n", xlab="")
  size = 0.05
  for (x in seq(0, 1 - size, size)) {
    for (y in seq(0, 1 - size, size)) {      
      membership_value <- numeric(nrow(fcm$centers))
      sample <- c(x, y)
      for (j2 in 1 : nrow(fcm$centers)) {
        z <- 0
        for (j1 in 1 : nrow(fcm$centers)) {
          z <- z + (vector.norm(sample - fcm$centers[j2, ]) / vector.norm(sample - fcm$centers[j1, ])) ^ (fcm$fuzzifier - 1)  
        }
        membership_value[j2] <- 1/z
      }
      color <- numeric(3)
      for (j in 1 : nrow(fcm$centers)) {
        color <- color + membership_value[j] * (col2rgb(colors[j]) / 255)
      }
      rect(x, y, x + size - 0.0001, y + size - 0.0001, col = rgb(color[1], color[2], color[3]), border = "white")
    }
  }
  
  for (j in 1 : nrow(fcm$centers)) {
    points(fcm$centers[j, 1], fcm$centers[j, 2], pch = 25, bg = colors[j])
  }
  
  for (i in 1 : nrow(data)) {
    uu <- numeric(nrow(fcm$centers))
    for (j in 1 : nrow(fcm$centers)) {
      z <- 0
      for (k in 1 : nrow(fcm$centers)) {
        z <- z + (vector.norm(data[i, ] - fcm$centers[j, ]) / vector.norm(data[i, ] - fcm$centers[k, ])) ^ (fcm$fuzzifier - 1)  
      }
      uu[j] <- (1 / z)      
    }
    cluster <- which.max(uu)
    points(data[i, 1], data[i, 2], pch = 21, bg = colors[cluster])
    segments(data[i, 1], data[i, 2], fcm$centers[cluster, 1], fcm$centers[cluster, 2], col = "gray22")
    if (max(uu) < 0.7) {
      points(data[i, 1], data[i, 2], pch = 11, col = "gray50")
    } 
  }
}

