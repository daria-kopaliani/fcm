visualize.clusters <- function(fcm, data, cell.side = 0.05) {
  
  colors <- topo.colors(nrow(fcm$centers))
  visualize.lattice(fcm, cell.side, colors)
  
  U <- fcm.membership.values(data, fcm$centers, fcm$fuzzifier)
  for (i in 1 : nrow(data)) {
    cluster <- which.max(U[i,])
    points(data[i, 1], data[i, 2], pch = 21, bg = colors[cluster])
    segments(data[i, 1], data[i, 2], fcm$centers[cluster, 1], fcm$centers[cluster, 2], col = "gray22")
    if (max(U[i,]) < 0.5) {
      points(data[i, 1], data[i, 2], pch = 11, col = "lightpink2")
    } 
  }
}

visualize.lattice <- function(fcm, cell.side = 0.05, colors = NULL) { 
  
  if (is.null(colors)) {
    colors <- topo.colors(nrow(fcm$centers))
  }
  
  plot(1, type = "n", xlim=c(0, 1), ylim=c(0, 1), yaxt="n", ylab="", xaxt="n", xlab="")
  for (x in seq(0, 1 - cell.side, cell.side)) {
    for (y in seq(0, 1 - cell.side, cell.side)) {      
      sample <- t(as.matrix(c(x, y)))
      U <- fcm.membership.values(sample, fcm$centers, fcm$fuzzifier)
      color <- numeric(3)
      for (j in 1 : nrow(fcm$centers)) {
        color <- color + U[j] * (col2rgb(colors[j]) / 256)
      }
      rect(x, y, x + cell.side - 0.0001, y + cell.side - 0.0001, col = rgb(color[1], color[2], color[3]), border = "white")
    }
  }
  
  for (j in 1 : nrow(fcm$centers)) {
    points(fcm$centers[j, 1], fcm$centers[j, 2], pch = 25, bg = colors[j])
  }
}

visualize.progress <- function(fcm, data, cell.side = 0.05) {
  
  colors <- topo.colors(nrow(fcm$centers))
  visualize.lattice(fcm, cell.side, colors)
  
  for (j in 1 : nrow(fcm$centers)) {
    points(fcm$centers[j, 1], fcm$centers[j, 2], pch = 25, bg = colors[j])
  }
  
  U <- fcm.membership.values(data, fcm$centers, fcm$fuzzifier)
  for (i in 1 : nrow(data)) {
    cluster <- which.max(U[i,])
    points(data[i, 1], data[i, 2], pch = 21, bg = colors[cluster])
    segments(data[i, 1], data[i, 2], fcm$centers[cluster, 1], fcm$centers[cluster, 2], col = "gray22")
    if (max(U[i,]) < 0.5) {
      points(data[i, 1], data[i, 2], pch = 11, col = "lightpink2")
    } 
  }
}