g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -3
high <- -low
step <- 0.25
replicas <- 15
coordenadas <- data.frame("X"=0, "Y"=0, "T"=0, "R" = 0)
animacion <- FALSE

replica <- function(t, coor) {
  currX <- runif(1, low, high) 
  currY <- runif(1, low, high)
  bestX <- currX
  bestY <- currY
  for (tiempo in 1:t) {
    deltaX <- runif(1, 0, step)
    deltaY <- runif(1, 0, step)
    izq <- currX - deltaX
    der <- currX + deltaX
    abajo <- currY - deltaY
    arriba <- currY + deltaY
    while(sum(c(izq, der, abajo, arriba) < low) !=0 || sum(c(izq, der, abajo, arriba) > high) != 0){
      currX <- runif(1, low, high) 
      currY <- runif(1, low, high)
      deltaX <- runif(1, 0, step)
      deltaY <- runif(1, 0, step)
      izq <- currX - deltaX
      der <- currX + deltaX
      abajo <- currY - deltaY
      arriba <- currY + deltaY 
    }
    if (g(izq, currY) > g(der, currY)) {
      currX <- izq
    } else {
      currX <- der
    }
    if (g(currX, currY) > g(bestX, bestY)) {
      bestX <- currX
      bestY <- currY
    }
    if (g(currX, abajo) > g(currX, arriba)) {
      currY <- abajo
    } else {
      currY <- arriba
    }
    if (g(currX, currY) > g(bestX, bestY)) {
      bestX <- currX
      bestY <- currY
    }
    if (g(izq, arriba) > g(der, arriba)) {
      currX <- izq
      currY <- arriba
    } else {
      currX <- der
      currY <- arriba
    }
    if (g(currX, currY) > g(bestX, bestY)) {
      bestX <- currX
      bestY <- currY
    }
    if (g(izq, abajo) > g(der, abajo)) {
      curryX <- izq
      currY <- abajo
    } else {
      curryX <- der
      currY <- abajo
    }
    if (g(currX, currY) > g(bestX, bestY)) {
      bestX <- currX
      bestY <- currY
    }
    b = data.frame("X" = bestX, "Y" = bestY, "T" = tiempo, "R" = i)
    coor <- rbind(coor, b)
  }
  best = c(bestX, bestY)
  return(coor)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
values <- seq(low, high, by = step)
x <- rep(values, each = length(values))
y <- rep(values, length(values))
z <- foreach(i = x, j = y, .combine=c) %dopar% g(i,j)
resultados2 <- data.frame(x, y, z)
tmax <- 10^4
resultados <- foreach(i = 1:replicas, .combine="rbind") %dopar% replica(tmax, coordenadas)
resultados <- data.frame(resultados)
stopImplicitCluster()
resultados <- resultados[!(resultados$T == 0),]
library(ggplot2)
for (i in 1:tmax){
  sS <- subset(resultados, T == i)
  if(animacion){
    ggsave(paste("paso", i, ".png", sep=""))
    ggplot(resultados2, aes(x = x, y = y)) + geom_tile(aes(fill=z)) + ggtitle(paste("Paso ", i, sep="")) +
      scale_fill_gradient2(name = "",low = "orange", mid = "yellow", high = "red", midpoint=(min(z)+max(z))/2, breaks = c(seq(floor(min(z)), 0, by = 0.5))) +
      scale_x_continuous("", breaks = seq(low, high, by = 1)) + scale_y_continuous("", breaks = seq(low, high, by = 1)) +
      guides(fill = guide_colorbar(barwidth = 1, barheight = 20)) + geom_point(data = sS, aes(x= X, y= Y, color = as.factor(R)), size = 4, shape = 17, stroke = 2) + scale_color_hue(l=80, c=150, guide = FALSE) +
      theme_minimal(base_size = 14)
    graphics.off()
  }
}

recorrido <- ggplot(resultados2, aes(x = x, y = y)) + geom_tile(aes(fill=z)) +
              scale_fill_gradient2(name = "",low = "orange", mid = "yellow", high = "red", midpoint=(min(z)+max(z))/2, breaks = c(seq(floor(min(z)), 0, by = 0.5))) +
              scale_x_continuous("", breaks = seq(low, high, by = 1)) + scale_y_continuous("", breaks = seq(low, high, by = 1)) +
              guides(fill = guide_colorbar(barwidth = 1, barheight = 20)) + geom_point(data = sS, aes(x= X, y= Y, color = as.factor(R)), size = 4, shape = 17, stroke = 2) + scale_color_hue(l=80, c=150, guide = FALSE) +
              theme_minimal(base_size = 14)


ggsave(paste("paso", i, ".eps", sep=""))
recorrido
graphics.off()

ggsave(paste("paso", i, ".png", sep=""))
recorrido
graphics.off()