rm(list = ls())
library(testit)
library(parallel)

knapsack <- function(cap, peso, valor) {
  inicio <- as.numeric(proc.time()[3])
  n <- length(peso)
  pt <- sum(peso) # deben ser enteros en este caso
  
  assert(n == length(valor))
  
  vt <- sum(valor) # pueden ser lo que sea
  
  if (pt < cap) { # cabe todo
    final <- as.numeric(proc.time()[3])
    tiempo <- final - inicio
    return(c(tiempo, vt))
  } else {
    filas <- cap + 1 # una para cada posible peso acumulado desde cero hasta cap
    cols <- n + 1 # una para cada objeto y una extra al inicio para no llevar nada
    
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) # al inicio todo vale negativo infinito
    
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 # todas las filas tienen un cero al inicio (no llevar nada da cero valor)
    }
    
    rownames(tabla) <- 0:cap # filas corresponden a pesos acumulados posibles
    colnames(tabla) <- c(0, valor) # columnas corresponden a objetos considerados
    
    for (objeto in 1:n) { # consideremos a cada objeto por turno
      p <- peso[objeto] # tomamos su peso a una variable
      v <- valor[objeto] # y su valor a otra variable
      
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - p
        tabla[acum, objeto + 1] <- tabla[acum, objeto]
        
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + v)
        } 
      }
      
    }
    final <- as.numeric(proc.time()[3])
    tiempo <- final - inicio
    return(c(tiempo, max(tabla)))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1, media, desv))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

generador.valores.co <- function(pesos) {
  valor <- double()
  n <- length(pesos)
  p <- 0.4
  for(i in 1:n){
    if(runif(1) < p){ #se agrega un poco de ruido
      if (runif(1) < p){
        valor[i] = pesos[i] * 5.6546 + pesos[i] * runif(1)
      } else{
        valor[i] = pesos[i] * 5.6546 - pesos[i] * runif(1)
      }
    } else{
      valor[i] = pesos[i] * 5.6546
    }
  }
  return(valor)
}

generador.valores.in <- function(pesos) {
  valor <- double()
  n <- length(pesos)
  p <- 0.4
  for(i in 1:n){
    if(runif(1) < p){ #se agrega un poco de ruido
      if (runif(1) < p){
        valor[i] = ((max(pesos)*n*2) / pesos[i]) + max(pesos) * runif(1)
      } else{
        valor[i] = ((max(pesos)*n*2) / pesos[i]) - max(pesos) * runif(1)
      }
    } else{
      valor[i] = ((max(pesos)*n*2) / pesos[i])
    }
  }
  return(valor)
}

desde <- 100
hasta <- 500
pasos <- 100
N <- seq(from = desde, to = hasta, by = pasos)

resultados <- data.frame(tiempo = double(), valor = double(), tipo = character(), tipo_valor = character(), n = integer())

nombres <- c("no correlacional", "correlacional", "inversamente correlacional")

for (n in N) {  
  init <- 200
  pm <- 0.05
  rep <- 50
  tmax <- 50

  cluster <- makeCluster(detectCores() - 1)

  pesos <- generador.pesos(n, 15, 80)
  valores1 <- generador.valores(pesos, 10, 500)
  valores2 <- generador.valores.co(pesos)
  valores3 <- generador.valores.in(pesos)
  Valores <- valores <- data.frame(valores1 = valores1, valores2 = valores2, valores3 = valores3)
  capacidad <- round(sum(pesos) * 0.65)

  png(paste("no_correlacional_", n, ".png", sep = ""))
  plot(pesos, valores1)
  graphics.off()
  png(paste("correlacional_", n, ".png", sep = ""))
  plot(pesos, valores2)
  graphics.off()
  png(paste("in_correlacional_", n, ".png", sep = ""))
  plot(pesos, valores3)
  graphics.off()

  g = 1

  for(valores in Valores){
    datos <- knapsack(capacidad, pesos, valores)
    optimo <- datos[2]
    tiempo <- datos[1]

    resultados <- rbind(resultados, data.frame(tiempo = tiempo, valor = optimo, tipo = "secuencial", tipo_valor = paste(nombres[g]), n = n))
    error <- data.frame(valor = double(), error = double(), tipo = character(), n = integer())

    clusterExport(cluster, "n")
    clusterExport(cluster, "pesos")
    clusterExport(cluster, "valores")
    clusterExport(cluster, "capacidad")

    pobl <- t(parSapply(cluster, 1:init, function(i) {
    return(round(runif(n)))
    }))
            
    p <- as.data.frame(pobl)
    tam <- dim(p)[1]      
    assert(tam == init)     
    mejores <- double()

    inicio <- as.numeric(proc.time()[3])
          
    for (iter in 1:tmax) {
      p$obj <- NULL
      p$fact <- NULL
              
      clusterExport(cluster, "p")
      clusterExport(cluster, "tam")
              
      probabilid <- runif(tam) < pm
      mutados <- which(probabilid %in% TRUE)
              
      mutaciones <- t(parSapply(cluster, mutados, function(i) {
        pos <- sample(1:n, 1)
        mut <- p[i,]
        mut[pos] <- (!p[i,][pos]) * 1
        return(as.numeric(mut))
      }))
              
      hijos <- matrix(parSapply(cluster, 1:rep, function(i) {
        padres <- sample(1:tam, 2, replace = FALSE)
        pos <- sample(2:(n-1), 1)
        x <- p[padres[1],]
        y <- p[padres[2],]
        xy <- c(x[1:pos], y[(pos+1):n])
        yx <- c(y[1:pos], x[(pos+1):n])
        return(as.numeric(c(xy, yx)))
      }), ncol = n, byrow = TRUE)
              
      p <- rbind(p, mutaciones, hijos)
      tam <- dim(p)[1]
              
      clusterExport(cluster, "p")
      clusterExport(cluster, "tam")
              
      p$obj <- parSapply(cluster, 1:tam, function(i) {
        return(sum(p[i,] * valores))
      })
              
      p$fact <- parSapply(cluster, 1:tam, function(i) {
        return(sum(p[i,] * pesos) <= capacidad)
      })
              
      mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
              
      p <- p[mantener,]
      tam <- dim(p)[1]
              
      assert(tam == init)
              
      factibles <- p[p$fact == TRUE,]
      mejor <- max(factibles$obj)
      mejores <- c(mejores, mejor)
    }
    fin <- as.numeric(proc.time()[3])
    transcurrido <- fin - inicio
    resultados <- rbind(resultados, data.frame(tiempo = transcurrido, valor = max(mejores), tipo = "paralelo", tipo_valor = paste(nombres[g]), n = n))
    error <- rbind(error, data.frame(valor = max(mejores), error = (optimo - max(mejores)) / optimo), tipo = paste(nombres[g]), n = n)
    png(paste("n_", n, "tipo_", nombres[g], ".png", sep = ""), width=600, height=300)
    plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
    points(1:tmax, mejores, pch=15)
    abline(h=optimo, col="green", lwd=3)
    graphics.off()
    g = g + 1
  }
}
stopCluster(cluster)

ggplot(resultados, aes(x = valor, y = tiempo, fill = valor)) + geom_line(aes(colour = factor(tipo))) + facet_grid(. ~ n)