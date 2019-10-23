MB <- function(i){ #MB "Movimiento Browniano"    
  repetir <- 30 #repetimos el experimento para poder comparar mejores resultados
  
  library(parallel) #Agregamos parallel para poder utilizar clusters, los cuales paralerizaran las cosas
  
  cluster <- makeCluster(detectCores()-1) #asignamos la cantidad de nucleos que queremos que use, nucleos totales - 1
  
  datos <-  data.frame()
  
  for (dimension in 1:8) {
    duracion<-2^(i)
    clusterExport(cluster, "duracion") #Creamos un cluster donde le asignamos la duracion y dimension
    clusterExport(cluster, "dimension")
    resultado <- parSapply(cluster, 1:repetir,
                           function(r) {
                             pos <- rep(0, dimension) #dimension que se usara
                             cont <- 0 #inicializamos un contador en 0
                             for (t in 1:duracion) {
                               cambiar <- sample(1:dimension, 1)#escojemos al azar en que dimension queremos cambiar la posicion
                               cambio <- 1
                               if (runif(1) < 0.5) { #avanza aleatoriamente, ya sea 1 o -1
                                 cambio <- -1
                               }
                               pos[cambiar] <- pos[cambiar] + cambio #realizamos el cambio en la posicion anteriormente elegida
                               if (all(pos==0)) { #Si llega al origen, sumara +1 al contador para poder sacar las probabilidades
                                 cont <- cont + 1 
                                 #break; #rompemos la caminata 
                               }
                             }
                             return(cont/duracion) #Nos ayuda a saber la probabilidad que tuvo la particula para regresar a su origen
                           })
    datos <- rbind(datos, resultado) #pegamos la informacion de datos y resultados en filas, como un dataframe
  }
  png(sprintf("%sPasos.png",2^i))
  boxplot(data.matrix(datos), use.cols=F, 
          xlab="Dimensi\u{F3}n", ylab="Probabilidad")
  stopCluster(cluster)
  graphics.off()
}

for (i in 6:12) { #realizamos el experimento cambiando la duracion de 2^6 hasta 2^12
  MB(i)
}