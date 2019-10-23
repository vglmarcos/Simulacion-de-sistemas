library(parallel)
library(ggplot2)
library(reshape2)
suppressMessages(library("sna"))
dim <- 30
num <- dim^2
lim <- 100
repeticiones <- 30
probabilidades <- seq(0,1,0.10)

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim), max(columna - 1, 1): min(columna + 1, dim)]
  return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}

resultados <- data.frame()

for(p in probabilidades){
  iteraciones <- list()
  actual <- matrix(1 * (runif(num) < p) , nrow=dim, ncol=dim)
  for (rep in 1:repeticiones){
    if(sum(actual) == 0){ #todos se generaron muertos
      iteraciones <- c(iteraciones, 0)
      iteraciones <- unlist(iteraciones)
    } else if(sum(actual) == num){ #todos estan vivos
      iteraciones <- c(iteraciones , 0)
      iteraciones <- unlist(iteraciones)
    } else{
      png(paste("p2_Rep",rep, "_t0.png", sep =""))
      plot.sociomatrix(actual, diaglab = FALSE, main = "Inicio")
      graphics.off()
      for(iteracion in 1:lim) {
        clusterExport(cluster, "actual")
        siguiente <- parSapply(cluster, 1:num, paso)
        if(sum(siguiente) == 0) { # todos murieron
          #print("Ya no queda nadie vivo.")
          break;
        }
        actual <- matrix(siguiente, nrow=dim, ncol = dim, byrow = TRUE)
        salida <- paste("p2_Rep",rep, "_t", iteracion, ".png", sep="")
        tiempo <- paste("Paso", iteracion)
        png(salida)
        plot.sociomatrix(actual, diaglab = FALSE, main = tiempo)
        graphics.off()
      }
      iteraciones <- c(iteraciones, iteracion)
      iteraciones <- unlist(iteraciones)
    }
  }
  resultados <- rbind(resultados, iteraciones)
}
stopCluster(cluster)

colnames(resultados) <- as.character(1:repeticiones) 
resultados <- cbind("Prob" = probabilidades, resultados)
resultados$probabilidades <- as.factor(resultados$probabilidades)
print(resultados)

resultados <- melt(resultados, id.vars = "Prob")

ggplot(data=resultados, aes(x=Prob, y=resultados$value, fill=Prob)) + 
  geom_bar(stat="identity", position="dodge") +
  theme_gray(base_size = 14) + xlab("Probabilidad") + ylab("Iteraciones") 
ggsave("GraficaP2.png")