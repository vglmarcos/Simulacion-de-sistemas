parte <- function() {
  valores <- generador(pedazo)
  return(sum(valores >= desde & valores <= hasta))
}
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
g <- function(x) { return((2 / pi) * f(x)) }
suppressMessages(library(distr))
suppressMessages(library(doParallel))
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)
generador  <- r(AbscontDistribution(d = g)) # creamos un generador
inicio <- -6
final <- -inicio
paso <- 0.25
desde <- 3
hasta <- 7
x <- seq(inicio, final, paso)

datos <- data.frame()
cuantos <- 500 #Cuantos valores se manejan
replicas <- 30
pedazos <- c(500,1000,1500,2000,5000,10000)
valor <- 0.0488341

decimas <- data.frame()
lista <- NULL

for (pedazo in pedazos) {
  for (rep in 1:replicas) {
    montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
    integral <- sum(montecarlo) / (cuantos * pedazo)
    resultado <- (pi / 2) * integral
    lista <- c(lista, resultado)
    error <- valor - resultado
    resultados <- c(rep, pedazo, valor, resultado, error)
    datos <- rbind(datos, resultados)
  }
  print(length(lista))
  decimas <- rbind(decimas, c(min(lista), max(lista), pedazo))
  lista <- NULL
}
stopCluster(cluster)

colnames(datos) <- c("replica", "muestra", "valor", "resultado", "error" )
colnames(decimas) <- c("minimo", "maximo", "muestra")

png("error.png")
boxplot(data=datos, error~muestra, xlab="Tamaño de muestra", ylab="Error", main="", col = "orange")
abline(h=0, col="red", pch=20)
graphics.off()

png("aproximacion.png")
boxplot(data=datos, resultado~muestra, xlab="Tamaño de muestra", ylab="Resultados", main="", col = "blue")
abline(h=valor, col="red", pch=20)
graphics.off()