library("readr")
library(reshape2)
library(ggplot2)
suppressMessages(library(doParallel))

primo <- function(n) { #Detectar primos
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  return(TRUE)
}

noPrimo <- function(n) { #Detectar no primos
  if (n == 1 || n == 2) {
    return(FALSE)
  }
  if (n %% 2 == 0) {
    return(TRUE)
  }
  return(FALSE)
}

Primos <- read.table("primes1.txt", skip = 0) #Tomar numeros primos de 6 digitos
Primos <- c(t(Primos))
Cores <- seq(1, detectCores() - 1, 1)
Proporciones <- c(25, 50, 75) #Quantil
sumatoria <- data.frame()
NumP <- 500

for(nucleo in Cores){
  registerDoParallel(makeCluster(nucleo))
  for(p in Proporciones) { #proporciones de los numeros primos 
    muestraPrimos <- sample(Primos, NumP, replace = FALSE)
    if (p == 50){
      muestraPrimos <- sample(muestraPrimos, length(muestraPrimos)/2)
      muestraNP <- foreach(n = muestraPrimos, .combine = c) %dopar% noPrimo(n)
    } else {
      m <- sample(muestraPrimos, length(muestraPrimos)*(p/100))
      cont <- 0
      for (a in muestraPrimos){
        if(cont < length(m)){
          if (a %in% m){
          } else {
            muestra <- muestra[muestra != a]
            cont <- cont + 1 
          }
        }
      }
      muestraNP <- foreach(n = m, .combine = c) %dopar% noPrimo(n)
    }
    combinacion <- sort(sample(c(muestra, muestraNP))) #unimos el vector de primos y no primos
    original <- combinacion
    invertido <- rev(combinacion)
    rep <- 20
    ot <-  numeric()
    it <-  numeric()
    at <-  numeric()
    for (i in 1:rep) {
      ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
      it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
      at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
    }
    sumaT <- rbind(summary(ot), summary(it), summary(at))
    colnames(sumaT) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max")
    sumaT <- cbind(sumaT, "Nucleos" = rep(nucleo, nrow(sumaT)))
    sumaT <- cbind(sumaT, "Orden" = rep(1:3, nrow(sumaT)/3))
    sumaT <- cbind(sumaT, "Proporcion" = rep(p/100, nrow(sumaT)))
    sumatoria <- rbind(sumatoria, sumaT)
    stopImplicitCluster() 
  }
}
print(sumatoria)
sumatoria <- melt(sumatoria, id.vars = c("Nucleos", "Proporcion", "Orden"))
gg <- ggplot(sumatoria, aes(x = as.factor(Nucleos), y = value, group = interaction (Nucleos, Orden), fill = as.factor(Orden))) +
  geom_bar(stat="identity", position="dodge") + theme_gray(base_size = 14) + labs(x = "Cantidad de n\u{FA}cleos", 
                                                     y = "Tiempo de ejecuci\u{F3}n en segundos", 
                                                     fill = "Tipo de \n ordenamiento") 
gg <- gg + scale_fill_manual( values = c("orange", "blue", "green"), aesthetics = "fill") + 
  facet_grid(~Proporcion, scale="fixed") + theme(legend.title.align=0.5) 
gg
ggsave("grafica.png")