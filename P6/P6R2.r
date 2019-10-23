l <- 1.5
n <- 50 #numero de agentes
pi <- 0.05 #probabilidad de infeccion al inicio
pr <- 0.02 #probabilidad de recuperacion
PV <- seq(0,1,0.1) #probabilidad de vacunacion al inicio
r <- 0.1 #umbral
t <- 1
pasosv <- seq(5,40,1)
replicas <- 50
Rvacunas <- data.frame()
dibujarSistema <- FALSE
ggplot <- TRUE
ka <- 6
ra <- 0.8 #distancia de amigo
pa <- 0.6 #probabilidad de amistad

for(pv in PV){
  for(rep in 1:replicas){
    agentes <- data.frame(x = double(), y = double(), pmx = double(), pmy = double(), 
    dx = double(), py = double(), estado  = character(), pasos = integer(), amigo = NULL)
    for (i in 1:n) { #Genero estados iniciales de los agentes
      if(runif(1) < pv){ #vacunados al inicio con probabilidad pv
        e <- "R"
      } else if(runif(1) < pi){
        e <- "I"
      } else{
        e <- "S"
      }

      #asignar amistad
      if(runif(1) < pa){
          amistad <- TRUE
      } else{
          amistad <- FALSE
      }

      pasos <- sample(pasosv,1)
      xc <- runif(1, 0, l)
      yc <- runif(1, 0, l)
      px <- runif(1,0,l)
      py <- runif(1,0,l)
      vx <- (px - xm) / pasos
      vy <- (py - ym) / pasos
      
      agentes <- rbind(agentes, data.frame(x = xc, y = yc, dx = vx, dy = vy, pmx = px, 
                                           pmy = py, estado = e, pasos = pasos, amigo = amistad))
      
      levels(agentes$estado) <- c("S", "I", "R")
    }
    
    epidemia <- integer()
    mayor <- 0
    actual <- 0
    iter <- ka
    
    while(TRUE) { #poner while para terminar la epidemia cuando todos esten enfermos y recuperados
      infectados <- dim(agentes[agentes$estado == "I",])[1]
      epidemia <- c(epidemia, infectados)
      if (infectados == 0) {
        break
      }
      contagios <- rep(FALSE, n)
      for (i in 1:n) { # posibles contagios
        a1 <- agentes[i, ]
        if (a1$estado == "I") { # desde los infectados
          for (j in 1:n) {
            if (!contagios[j]) { # aun sin contagio
              a2 <- agentes[j, ]
              if (a2$estado == "S") { # hacia los susceptibles
                dx <- a1$x - a2$x
                dy <- a1$y - a2$y
                d <- sqrt(dx^2 + dy^2)
                #bajar la velocidad a la mitad cuando se encuentre a un amigo
                if(d < ra && a1$amigo == a2$amigo){
                  if(iter == ka){
                    dx <- dx / 2
                    dy <- dy / 2
                    iter <- iter - 1
                  } else if(iter > 0){
                    iter <- iter - 1
                  } else{ #regresa a su velocidad normal
                    dx <- dx * 2
                    dy <- dy * 2
                    iter <- ka
                  }
                }
                if (d < r) { # umbral
                  p <- (r - d) / r
                  if (runif(1) < p) {
                    contagios[j] <- TRUE
                  }
                }
              }
            }
          }
        }
      }
      for (i in 1:n) { # movimientos y actualizaciones
        a <- agentes[i, ]
        if (contagios[i]) {
          a$estado <- "I"
        } else if (a$estado == "I") { # ya estaba infectado
          if (runif(1) < pr) {
            a$estado <- "R" # recupera
          }
        }
        a$x <- a$x + a$dx
        a$y <- a$y + a$dy
        if(a$pasos != 1){
            a$pasos = a$pasos - 1
        } else {
            a$pasos <- sample(pasosv,1)
            a$x <- a$pmx
            a$y <- a$pmy
            a$pmx <- runif(1,0,l)
            a$pmy <- runif(1,0,l)
            a$dx <- (a$pmx - a$x) / pasos
            a$dy <- (a$pmy - a$y) / pasos
        }
        agentes[i, ] <- a
      }
      if(dibujarSistema){
        aS <- agentes[agentes$estado == "S",]
        aI <- agentes[agentes$estado == "I",]
        aR <- agentes[agentes$estado == "R",]
        tl <- paste(tiempo, "", sep="")
        while (nchar(tl) < digitos) {
          tl <- paste("0", tl, sep="")
        }
        salida <- paste("p6_t", t, ".png", sep="")
        tiempo <- paste("Paso", t)
        png(salida)
        plot(l, type="n", main=tiempo, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
        if (dim(aS)[1] > 0) {
          points(aS$x, aS$y, pch=15, col="chartreuse3", bg="chartreuse3")
        }
        if (dim(aI)[1] > 0) {
          points(aI$x, aI$y, pch=16, col="firebrick2", bg="firebrick2")
        }
        if (dim(aR)[1] > 0) {
          points(aR$x, aR$y, pch=17, col="goldenrod", bg="goldenrod")
        }
        graphics.off()
      }
      infe <- dim(agentes[agentes$estado == "I",])[1]
      recu <- dim(agentes[agentes$estado == "R",])[1]
      if(recu > infe){
        break
      }
      t <- t + 1
    }
    maximo_infectados <- max(epidemia)
    porcentaje <- 100 * maximo_infectados / n
    Rvacunas <- rbind(Rvacunas, c(pv, rep, maximo_infectados, porcentaje))
    print(pv) 
  }
}
colnames(Rvacunas) <- c("Probabilidad", "Replicas", "Max_Infectados", "Porcentaje")
print(Rvacunas)
if(ggplot){
  #Grafica con ggplot violin
  library(ggplot2)
  Rvacunas$Probabilidad <- as.factor(Rvacunas$Probabilidad)
  tema <- theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )
  #png
  p <- ggplot(Rvacunas, aes(x = Probabilidad, y = Porcentaje, fill = Probabilidad)) + geom_violin()
  p <- p + geom_violin(scale = "width", alpha = 0.6) + geom_violin(trim = F) + geom_boxplot(width=0.3, alpha=0.8)
  p <- p + labs(x="Probabilidades de vacunaci\u{F3}n", y = "Porcentaje m\u{E1}ximo de infectados") + tema
  ggsave("ResultadosP6R2.png")
  #eps
  e <- ggplot(Rvacunas, aes(x = Probabilidad, y = Porcentaje, fill = Probabilidad)) + geom_violin()
  e <- e + geom_violin(scale = "width") + geom_violin(trim = F) + geom_boxplot(width=0.3)
  e <- e + labs(x="Probabilidades de vacunaci\u{F3}n", y = "Porcentaje m\u{E1}ximo de infectados") + tema
  ggsave("ResultadosP6R2.eps")
} else{
  #Grafica con boxplot
  png("boxplotP6R2.png")
  probabilidades <- Rvacunas$Probabilidad
  porcentaje_maximos <- Rvacunas$Porcentaje
  boxplot(porcentaje_maximos~probabilidades, col = "orange", border="brown", xlab = "Probabilidades de vacunaci\u{F3}n", 
          ylab = "Porcentaje m\u{E1}ximo de infectados")
  graphics.off()
}