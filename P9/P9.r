n <- 50
p <- data.frame(x = rnorm(n), y = rnorm(n), carga = rnorm(n), masa = rnorm(n))
carga <- FALSE
fg <- FALSE
normalizar <- function(v){
  max <- max(v)
  min <- min(v)
  V <- (v - min) / (max - min)
  return(V)
}
p$x <- normalizar(p$x) # ahora son de 0 a 1
p$y <- normalizar(p$y) # las y tambien
p$masa <- normalizar(p$masa) # la masa es de 0 a 1
cargamax <- max(p$c)
cargamin <- min(p$c)
p$carga <- 2 * (p$carga - cargamin) / (cargamax - cargamin) - 1 # cargas son entre -1 y 1
p$g <- round(5 * p$carga) # coloreamos segun la carga a 11 niveles de -5 a 5
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
png("p9i.png")
library(lattice)
xyplot(y ~ x, group=g, data=p, auto.key=list(space="right"),
       xlab="X", ylab="Y", main="Part\u{00ed}culas generadas",
       par.settings = list(superpose.symbol = list(pch = 15, cex = 1.5,
                                                   col = colores)))
graphics.off()

eps <- 0.001
fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  cargai <- p[i,]$carga
  masai <- p[i,]$masa
  fx <- 0
  fy <- 0
  for (j in 1:n) {
    cargaj <- p[j,]$carga
    masaj <- p[j,]$masa
    dirc <- (-1)^(1 + 1 * (cargai * cargaj < 0))
    dirm <- (-1)^(1 + 1 * (masai < masaj))
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factorc <- dirc * abs(cargai - cargaj) / (sqrt(dx^2 + dy^2) + eps)
    factorm <- dirm * (masai * masaj) / (dx^2 + dy^2 + eps)
    if (carga){
      fx <- fx - dx * factorc
      fy <- fy - dy * factorc
    } else if (fg) {
      fx <- fx - dx * factorm
      fy <- fy - dy * factorm
    }
    else {
      fx <- fx - dx * (factorc + factorm)
      fy <- fy - dy * (factorc + factorm)
    }
  }
  return(c(fx, fy))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
system("rm -f p9_t*.png") # borramos anteriores en el caso que lo hayamos corrido
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
  tl <- paste("0", tl, sep="")
}
png(paste("p9_t", tl, ".png", sep=""))
plot(p$x, p$y, col=colores[p$g+6], pch=15, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
     main="Estado inicial", xlab="X", ylab="Y")
graphics.off()
for (iter in 1:tmax) {
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0)
  p$v <- foreach(i = 1:n, .combine=c) %dopar% (p[i,]$x + p[i,]$y)
  tl <- paste(iter, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  png(paste("p9_t", tl, ".png", sep=""))
  plot(p$x, p$y, col=colores[p$g+6], pch=15, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
       main=paste("Paso", iter), xlab="X", ylab="Y")
  graphics.off()
}
stopImplicitCluster()

p$v <- aux

library(ggplot2)

#p$v <- normalizar(p$v)

tema <- theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)

ggplot(p, aes(x= masa, y= carga, colour = v)) + geom_point(aes(colour = v)) + geom_point(size = 5) + tema +
  labs(x = "Masa", y = "Magnitud de la carga", colour = "Velocidad") + geom_line() + scale_color_gradient(low = "yellow", high ="red")
ggsave("resultados.eps")