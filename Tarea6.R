# Parametros del LCG
a <- 1664525
c <- 1013904223
m <- 2^32
n <- 1000

# Semilla
x <- numeric(n)
x[1] <- 12345

# Generación
for (i in 2:n){
  x[i] <- (a * x[i-1] + c) %% m
}

# Normalización [0,1)
u <- x/m

# Visualización
library(ggplot2)
ggplot(data.frame(u), aes(x=u)) +
  geom_histogram(bins=30, fill="steelblue", color="black") +
  labs(title="Distribución de números pseudoaleatorios (LCG)")



#############
set.seed(123, kind="Mersenne-Twister")

u_mt <- runif(1000)

library(ggplot2)
ggplot(data.frame(u_mt), aes(x=u_mt)) +
  geom_histogram(bins=30, fill="orange", color="black") +
  labs(title="Distribución de números pseudoaleatorios (Mersenne Twister)")