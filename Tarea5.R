set.seed(123)

n_sizes <- c(10, 30, 50, 80, 100, 1000)

B <- 500

for (n in n_sizes) {
  medias <- replicate(B, mean(rexp(n, rate = 1)))
  
  sw <- shapiro.test(medias)
  cat("Tamaño muestral:", n, "\n")
  cat("p-valor Shapiro-Wilk:", sw$p.value, "\n\n")
}
