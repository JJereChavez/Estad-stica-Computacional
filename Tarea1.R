# Creamos datos los cuales son (100 estudiantes, 6 materias)
datos <- data.frame(
  matematicas = c(85, 90, 78, 92, 88, 76, 94, 82, 87, 91, 79, 86, 93, 84, 89, 
                  77, 95, 83, 88, 90, 75, 92, 86, 89, 91, 78, 85, 93, 87, 84,
                  88, 92, 80, 86, 94, 82, 89, 87, 91, 85, 79, 93, 86, 88, 90,
                  76, 94, 83, 87, 92, 81, 89, 85, 90, 88, 77, 93, 86, 84, 91,
                  82, 88, 95, 87, 90, 79, 85, 92, 86, 89, 83, 91, 88, 84, 93,
                  80, 87, 94, 85, 90, 78, 92, 86, 89, 87, 82, 91, 88, 85, 93,
                  79, 86, 90, 84, 92, 87, 89, 91, 85, 88),
  
  fisica = c(72, 68, 85, 79, 91, 63, 88, 75, 82, 69, 87, 74, 90, 71, 84,
             66, 89, 77, 83, 70, 92, 76, 81, 73, 86, 68, 85, 78, 91, 74,
             80, 67, 88, 75, 83, 72, 90, 76, 84, 69, 87, 73, 81, 78, 92,
             65, 89, 74, 82, 71, 86, 77, 84, 70, 88, 75, 83, 68, 91, 76,
             80, 72, 87, 74, 85, 69, 90, 77, 82, 73, 88, 75, 84, 71, 89,
             67, 83, 76, 91, 74, 80, 72, 86, 78, 84, 70, 87, 75, 89, 73,
             82, 68, 90, 76, 85, 72, 88, 74, 83, 79),
  
  ingles = c(88, 87, 89, 86, 90, 85, 91, 88, 87, 89, 86, 90, 88, 87, 89,
             85, 91, 88, 87, 90, 86, 89, 87, 88, 90, 85, 89, 88, 87, 90,
             86, 88, 89, 87, 90, 85, 89, 88, 87, 90, 86, 89, 87, 88, 90,
             85, 89, 88, 87, 90, 86, 89, 87, 88, 90, 85, 89, 88, 87, 90,
             86, 89, 87, 88, 90, 85, 89, 88, 87, 90, 86, 89, 87, 88, 90,
             85, 89, 88, 87, 90, 86, 89, 87, 88, 90, 85, 89, 88, 87, 90,
             86, 89, 87, 88, 90, 85, 89, 88, 87, 90),
  
  historia = c(95, 68, 82, 91, 73, 86, 79, 94, 71, 88, 76, 93, 69, 85, 78,
               96, 70, 87, 80, 92, 74, 89, 77, 95, 72, 84, 81, 90, 75, 93,
               68, 86, 79, 94, 73, 88, 76, 91, 71, 87, 80, 95, 74, 89, 77,
               92, 69, 85, 82, 96, 72, 90, 75, 88, 78, 93, 71, 86, 80, 94,
               73, 89, 76, 91, 74, 87, 79, 95, 72, 88, 75, 92, 78, 90, 71,
               86, 80, 94, 73, 89, 76, 93, 79, 87, 74, 91, 77, 95, 70, 88,
               81, 92, 75, 90, 78, 86, 73, 94, 76, 89),
  
  quimica = c(45, 92, 78, 34, 88, 67, 41, 95, 73, 38, 89, 64, 47, 91, 76,
              52, 85, 69, 43, 94, 71, 39, 87, 65, 48, 93, 77, 35, 90, 68,
              44, 96, 72, 40, 88, 66, 49, 92, 78, 36, 89, 67, 45, 94, 73,
              41, 87, 69, 47, 91, 75, 38, 93, 64, 50, 95, 77, 42, 88, 70,
              46, 90, 68, 37, 92, 66, 48, 94, 74, 43, 89, 71, 49, 87, 67,
              44, 93, 76, 40, 91, 68, 47, 95, 72, 38, 88, 65, 50, 90, 78,
              41, 92, 69, 46, 94, 73, 48, 89, 67, 44),
  
  educacion_fisica = c(98, 95, 97, 99, 96, 94, 98, 97, 95, 99, 96, 98, 94, 97, 99,
                       95, 98, 96, 97, 94, 99, 95, 98, 97, 96, 94, 99, 98, 95, 97,
                       96, 99, 94, 98, 97, 95, 96, 99, 98, 94, 97, 95, 99, 96, 98,
                       97, 94, 99, 95, 98, 96, 97, 99, 94, 98, 95, 97, 96, 99, 94,
                       98, 97, 95, 99, 96, 94, 98, 97, 99, 95, 96, 98, 94, 97, 99,
                       95, 98, 96, 97, 94, 99, 95, 98, 97, 96, 94, 99, 98, 95, 97,
                       96, 99, 94, 98, 97, 95, 96, 99, 98, 94)
)

cat("Tenemos", nrow(datos), "estudiantes y", ncol(datos), "materias\n\n")

# Vemos las primeras calificaciones
print("Primeras 5 filas:")
print(head(datos, 5))

cat("\n=== CALCULANDO COEFICIENTE DE VARIACIÓN ===\n\n")

# Para cada materia, calculamos:
# 1. Media (promedio)
# 2. Desviación estándar (qué tan dispersos están los datos)
# 3. Coeficiente de Variación = (Desviación estándar / Media) × 100

# MATEMÁTICAS
media_mat <- mean(datos$matematicas)
desv_mat <- sd(datos$matematicas)
cv_mat <- (desv_mat / media_mat) * 100

cat("MATEMÁTICAS:\n")
cat("  Promedio:", round(media_mat, 2), "\n")
cat("  Desviación estándar:", round(desv_mat, 2), "\n")
cat("  Coeficiente de Variación:", round(cv_mat, 2), "%\n\n")

# FÍSICA
media_fis <- mean(datos$fisica)
desv_fis <- sd(datos$fisica)
cv_fis <- (desv_fis / media_fis) * 100

cat("FÍSICA:\n")
cat("  Promedio:", round(media_fis, 2), "\n")
cat("  Desviación estándar:", round(desv_fis, 2), "\n")
cat("  Coeficiente de Variación:", round(cv_fis, 2), "%\n\n")

# INGLÉS
media_ing <- mean(datos$ingles)
desv_ing <- sd(datos$ingles)
cv_ing <- (desv_ing / media_ing) * 100

cat("INGLÉS:\n")
cat("  Promedio:", round(media_ing, 2), "\n")
cat("  Desviación estándar:", round(desv_ing, 2), "\n")
cat("  Coeficiente de Variación:", round(cv_ing, 2), "%\n\n")

# HISTORIA
media_hist <- mean(datos$historia)
desv_hist <- sd(datos$historia)
cv_hist <- (desv_hist / media_hist) * 100

cat("HISTORIA:\n")
cat("  Promedio:", round(media_hist, 2), "\n")
cat("  Desviación estándar:", round(desv_hist, 2), "\n")
cat("  Coeficiente de Variación:", round(cv_hist, 2), "%\n\n")

# QUÍMICA
media_quim <- mean(datos$quimica)
desv_quim <- sd(datos$quimica)
cv_quim <- (desv_quim / media_quim) * 100

cat("QUÍMICA:\n")
cat("  Promedio:", round(media_quim, 2), "\n")
cat("  Desviación estándar:", round(desv_quim, 2), "\n")
cat("  Coeficiente de Variación:", round(cv_quim, 2), "%\n\n")

# EDUCACIÓN FÍSICA
media_ef <- mean(datos$educacion_fisica)
desv_ef <- sd(datos$educacion_fisica)
cv_ef <- (desv_ef / media_ef) * 100

cat("EDUCACIÓN FÍSICA:\n")
cat("  Promedio:", round(media_ef, 2), "\n")
cat("  Desviación estándar:", round(desv_ef, 2), "\n")
cat("  Coeficiente de Variación:", round(cv_ef, 2), "%\n\n")

# Hacer una tabla resumen simple
cat("=== RESUMEN DE COEFICIENTES DE VARIACIÓN ===\n\n")

materias <- c("Matemáticas", "Física", "Inglés", "Historia", "Química", "Ed. Física")
cv_valores <- c(cv_mat, cv_fis, cv_ing, cv_hist, cv_quim, cv_ef)

# Ordenar de mayor a menor CV
orden <- order(cv_valores, decreasing = TRUE)
materias_ordenadas <- materias[orden]
cv_ordenados <- cv_valores[orden]

for(i in 1:length(materias_ordenadas)) {
  cat(i, ".", materias_ordenadas[i], ":", round(cv_ordenados[i], 2), "%\n")
}