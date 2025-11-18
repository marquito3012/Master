# ============================================================================
# REGRESIÓN LINEAL MÚLTIPLE CON PCA - BUENAS PRÁCTICAS
# ============================================================================
# Este código implementa correctamente la secuencia recomendada:
# 1. División de datos (train/val/test)
# 2. Preprocesamiento solo en train
# 3. Aplicación de transformaciones aprendidas a val/test
# 4. Entrenamiento y evaluación
# ============================================================================

# --- LIBRERÍAS ---
library(readxl)   # Para leer archivos Excel
library(caret)    # Para crear particiones estratificadas
library(stats)    # Para PCA y regresión lineal

# --- 1. CARGA DE DATOS ---
# Lee el archivo Excel con las características
df <- read_excel("C:/Users/34622/Desktop/Master Uax/SM142001_Matemáticas y Estadística para la IA/Codigos_2025_2026/Caracteristicas_Limp.xlsx")

# Separa variables predictoras (X) y variable objetivo (y)
X <- df[, c("Intelligence", "Strength", "Speed", "Durability", "Power")]
y <- df$Combat

cat("Dimensiones del dataset:", nrow(df), "filas,", ncol(X), "predictores\n\n")

# --- 2. DIVISIÓN DE DATOS (60% train, 20% val, 20% test) ---
set.seed(42)  # Semilla para reproducibilidad

# Paso 1: Separar 60% para entrenamiento
# createDataPartition hace división estratificada respetando distribución de y
trainIndex <- createDataPartition(y, p = 0.6, list = FALSE)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]

# Paso 2: Del 40% restante, dividir en 50%-50% (validación y reserva/test)
remainingIndex <- setdiff(seq_len(nrow(df)), trainIndex)
valIndex <- createDataPartition(y[remainingIndex], p = 0.5, list = FALSE)

X_val <- X[remainingIndex[valIndex], ]
y_val <- y[remainingIndex[valIndex]]

X_test <- X[remainingIndex[-valIndex], ]  # Renombrado a "test" 
y_test <- y[remainingIndex[-valIndex]]

cat("Tamaños de conjuntos:\n")
cat("  - Entrenamiento:", nrow(X_train), "observaciones\n")
cat("  - Validación:", nrow(X_val), "observaciones\n")
cat("  - Test:", nrow(X_test), "observaciones\n\n")

# --- 3. ESCALADO DE DATOS ---
# CRÍTICO: Solo se calculan parámetros (media y desviación) del conjunto de ENTRENAMIENTO
# Esto evita "data leakage" (filtración de información del test al train)

# Escalar conjunto de entrenamiento
X_train_scaled <- scale(X_train)

# Guardar los parámetros de escalado aprendidos del entrenamiento
center_params <- attr(X_train_scaled, "scaled:center")  # Medias
scale_params <- attr(X_train_scaled, "scaled:scale")    # Desviaciones estándar

# Aplicar los MISMOS parámetros a validación y test
# NUNCA recalcular medias/desviaciones de val/test
X_val_scaled <- scale(X_val, center = center_params, scale = scale_params)
X_test_scaled <- scale(X_test, center = center_params, scale = scale_params)

cat("Escalado completado usando parámetros de entrenamiento\n")
cat("Ejemplo - Media Intelligence (train):", round(center_params["Intelligence"], 2), "\n\n")

# --- 4. ANÁLISIS DE COMPONENTES PRINCIPALES (PCA) ---
# PCA se ajusta SOLO con datos de entrenamiento
# center=FALSE y scale.=FALSE porque ya escalamos manualmente antes
pca <- prcomp(X_train_scaled, center = FALSE, scale. = FALSE)

# Calcular varianza explicada acumulada por cada componente
var_explained <- summary(pca)$importance[3, ]  # Tercera fila = proporción acumulada

# Seleccionar componentes que expliquen al menos 95% de la varianza
n_comp <- min(which(var_explained >= 0.95))

cat("--- RESULTADO PCA ---\n")
cat("Componentes que explican ≥95% varianza:", n_comp, "de", ncol(X), "\n")
cat("Varianza explicada por componente:\n")
print(round(summary(pca)$importance, 3))
cat("\n")

# --- 5. PROYECCIÓN A ESPACIO PCA ---
# Transformar datos usando la rotación (loading matrix) aprendida del train

# Train: Ya transformado por prcomp
X_train_pca <- pca$x[, 1:n_comp, drop = FALSE]

# Val y Test: Proyectar usando la matriz de rotación del PCA entrenado
X_val_pca <- as.matrix(X_val_scaled) %*% pca$rotation[, 1:n_comp, drop = FALSE]
X_test_pca <- as.matrix(X_test_scaled) %*% pca$rotation[, 1:n_comp, drop = FALSE]

# --- 6. ENTRENAMIENTO DEL MODELO ---
# Ajustar regresión lineal múltiple con los componentes principales
modelo_lm_pca <- lm(y_train ~ ., data = as.data.frame(X_train_pca))

cat("--- MODELO AJUSTADO ---\n")
print(summary(modelo_lm_pca))
cat("\n")
#salidas: Los errores están ligeramente sesgados hacia subestimar Combat (Mediana ≈ -5.2)
#Outliers: Hay algunos casos con errores muy grandes (±50 puntos), probablemente personajes con habilidades únicas
#Todos los PC son estadisticamente significativos
#R² = 0.67 es bueno para datos reales



# --- 7. PREDICCIÓN Y EVALUACIÓN ---
# Predecir en validación y test usando las proyecciones PCA
pred_train_pca <- predict(modelo_lm_pca, newdata = as.data.frame(X_train_pca))
pred_val_pca <- predict(modelo_lm_pca, newdata = as.data.frame(X_val_pca))
pred_test_pca <- predict(modelo_lm_pca, newdata = as.data.frame(X_test_pca))

# Función para calcular métricas de error
rmse <- function(y_true, y_pred) sqrt(mean((y_true - y_pred)^2))
mae <- function(y_true, y_pred) mean(abs(y_true - y_pred))
r2 <- function(y_true, y_pred) cor(y_true, y_pred)^2

# --- 8. RESULTADOS ---
cat("========================================\n")
cat("      MÉTRICAS DE RENDIMIENTO (PCA)\n")
cat("========================================\n\n")

cat("ENTRENAMIENTO:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_pca), 2), "\n") #promedio de los errores de predicción
cat("  MAE: ", round(mae(y_train, pred_train_pca), 2), "\n") #Error promedio sin penalizar outlaiers
cat("  R²:  ", round(r2(y_train, pred_train_pca), 4), "\n\n") #Coeficiente de determinación (varianza explicada)

cat("VALIDACIÓN:\n")
cat("  RMSE:", round(rmse(y_val, pred_val_pca), 2), "\n")
cat("  MAE: ", round(mae(y_val, pred_val_pca), 2), "\n")
cat("  R²:  ", round(r2(y_val, pred_val_pca), 4), "\n\n")

cat("TEST (conjunto de reserva):\n")
cat("  RMSE:", round(rmse(y_test, pred_test_pca), 2), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_pca), 2), "\n")
cat("  R²:  ", round(r2(y_test, pred_test_pca), 4), "\n\n")
#salidas: Train y Test son casi idénticos (RMSE: 19.33 vs 19.16) generaliza perfectamente a datos nuevos



# --- 9. VISUALIZACIONES OPCIONALES ---
# Descomenta si quieres ver gráficos

# # Varianza explicada por componente
par(mfrow = c(1, 2))
 barplot(summary(pca)$importance[2, ], 
         main = "Varianza por Componente",
         xlab = "Componente Principal", 
         ylab = "Proporción de Varianza",
         col = "steelblue")
 
 plot(var_explained, type = "b", 
      main = "Varianza Acumulada",
      xlab = "Número de Componentes", 
      ylab = "Proporción Acumulada",
      col = "darkred", lwd = 2)
 abline(h = 0.95, col = "blue", lty = 2, lwd = 2)
 legend("bottomright", legend = "Umbral 95%", 
        col = "blue", lty = 2, lwd = 2)
 
# # Predicciones vs Valores Reales
 par(mfrow = c(1, 3))
 plot(y_train, pred_train_pca, main = "Train", 
      xlab = "Real", ylab = "Predicho", pch = 19, col = "blue")
 abline(0, 1, col = "red", lwd = 2)
 
 plot(y_val, pred_val_pca, main = "Validación", 
      xlab = "Real", ylab = "Predicho", pch = 19, col = "green")
 abline(0, 1, col = "red", lwd = 2)
 
 plot(y_test, pred_test_pca, main = "Test", 
      xlab = "Real", ylab = "Predicho", pch = 19, col = "orange")
 abline(0, 1, col = "red", lwd = 2)

# ============================================================================
# NOTAS IMPORTANTES:
# ============================================================================
# 1. NUNCA usar estadísticas de val/test para preprocesamiento
# 2. PCA se ajusta solo en train, luego se proyectan val/test
# 3. El conjunto de test NO debe tocarse hasta la evaluación final
# 4. Si RMSE(val) << RMSE(test) → posible overfitting
# 5. Usar validación para ajustar hiperparámetros (ej: número de componentes)
# ============================================================================
 
 cat("\n\n")
 cat("############################################################\n")
 cat("#                                                          #\n")
 cat("#     EXTENSIÓN: MODELOS CON REGULARIZACIÓN               #\n")
 cat("#                                                          #\n")
 cat("############################################################\n\n")
 
 # --- LIBRERÍA ADICIONAL ---
 # Necesitamos glmnet para Ridge y Lasso
 library(glmnet)
 
 # ==============================================================================
 # PASO 10: RIDGE REGRESSION (L2)
 # ==============================================================================
 # Ridge penaliza la suma de los cuadrados de los coeficientes: λ × Σ(β²)
 # Objetivo: Reducir coeficientes grandes para evitar overfitting
 # NO elimina variables (coeficientes nunca llegan exactamente a 0)
 
 cat("========================================\n")
 cat("  ENTRENANDO RIDGE REGRESSION (L2)\n")
 cat("========================================\n\n")
 
 # Definir grilla de valores lambda para probar
 # Lambda controla la fuerza de la regularización (de 100 a 0.01)
 lambda_grid <- 10^seq(2, -2, length = 100)
 
 # Entrenar Ridge con validación cruzada de 10 folds
 # K-Fold CV usa TODO el conjunto de entrenamiento para elegir lambda óptimo
 # alpha=0 indica Ridge (L2), alpha=1 sería Lasso (L1)
 cat("Buscando lambda óptimo con 10-Fold Cross-Validation...\n")
 ridge_cv <- cv.glmnet(
   X_train_pca,           # Datos de entrenamiento (componentes PCA)
   y_train,               # Variable objetivo
   alpha = 0,             # Ridge (L2)
   lambda = lambda_grid,  # Grilla de lambdas a probar
   nfolds = 10,           # 10-fold cross-validation
   standardize = FALSE    # Ya escalamos con PCA
 )
 
 # Lambda óptimo: el que minimiza el error de validación cruzada
 lambda_ridge_opt <- ridge_cv$lambda.min
 
 cat("\nResultado de validación cruzada:\n")
 cat("  Lambda óptimo:", round(lambda_ridge_opt, 4), "\n")
 cat("  Interpretación: Este lambda minimiza el error de CV\n\n")
 
 # Entrenar modelo Ridge final con lambda óptimo
 modelo_ridge <- glmnet(
   X_train_pca, 
   y_train, 
   alpha = 0,                    # Ridge
   lambda = lambda_ridge_opt,    # Lambda óptimo encontrado
   standardize = FALSE
 )
 
 # Mostrar coeficientes de Ridge
 cat("Coeficientes Ridge:\n")
 print(round(coef(modelo_ridge), 4))
 cat("\nObservación: Ridge REDUCE coeficientes pero NO los elimina (ninguno es 0)\n\n")
 
 # ==============================================================================
 # PASO 11: LASSO REGRESSION (L1)
 # ==============================================================================
 # Lasso penaliza la suma de valores absolutos: λ × Σ|β|
 # Objetivo: Selección de variables (puede reducir coeficientes a 0)
 # SÍ elimina variables irrelevantes automáticamente
 
 cat("========================================\n")
 cat("  ENTRENANDO LASSO REGRESSION (L1)\n")
 cat("========================================\n\n")
 
 cat("Buscando lambda óptimo con 10-Fold Cross-Validation...\n")
 lasso_cv <- cv.glmnet(
   X_train_pca, 
   y_train, 
   alpha = 1,             # Lasso (L1)
   lambda = lambda_grid,
   nfolds = 10,
   standardize = FALSE
 )
 
 lambda_lasso_opt <- lasso_cv$lambda.min
 
 cat("\nResultado de validación cruzada:\n")
 cat("  Lambda óptimo:", round(lambda_lasso_opt, 4), "\n")
 cat("  Interpretación: Este lambda minimiza el error de CV\n\n")
 
 # Entrenar modelo Lasso final
 modelo_lasso <- glmnet(
   X_train_pca, 
   y_train, 
   alpha = 1,                    # Lasso
   lambda = lambda_lasso_opt,
   standardize = FALSE
 )
 
 # Mostrar coeficientes de Lasso
 cat("Coeficientes Lasso:\n")
 coef_lasso <- coef(modelo_lasso)
 print(round(coef_lasso, 4))
 
 # Contar cuántos coeficientes fueron eliminados (reducidos a 0)
 n_eliminados <- sum(abs(coef_lasso[-1]) < 1e-10)  # Excluir intercepto
 cat("\nVariables eliminadas por Lasso:", n_eliminados, "de", n_comp, "\n")
 
 if(n_eliminados > 0) {
   cat("Interpretación: Lasso identificó que", n_eliminados, 
       "componente(s) tienen poco poder predictivo\n")
   cat("               El modelo se simplifica automáticamente\n\n")
 } else {
   cat("Interpretación: Los", n_comp, 
       "componentes son todos relevantes para predecir Combat\n\n")
 }
 
 # ==============================================================================
 # PASO 12: PREDICCIONES CON RIDGE Y LASSO
 # ==============================================================================
 
 cat("========================================\n")
 cat("  GENERANDO PREDICCIONES\n")
 cat("========================================\n\n")
 
 # Predicciones Ridge
 pred_train_ridge <- as.vector(predict(modelo_ridge, newx = X_train_pca, s = lambda_ridge_opt))
 pred_val_ridge <- as.vector(predict(modelo_ridge, newx = X_val_pca, s = lambda_ridge_opt))
 pred_test_ridge <- as.vector(predict(modelo_ridge, newx = X_test_pca, s = lambda_ridge_opt))
 
 # Predicciones Lasso
 pred_train_lasso <- as.vector(predict(modelo_lasso, newx = X_train_pca, s = lambda_lasso_opt))
 pred_val_lasso <- as.vector(predict(modelo_lasso, newx = X_val_pca, s = lambda_lasso_opt))
 pred_test_lasso <- as.vector(predict(modelo_lasso, newx = X_test_pca, s = lambda_lasso_opt))
 
 cat("Predicciones completadas para los 3 modelos en train/val/test\n\n")
 
 # ==============================================================================
 # PASO 13: CALCULAR FUNCIONES DE PÉRDIDA (MSE Y MAE)
 # ==============================================================================
 
 cat("========================================\n")
 cat("  FUNCIONES DE PÉRDIDA\n")
 cat("========================================\n\n")
 
 # MSE: Mean Squared Error (penaliza errores grandes)
 mse <- function(y_true, y_pred) {
   mean((y_true - y_pred)^2)
 }
 
 # MAE ya está definida arriba, pero la incluimos aquí también por claridad
 # MAE: Mean Absolute Error (error promedio sin penalizar outliers)
 
 cat("MSE (Mean Squared Error):\n")
 cat("  - Penaliza errores grandes (outliers)\n")
 cat("  - Unidades: (puntos de Combat)²\n")
 cat("  - Fórmula: (1/n) × Σ(y_real - y_pred)²\n\n")
 
 cat("MAE (Mean Absolute Error):\n")
 cat("  - Error promedio sin penalizar outliers\n")
 cat("  - Unidades: puntos de Combat\n")
 cat("  - Fórmula: (1/n) × Σ|y_real - y_pred|\n")
 cat("  - Más fácil de interpretar que MSE\n\n")
 
 cat("RMSE (Root Mean Squared Error):\n")
 cat("  - Raíz cuadrada de MSE\n")
 cat("  - Mismas unidades que y (puntos)\n")
 cat("  - Métrica estándar en regresión\n\n")
 
 # ==============================================================================
 # PASO 14: TABLA COMPARATIVA DE LOS 3 MODELOS
 # ==============================================================================
 
 cat("========================================\n")
 cat("  COMPARACIÓN DE MODELOS\n")
 cat("========================================\n\n")
 
 # Crear tabla con todas las métricas
 resultados <- data.frame(
   Modelo = rep(c("Lineal", "Ridge", "Lasso"), each = 3),
   Conjunto = rep(c("Train", "Val", "Test"), 3),
   
   MSE = round(c(
     mse(y_train, pred_train_pca), mse(y_val, pred_val_pca), mse(y_test, pred_test_pca),
     mse(y_train, pred_train_ridge), mse(y_val, pred_val_ridge), mse(y_test, pred_test_ridge),
     mse(y_train, pred_train_lasso), mse(y_val, pred_val_lasso), mse(y_test, pred_test_lasso)
   ), 2),
   
   MAE = round(c(
     mae(y_train, pred_train_pca), mae(y_val, pred_val_pca), mae(y_test, pred_test_pca),
     mae(y_train, pred_train_ridge), mae(y_val, pred_val_ridge), mae(y_test, pred_test_ridge),
     mae(y_train, pred_train_lasso), mae(y_val, pred_val_lasso), mae(y_test, pred_test_lasso)
   ), 2),
   
   RMSE = round(c(
     rmse(y_train, pred_train_pca), rmse(y_val, pred_val_pca), rmse(y_test, pred_test_pca),
     rmse(y_train, pred_train_ridge), rmse(y_val, pred_val_ridge), rmse(y_test, pred_test_ridge),
     rmse(y_train, pred_train_lasso), rmse(y_val, pred_val_lasso), rmse(y_test, pred_test_lasso)
   ), 2)
 )
 
 print(resultados)
 
 # Identificar mejor modelo en TEST
 cat("\n--- MEJOR MODELO EN TEST ---\n")
 test_results <- resultados[resultados$Conjunto == "Test", ]
 cat("Mejor MSE: ", test_results$Modelo[which.min(test_results$MSE)], 
     "=", min(test_results$MSE), "\n")
 cat("Mejor MAE: ", test_results$Modelo[which.min(test_results$MAE)], 
     "=", min(test_results$MAE), "\n")
 cat("Mejor RMSE:", test_results$Modelo[which.min(test_results$RMSE)], 
     "=", min(test_results$RMSE), "\n\n")
 
 # ==============================================================================
 # PASO 15: GRÁFICAS DE FUNCIONES DE PÉRDIDA
 # ==============================================================================
 
 cat("========================================\n")
 cat("  GENERANDO GRÁFICAS\n")
 cat("========================================\n\n")
 # Colores y nombres de los modelos
 colores <- c("steelblue", "darkgreen", "darkorange")
 nombres_modelos <- c("Lineal", "Ridge", "Lasso")
 
 # Matrices
 mse_matrix  <- matrix(resultados$MSE,  nrow = 3, byrow = TRUE)
 mae_matrix  <- matrix(resultados$MAE,  nrow = 3, byrow = TRUE)
 rmse_matrix <- matrix(resultados$RMSE, nrow = 3, byrow = TRUE)
 colnames(mse_matrix)  <- colnames(mae_matrix)  <- colnames(rmse_matrix) <- c("Train", "Val", "Test")
 rownames(mse_matrix)  <- rownames(mae_matrix)  <- rownames(rmse_matrix) <- nombres_modelos
 
 # Función para agregar etiquetas sobre barras
 add_bar_labels <- function(bar_positions, valores, decimales = 2) {
   n_grupos <- ncol(valores)
   n_modelos <- nrow(valores)
   idx <- 1
   for(j in 1:n_grupos) {
     for(i in 1:n_modelos) {
       text(bar_positions[idx], valores[i, j], labels = round(valores[i, j], decimales), pos=3, cex=0.7)
       idx <- idx + 1
     }
   }
 }
 
 # Gráfica 1: MSE
 par(mfrow = c(1, 3), mar = c(5, 5, 4, 2)) 
 bp_mse <- barplot(mse_matrix, beside=TRUE, col=colores, main="MSE (Mean Squared Error)\nMenor es mejor",
                   ylab="MSE (puntos²)", xlab="Conjunto", ylim=c(0, max(mse_matrix)*1.15),
                   legend.text=nombres_modelos, args.legend=list(x="topright", cex=0.8))
 add_bar_labels(bp_mse, mse_matrix, decimales=1)
 
 # Gráfica 2: MAE
 bp_mae <- barplot(mae_matrix, beside=TRUE, col=colores, main="MAE (Mean Absolute Error)\nMenor es mejor",
                   ylab="MAE (puntos)", xlab="Conjunto", ylim=c(0, max(mae_matrix)*1.15),
                   legend.text=nombres_modelos, args.legend=list(x="topright", cex=0.8))
 add_bar_labels(bp_mae, mae_matrix, decimales=2)
 
 # Gráfica 3: RMSE
 bp_rmse <- barplot(rmse_matrix, beside=TRUE, col=colores, main="RMSE (Root Mean Squared Error)\nMenor es mejor",
                    ylab="RMSE (puntos)", xlab="Conjunto", ylim=c(0, max(rmse_matrix)*1.15),
                    legend.text=nombres_modelos, args.legend=list(x="topright", cex=0.8))
 add_bar_labels(bp_rmse, rmse_matrix, decimales=2)
 
 # Gráfica 4: Evolución Train → Val → Test
 dev.off(); try(dev.off(), silent = TRUE)
 par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
 
 plot(1:3, mse_matrix[1, ], type="b", col=colores[1], lwd=2, pch=19,
      ylim=range(mse_matrix), xlab="", ylab="MSE (puntos²)",
      main="Evolución de MSE: Train → Val → Test", xaxt="n")
 lines(1:3, mse_matrix[2, ], type="b", col=colores[2], lwd=2, pch=19)
 lines(1:3, mse_matrix[3, ], type="b", col=colores[3], lwd=2, pch=19)
 axis(1, at=1:3, labels=c("Train","Val","Test"))
 legend("topleft", legend=nombres_modelos, col=colores, lwd=2, pch=19, cex=0.8)
 grid()
 
 plot(1:3, mae_matrix[1, ], type="b", col=colores[1], lwd=2, pch=19,
      ylim=range(mae_matrix), xlab="Conjunto", ylab="MAE (puntos)",
      main="Evolución de MAE: Train → Val → Test", xaxt="n")
 lines(1:3, mae_matrix[2, ], type="b", col=colores[2], lwd=2, pch=19)
 lines(1:3, mae_matrix[3, ], type="b", col=colores[3], lwd=2, pch=19)
 axis(1, at=1:3, labels=c("Train","Val","Test"))
 legend("topleft", legend=nombres_modelos, col=colores, lwd=2, pch=19, cex=0.8)
 grid()
 par(mfrow = c(1, 1))
 
 
 
 # ==============================================================================
 # PASO 16: ANÁLISIS E INTERPRETACIÓN FINAL
 # ==============================================================================
 
 cat("\n========================================\n")
 cat("  ANÁLISIS DE OVERFITTING\n")
 cat("========================================\n\n")
 
 # Análisis de cada modelo
 for(i in 1:3) {
   modelo_nombre <- nombres_modelos[i]
   train_rmse <- rmse_matrix[i, "Train"]
   test_rmse <- rmse_matrix[i, "Test"]
   diff <- test_rmse - train_rmse
   diff_pct <- round(100 * diff / train_rmse, 2)
   
   cat(modelo_nombre, ":\n")
   cat("  Train RMSE =", round(train_rmse, 2), "\n")
   cat("  Test RMSE  =", round(test_rmse, 2), "\n")
   cat("  Diferencia =", round(diff, 2), "puntos (", diff_pct, "%)\n")
   
   if(abs(diff_pct) < 2) {
     cat("  → ✓ Excelente generalización (sin overfitting)\n\n")
   } else if(diff_pct > 5) {
     cat("  → ⚠ Posible overfitting\n\n")
   } else {
     cat("  → ✓ Buena generalización\n\n")
   }
 }
 
 cat("========================================\n")
 cat("  CONCLUSIONES FINALES\n")
 cat("========================================\n\n")
 
 # Identificar mejor modelo
 best_model <- test_results$Modelo[which.min(test_results$RMSE)]
 best_rmse <- min(test_results$RMSE)
 
 # Calcular diferencia entre mejor y peor
 max_rmse <- max(test_results$RMSE)
 mejora <- round(100 * (max_rmse - best_rmse) / max_rmse, 2)
 
 cat("1. MEJOR MODELO EN TEST:", best_model, "\n")
 cat("   RMSE =", round(best_rmse, 2), "puntos\n\n")
 
 if(mejora < 1) {
   cat("2. DIFERENCIA ENTRE MODELOS: <1% (prácticamente idénticos)\n")
   cat("   Interpretación:\n")
   cat("   - Los 3 modelos tienen rendimiento equivalente\n")
   cat("   - La regularización NO aporta mejora significativa\n")
   cat("   - PCA ya optimizó el problema (eliminó multicolinealidad)\n")
   cat("   - No hay overfitting que corregir\n\n")
   cat("   RECOMENDACIÓN: Usar modelo LINEAL por simplicidad\n\n")
 } else {
   cat("2. MEJORA CON REGULARIZACIÓN:", mejora, "%\n")
   cat("   Interpretación:\n")
   cat("   - La regularización SÍ mejora el rendimiento\n")
   cat("   - Había overfitting que Ridge/Lasso corrigieron\n\n")
   cat("   RECOMENDACIÓN: Usar modelo", best_model, "\n\n")
 }
 
 cat("3. SOBRE LAS FUNCIONES DE PÉRDIDA:\n")
 cat("   MSE  = Penaliza errores grandes (sensible a outliers)\n")
 cat("   MAE  = Error promedio (más robusto, más interpretable)\n")
 cat("   RMSE = Métrica estándar (balance entre MSE y MAE)\n\n")
 
 cat("4. ERROR TÍPICO DE PREDICCIÓN:\n")
 cat("   Si Combat real = 80 puntos\n")
 cat("   Predicción típica: entre", round(80 - best_rmse), "y", 
     round(80 + best_rmse), "puntos\n")
 cat("   Intervalo: ±", round(best_rmse, 1), "puntos\n\n")
 
 cat("5. PODER PREDICTIVO:\n")
 cat("   R² ≈", round(test_results$RMSE[1], 4), 
     "→ Explica ~67% de variabilidad en Combat\n")
 cat("   33% restante: variables no incluidas o aleatoriedad inherente\n\n")
 
 # Comparar con modelo nulo (baseline)
 media_y <- mean(y_train)
 pred_nula <- rep(media_y, length(y_test))
 rmse_nulo <- rmse(y_test, pred_nula)
 mejora_vs_nulo <- round(100 * (rmse_nulo - best_rmse) / rmse_nulo, 1)
 
 cat("6. COMPARACIÓN CON MODELO NULO (predecir siempre la media):\n")
 cat("   RMSE modelo nulo:", round(rmse_nulo, 2), "\n")
 cat("   RMSE mejor modelo:", round(best_rmse, 2), "\n")
 cat("   Mejora:", mejora_vs_nulo, "%\n")
 cat("   → Tu modelo es", mejora_vs_nulo, "% mejor que adivinar\n\n")
 

 # ============================================================================
 # NOTAS SOBRE RIDGE Y LASSO:
 # ============================================================================
 # 1. Ridge y Lasso se aplican SOBRE los componentes PCA (no sobre vars originales)
 # 2. Lambda se selecciona con K-Fold CV (k=10) para evitar data leakage (fuga de datos)
 # 3. Ridge NO elimina variables (coeficientes → pequeños pero ≠ 0)
 # 4. Lasso SÍ puede eliminar variables (coeficientes → exactamente 0)
 # 5. Si lambda óptimo es muy pequeño (≈0.001) → regularización innecesaria
 # 6. Si Train ≈ Test → No hay overfitting (Memoriza, no generaliza) → Regularización aporta poco
 # 7. MSE penaliza outliers, MAE no → Usar ambas para análisis completo
 # ============================================================================
 
 
 
 
 
 
 