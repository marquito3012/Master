# Instalar librerías si no están instaladas
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(corrplot)) install.packages("corrplot")
if(!require(caret)) install.packages("caret")
if(!require(moments)) install.packages("moments")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(glmnet)) install.packages("glmnet")

# Cargar librerías
library(tidyverse)
library(corrplot)
library(caret)
library(moments)
library(gridExtra)
library(ggplot2)
library(glmnet)

# Cargar datos
df <- read.csv("/home/marco/Master/Matemáticas y estadística para la IA/Ejercicio Feedback 1/train.csv", stringsAsFactors = FALSE)

# Vemos las dimensiones
dim(df)

#######################################################################################################################################################################
# LIMPIEZA DE LOS DATOS
#######################################################################################################################################################################
# Transformación de los NA no Nulos
df_clean <- df %>%
  mutate(
    # Variables donde NA = "None" o "No Aplica"
    Alley = replace_na(Alley, "None"),
    BsmtQual = replace_na(BsmtQual, "None"),
    BsmtCond = replace_na(BsmtCond, "None"),
    BsmtExposure = replace_na(BsmtExposure, "None"),
    BsmtFinType1 = replace_na(BsmtFinType1, "None"),
    BsmtFinType2 = replace_na(BsmtFinType2, "None"),
    FireplaceQu = replace_na(FireplaceQu, "None"),
    GarageType = replace_na(GarageType, "None"),
    GarageFinish = replace_na(GarageFinish, "None"),
    GarageQual = replace_na(GarageQual, "None"),
    GarageCond = replace_na(GarageCond, "None"),
    PoolQC = replace_na(PoolQC, "None"),
    Fence = replace_na(Fence, "None"),
    MiscFeature = replace_na(MiscFeature, "None"),
    
    # Variables numéricas donde NA implica 0
    GarageYrBlt = replace_na(GarageYrBlt, 0),
    GarageArea = replace_na(GarageArea, 0),
    GarageCars = replace_na(GarageCars, 0),
    BsmtFinSF1 = replace_na(BsmtFinSF1, 0),
    BsmtFinSF2 = replace_na(BsmtFinSF2, 0),
    BsmtUnfSF = replace_na(BsmtUnfSF, 0),
    TotalBsmtSF = replace_na(TotalBsmtSF, 0),
    BsmtFullBath = replace_na(BsmtFullBath, 0),
    BsmtHalfBath = replace_na(BsmtHalfBath, 0),
    MasVnrArea = replace_na(MasVnrArea, 0)
  )

# Verificamos si quedan NAs "reales"
colSums(is.na(df_clean))[colSums(is.na(df_clean)) > 0]

##### Mapeo de las variables categóricas ordinales
# Función para mapear calidades a números
qual_map <- c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)

# Variables que comparten esta escala
qual_cols <- c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", 
               "HeatingQC", "KitchenQual", "FireplaceQu", 
               "GarageQual", "GarageCond", "PoolQC")

# Aplicar el mapeo
for(col in qual_cols){
  df_clean[[col]] <- as.numeric(recode(df_clean[[col]], !!!qual_map))
}

# Mapeo para BsmtFinType 1 y 2
bsmt_fin_map <- c("None" = 0, "Unf" = 1, "LwQ" = 2, "Rec" = 3, "BLQ" = 4, "ALQ" = 5, "GLQ" = 6)
df_clean$BsmtFinType1 <- as.numeric(recode(df_clean$BsmtFinType1, !!!bsmt_fin_map))
df_clean$BsmtFinType2 <- as.numeric(recode(df_clean$BsmtFinType2, !!!bsmt_fin_map))

# Mapeo de Functional
func_map <- c("Sal" = 0, "Sev" = 1, "Maj2" = 2, "Maj1" = 3, "Mod" = 4, 
              "Min2" = 5, "Min1" = 6, "Typ" = 7)
df_clean$Functional <- as.numeric(recode(df_clean$Functional, !!!func_map))

# Verificar que se han convertido a numéricas
str(df_clean[, qual_cols])

#######################################################################################################################################################################
# EDA
#######################################################################################################################################################################
# Histograma de SalePrice
ggplot(df_clean, aes(x = SalePrice)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Distribución Original de SalePrice", x = "Precio ($)", y = "Densidad") +
  theme_minimal()

# Calcular Skewness (Asimetría)
cat("Asimetría original:", skewness(df_clean$SalePrice), "\n")

# Seleccionar solo variables numéricas
nums <- unlist(lapply(df_clean, is.numeric))
df_nums <- df_clean[, nums]

# Calcular matriz de correlación
cor_matrix <- cor(df_nums, use = "complete.obs")

# Filtrar para ver solo las que tienen alta correlación con SalePrice
high_cor_cols <- names(which(abs(cor_matrix[,"SalePrice"]) > 0.5)) # Umbral de 0.5
cor_matrix_high <- cor_matrix[high_cor_cols, high_cor_cols]

# Plot del Heatmap
corrplot(cor_matrix_high, method = "color", 
         type = "upper", 
         order = "hclust", 
         addCoef.col = "black", # Añadir coeficientes numéricos
         tl.col = "black", tl.srt = 45, # Color y rotación de texto
         diag = FALSE,
         number.cex = 0.7,
         title = "Matriz de Correlación (Variables más influyentes)", 
         mar = c(0,0,1,0))

# Transformación Logarítmica de SalePrice
df_clean$SalePrice_Log <- log(df_clean$SalePrice)

# Visualizar el cambio (Antes vs Después)
p_before <- ggplot(df_clean, aes(x = SalePrice)) +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Antes: Asimetría 1.88") + theme_minimal()

p_after <- ggplot(df_clean, aes(x = SalePrice_Log)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = paste("Después: Asimetría", round(skewness(df_clean$SalePrice_Log), 2))) +
  theme_minimal()

grid.arrange(p_before, p_after, ncol = 2)

# Vemos la distribución de Utilities
ggplot(df_clean, aes(x = Utilities)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Frecuencias de Utilities", x = "Utilities", y = "Frecuencia")

# Seleccionamos variables categóricas ordinales que hemos mapeado a numéricas
vars_to_check <- c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", 
                   "HeatingQC", "KitchenQual", "FireplaceQu", "GarageQual", 
                   "GarageCond", "PoolQC", "BsmtFinType1", "BsmtFinType2", "Functional")
df_subset <- df_clean[, c(vars_to_check, "SalePrice_Log")]
cor_matrix <- cor(df_subset, use = "complete.obs")
high_cor_cols_ord <- names(which(abs(cor_matrix[,"SalePrice_Log"]) > 0.5))
high_cor_cols_ord <- high_cor_cols_ord[-5]
high_cor_cols_ord

# Creamos una lista de gráficos (Boxplots)
plot_list <- list()

for(var in high_cor_cols_ord){
  # Corrección:
  # 1. Usamos .data[[var]] para leer la columna cuyo nombre está en la variable 'var'
  # 2. Quitamos las comillas de SalePrice_Log para que lea la columna numérica
  
  p <- ggplot(df_clean, aes(x = factor(.data[[var]]), y = SalePrice_Log)) +
    geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
    labs(title = paste("Precio vs", var), x = "Calidad", y = "Log(Precio)") +
    theme_minimal()
  
  plot_list[[var]] <- p
}

grid.arrange(grobs = plot_list, ncol = 2)

################################################################################
# TRATAMIENTO DE OUTLIERS
################################################################################
# GrLivArea
ggplot(df_clean, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "GrLivArea vs SalePrice", x = "GrLivArea", y = "SalePrice")

# 2. Filtrado manual basado en EDA
# Eliminamos casas con más de 4000 pies cuadrados de superficie habitable 
# pero que valen menos de 300.000$ (son anomalías claras)
dim_antes <- dim(df_clean)

df_clean <- df_clean %>%
  filter(!(GrLivArea > 4000 & SalePrice < 300000))

dim_despues <- dim(df_clean)

cat("Se han eliminado", dim_antes[1] - dim_despues[1], "outliers detectados visualmente en EDA.\n")

################################################################################
# División de datos y preparación para el modelo (SOLO NUMÉRICAS)
################################################################################

# 1. Identificar variables numéricas
nums <- unlist(lapply(df_clean, is.numeric))
df_numeric_only <- df_clean[, nums]

# 2. Limpieza final para el modelo
# Eliminamos 'Id' (no sirve para predecir) y 'SalePrice' (usaremos la Log)
df_model <- df_numeric_only %>% 
  select(-Id, -SalePrice)

# Verificamos las dimensiones
cat("Nuevas dimensiones para el modelo (Solo numéricas):", dim(df_model), "\n")

# Elegimos la semilla para la reproducibilidad del código
set.seed(77)

# 1. Crear índice para separar Train (60%) del resto (40%)
trainIndex <- createDataPartition(df_model$SalePrice_Log, p = 0.6, list = FALSE)

# Conjunto de Entrenamiento
train_data <- df_model[trainIndex, ]

# Datos restantes (para dividir luego en Val y Test)
temp_data <- df_model[-trainIndex, ]

# 2. Dividir los datos restantes (40%) en dos partes iguales
valIndex <- createDataPartition(temp_data$SalePrice_Log, p = 0.5, list = FALSE)

# Conjunto de Validación
val_data <- temp_data[valIndex, ]

# Conjunto de Test
test_data <- temp_data[-valIndex, ]

# Verificación de dimensiones
cat("Dimensiones Train:", dim(train_data), "\n")
cat("Dimensiones Validation:", dim(val_data), "\n")
cat("Dimensiones Test:", dim(test_data), "\n")

#######################################################################################################################################################################
# Imputación y normalización
#######################################################################################################################################################################

# 1. Separar predictores y variable objetivo
predictors_cols <- setdiff(names(train_data), "SalePrice_Log")

# 2. Crear el objeto de preprocesamiento (SOLO con train_data)
# 'nzv' elimina variables dummy que son casi constantes
# 'medianImpute' rellena NAs restantes
preprocessParams <- preProcess(train_data[, predictors_cols], method = c("nzv", "medianImpute", "center", "scale"))

print(preprocessParams)

# 3. Aplicar transformaciones a Train, Val y Test
train_norm <- predict(preprocessParams, train_data[, predictors_cols])
val_norm   <- predict(preprocessParams, val_data[, predictors_cols])
test_norm  <- predict(preprocessParams, test_data[, predictors_cols])

# 4. Volver a añadir la variable objetivo (SalePrice_Log) que habíamos apartado
train_norm$SalePrice_Log <- train_data$SalePrice_Log
val_norm$SalePrice_Log   <- val_data$SalePrice_Log
test_norm$SalePrice_Log  <- test_data$SalePrice_Log

# Verificación rápida
# Las medias de las variables numéricas en train_norm deberían ser cercanas a 0 y la desviación estándar cercana a 1.
cat("Media de GrLivArea (Train):", mean(train_norm$GrLivArea), "\n")
cat("SD de GrLivArea (Train):", sd(train_norm$GrLivArea), "\n")

#######################################################################################################################################################################
# PCA, Lasso, Ridge y regresión Lasso y Ridge aplicadas sobre los componentes principales extraídos por PCA
#######################################################################################################################################################################

################################################################################
# 1. GENERACIÓN DE COMPONENTES PRINCIPALES (PCA)
################################################################################

predictors_cols_norm <- setdiff(names(train_norm), "SalePrice_Log")

cat("Número de predictores después del preprocesamiento:", length(predictors_cols_norm), "\n")

# Calculamos PCA solo con el set de entrenamiento normalizado (excluyendo la variable objetivo)
# scale. = FALSE y center = FALSE porque YA normalizamos los datos antes
pca_obj <- prcomp(train_norm[, predictors_cols_norm], center = FALSE, scale. = FALSE)

# --- INTERPRETACIÓN DE COMPONENTES PCA ---
pca_importance <- summary(pca_obj)$importance
cat("Varianza explicada por componentes:\n")
print(pca_importance[, 1:10])  # Primeros 10 componentes

# Loadings de los primeros 5 componentes
pca_loadings <- pca_obj$rotation[, 1:5]
cat("\nLoadings de los primeros 5 componentes (primeras 10 variables):\n")
print(pca_loadings[1:10, ])

# Nos quedamos con los componentes que expliquen el 95% de la varianza
cum_var <- cumsum(pca_obj$sdev^2 / sum(pca_obj$sdev^2))
num_comp <- which(cum_var >= 0.95)[1]
cat("Número de componentes para 95% varianza:", num_comp, "\n")

# Proyectar los datos (Train, Val, Test) en el espacio de PCA
# Nos quedamos solo con los 'num_comp' primeros componentes
train_pca <- data.frame(predict(pca_obj, train_norm[, predictors_cols_norm]))[, 1:num_comp]
val_pca   <- data.frame(predict(pca_obj, val_norm[, predictors_cols_norm]))[, 1:num_comp]
test_pca  <- data.frame(predict(pca_obj, test_norm[, predictors_cols_norm]))[, 1:num_comp]

# Añadir la variable objetivo a los datasets de PCA
train_pca$SalePrice_Log <- train_norm$SalePrice_Log
val_pca$SalePrice_Log   <- val_norm$SalePrice_Log
test_pca$SalePrice_Log  <- test_norm$SalePrice_Log

################################################################################
# 2. ENTRENAMIENTO DE MODELOS
################################################################################

# Configuración de control para el entrenamiento (Cross-Validation de 5 folds)
ctrl <- trainControl(method = "cv", number = 5)

# --- MODELO 1: Regresión Lineal Múltiple sobre PCA (PCR) ---
cat("Entrenando Modelo 1: Regresión Lineal sobre PCA...\n")
model_pca_lm <- train(SalePrice_Log ~ ., 
                      data = train_pca, 
                      method = "lm", 
                      trControl = ctrl)

# --- MODELO 2: Lasso (Sobre datos originales normalizados) ---
cat("Entrenando Modelo 2: Lasso (Original)...\n")
# Grid de hiperparámetros lambda
lambda_grid <- 10^seq(-3, 1, length = 100)

model_lasso <- train(SalePrice_Log ~ ., 
                     data = train_norm, 
                     method = "glmnet",
                     trControl = ctrl,
                     tuneGrid = expand.grid(alpha = 1, lambda = lambda_grid),
                     standardize = FALSE) # alpha=1 es Lasso

# --- MODELO 3: Ridge (Sobre datos originales normalizados) ---
cat("Entrenando Modelo 3: Ridge (Original)...\n")
model_ridge <- train(SalePrice_Log ~ ., 
                     data = train_norm, 
                     method = "glmnet",
                     trControl = ctrl,
                     tuneGrid = expand.grid(alpha = 0, lambda = lambda_grid),
                     standardize = FALSE) # alpha=0 es Ridge

# --- MODELO 4a: Lasso sobre PCA ---
cat("Entrenando Modelo 4a: Lasso sobre PCA...\n")
model_pca_lasso <- train(SalePrice_Log ~ ., 
                         data = train_pca, 
                         method = "glmnet", 
                         trControl = ctrl,
                         tuneGrid = expand.grid(alpha = 1, lambda = lambda_grid))

# --- MODELO 4b: Ridge sobre PCA ---
cat("Entrenando Modelo 4b: Ridge sobre PCA...\n")
model_pca_ridge <- train(SalePrice_Log ~ ., 
                         data = train_pca, 
                         method = "glmnet", 
                         trControl = ctrl,
                         tuneGrid = expand.grid(alpha = 0, lambda = lambda_grid))

################################################################################
# 3. EVALUACIÓN Y COMPARACIÓN (Sobre conjunto de Validación)
################################################################################

# Función auxiliar para calcular métricas
calc_metrics <- function(model, data, actual) {
  preds <- predict(model, newdata = data)
  rmse_val <- RMSE(preds, actual)
  mae_val <- MAE(preds, actual)
  r2_val <- R2(preds, actual)
  return(c(RMSE = rmse_val, MAE = mae_val, R2 = r2_val))
}

# Evaluar todos los modelos
results <- rbind(
  PCA_Linear = calc_metrics(model_pca_lm, val_pca, val_pca$SalePrice_Log),
  Lasso_Original = calc_metrics(model_lasso, val_norm, val_norm$SalePrice_Log),
  Ridge_Original = calc_metrics(model_ridge, val_norm, val_norm$SalePrice_Log),
  Lasso_PCA = calc_metrics(model_pca_lasso, val_pca, val_pca$SalePrice_Log),
  Ridge_PCA = calc_metrics(model_pca_ridge, val_pca, val_pca$SalePrice_Log)
)

# Convertir a Dataframe para visualizar mejor
results_df <- as.data.frame(results)
results_df <- results_df[order(results_df$RMSE), ] # Ordenar por mejor RMSE (menor es mejor)

print("Resultados de la Evaluación (Validación):")
print(results_df)

# Gráfico comparativo de RMSE
results_df$Model <- rownames(results_df)
ggplot(results_df, aes(x = reorder(Model, RMSE), y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Comparación de Modelos (RMSE)", x = "Modelo", y = "RMSE (Menor es mejor)") +
  theme_minimal() +
  theme(legend.position = "none")

################################################################################
# 4. SELECCIÓN DEL MEJOR MODELO Y PREDICCIÓN FINAL (Test)
################################################################################

# Identificar el mejor modelo (menor RMSE)
best_model_name <- rownames(results_df)[1]
cat("El mejor modelo es:", best_model_name, "\n")

# Seleccionar el objeto del modelo correspondiente para usar en Test
if(best_model_name == "PCA_Linear") best_model <- model_pca_lm
if(best_model_name == "Lasso_Original") best_model <- model_lasso
if(best_model_name == "Ridge_Original") best_model <- model_ridge
if(best_model_name == "Lasso_PCA") best_model <- model_pca_lasso
if(best_model_name == "Ridge_PCA") best_model <- model_pca_ridge

# Evaluar en Test (Datos que el modelo NUNCA ha visto)
if(grepl("PCA", best_model_name)) {
  test_set_final <- test_pca
} else {
  test_set_final <- test_norm
}

final_preds <- predict(best_model, newdata = test_set_final)
final_rmse <- RMSE(final_preds, test_set_final$SalePrice_Log)
final_mae <- MAE(final_preds, test_set_final$SalePrice_Log)

cat("\n--- RESULTADOS FINALES EN TEST ---\n")
cat("Modelo seleccionado:", best_model_name, "\n")
cat("RMSE Final (Test):", final_rmse, "\n")
cat("MAE Final (Test):", final_mae, "\n")


# Convertir predicciones y valores reales a dólares
test_pred_dollars <- exp(final_preds)
actual_dollars <- exp(test_set_final$SalePrice_Log)

# Calcular RMSE y MAE en dólares
RMSE_dollars <- RMSE(test_pred_dollars, actual_dollars)
MAE_dollars <- MAE(test_pred_dollars, actual_dollars)

cat("RMSE en dólares (Test):", RMSE_dollars, "\n")
cat("MAE en dólares (Test):", MAE_dollars, "\n")

# --- ANÁLISIS DE RESIDUOS ---
residuals <- actual_dollars - test_pred_dollars

# Gráfico de residuos vs predicciones
ggplot() + 
  geom_point(aes(x = test_pred_dollars, y = residuals), alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Predicciones (dólares)", y = "Residuos (dólares)", 
       title = "Residuos vs Predicciones (escala original)") +
  theme_minimal()

# Histograma de residuos
ggplot() + 
  geom_histogram(aes(x = residuals), bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(x = "Residuos (dólares)", y = "Frecuencia", 
       title = "Distribución de Residuos") +
  theme_minimal()