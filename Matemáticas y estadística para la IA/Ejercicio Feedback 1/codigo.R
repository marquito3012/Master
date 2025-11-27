# Instalar librerías si no están instaladas
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(corrplot)) install.packages("corrplot")
if(!require(caret)) install.packages("caret")
if(!require(moments)) install.packages("moments")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(ggplot2)) install.packages("ggplot2")

# Cargar librerías
library(tidyverse)
library(corrplot)
library(caret)
library(moments)
library(gridExtra)
library(ggplot2)

# Cargar datos
df <- read.csv("/home/marco/Master/Matemáticas y estadística para la IA/Ejercicio Feedback 1/train.csv", stringsAsFactors = FALSE)

# Vemos las dimensiones
dim(df)

#######################################################################################################################################################################
# EDA (se mantiene igual ya que no causa data leakage)
#######################################################################################################################################################################

# Transformación Logarítmica de SalePrice (se mantiene ya que es solo transformación de variable objetivo)
df$SalePrice_Log <- log(df$SalePrice)

# Histograma de SalePrice
ggplot(df, aes(x = SalePrice)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Distribución Original de SalePrice", x = "Precio ($)", y = "Densidad") +
  theme_minimal()

# Calcular Skewness (Asimetría)
cat("Asimetría original:", skewness(df$SalePrice), "\n")

# Seleccionar solo variables numéricas
nums <- unlist(lapply(df, is.numeric))
df_nums <- df[, nums]

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

# Visualizar el cambio (Antes vs Después)
p_before <- ggplot(df, aes(x = SalePrice)) +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Antes: Asimetría 1.88") + theme_minimal()

p_after <- ggplot(df, aes(x = SalePrice_Log)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = paste("Después: Asimetría", round(skewness(df$SalePrice_Log), 2))) +
  theme_minimal()

grid.arrange(p_before, p_after, ncol = 2)

# Vemos la distribución de Utilities
ggplot(df, aes(x = Utilities)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Frecuencias de Utilities", x = "Utilities", y = "Frecuencia")

# Gráficos de dispersión de variables numéricas clave vs SalePrice
# GrLivArea
p1 <- ggplot(df, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "GrLivArea vs SalePrice", x = "GrLivArea", y = "SalePrice")

# OverallQual
p2 <- ggplot(df, aes(x = factor(OverallQual), y = SalePrice)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  labs(title = "OverallQual vs SalePrice", x = "OverallQual", y = "SalePrice")

grid.arrange(p1, p2, ncol = 2)

#######################################################################################################################################################################
# DIVISIÓN DE DATOS (PRIMERO, antes de cualquier preprocesamiento)
#######################################################################################################################################################################

set.seed(77)

# Crear dataframe base para modelado
df_base <- df %>%
  select(-SalePrice) # Eliminamos SalePrice original, usaremos SalePrice_Log

# 1. Crear índice para separar Train (60%) del resto (40%)
trainIndex <- createDataPartition(df_base$SalePrice_Log, p = 0.6, list = FALSE)

# Conjunto de Entrenamiento
train_data <- df_base[trainIndex, ]

# Datos restantes (para dividir luego en Val y Test)
temp_data <- df_base[-trainIndex, ]

# 2. Dividir los datos restantes (40%) en dos partes iguales (50% y 50%)
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
# FUNCIÓN PARA LIMPIEZA Y TRANSFORMACIÓN (se aplicará separadamente a cada conjunto)
#######################################################################################################################################################################

apply_preprocessing <- function(data) {
  data_clean <- data %>%
    mutate(
      # Variables donde NA = "None" o "No Access"
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
  
  # Mapeo de las variables categóricas ordinales (usando mapeos fijos, no aprendidos de datos)
  qual_map <- c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)
  qual_cols <- c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", 
                 "HeatingQC", "KitchenQual", "FireplaceQu", 
                 "GarageQual", "GarageCond", "PoolQC")
  
  for(col in qual_cols){
    data_clean[[col]] <- as.numeric(recode(data_clean[[col]], !!!qual_map))
  }
  
  bsmt_fin_map <- c("None" = 0, "Unf" = 1, "LwQ" = 2, "Rec" = 3, "BLQ" = 4, "ALQ" = 5, "GLQ" = 6)
  data_clean$BsmtFinType1 <- as.numeric(recode(data_clean$BsmtFinType1, !!!bsmt_fin_map))
  data_clean$BsmtFinType2 <- as.numeric(recode(data_clean$BsmtFinType2, !!!bsmt_fin_map))
  
  func_map <- c("Sal" = 0, "Sev" = 1, "Maj2" = 2, "Maj1" = 3, "Mod" = 4, 
                "Min2" = 5, "Min1" = 6, "Typ" = 7)
  data_clean$Functional <- as.numeric(recode(data_clean$Functional, !!!func_map))
  
  return(data_clean)
}

# Aplicar preprocesamiento a cada conjunto
train_clean <- apply_preprocessing(train_data)
val_clean <- apply_preprocessing(val_data)
test_clean <- apply_preprocessing(test_data)

# Verificar que se han convertido a numéricas
cat("Verificación conversión numérica en train:\n")
qual_cols <- c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", 
               "HeatingQC", "KitchenQual", "FireplaceQu", 
               "GarageQual", "GarageCond", "PoolQC")
print(str(train_clean[, qual_cols]))

#######################################################################################################################################################################
# ONE-HOT ENCODING (ajustado solo con train y aplicado a todos los conjuntos)
#######################################################################################################################################################################

# 1. Eliminamos variables que NO son predictoras
train_modeling <- train_clean %>% select(-Id)
val_modeling <- val_clean %>% select(-Id)
test_modeling <- test_clean %>% select(-Id)

# 2. One-Hot Encoding (Variables Dummy) - SOLO se ajusta con train
dummies_conf <- dummyVars(SalePrice_Log ~ ., data = train_modeling)

# Aplicamos la transformación a TODOS los conjuntos usando los parámetros aprendidos de train
train_dummy <- predict(dummies_conf, newdata = train_modeling)
val_dummy <- predict(dummies_conf, newdata = val_modeling)
test_dummy <- predict(dummies_conf, newdata = test_modeling)

# Convertir a dataframes y añadir variable objetivo
train_dummy_df <- as.data.frame(train_dummy)
train_dummy_df$SalePrice_Log <- train_modeling$SalePrice_Log

val_dummy_df <- as.data.frame(val_dummy)
val_dummy_df$SalePrice_Log <- val_modeling$SalePrice_Log

test_dummy_df <- as.data.frame(test_dummy)
test_dummy_df$SalePrice_Log <- test_modeling$SalePrice_Log

# 3. Limpieza de nombres de columnas
names(train_dummy_df) <- make.names(names(train_dummy_df))
names(val_dummy_df) <- make.names(names(val_dummy_df))
names(test_dummy_df) <- make.names(names(test_dummy_df))

# Verificamos el cambio drástico en dimensiones
cat("Nuevas dimensiones para Train:", dim(train_dummy_df), "\n")
cat("Nuevas dimensiones para Validation:", dim(val_dummy_df), "\n")
cat("Nuevas dimensiones para Test:", dim(test_dummy_df), "\n")

#######################################################################################################################################################################
# IMPUTACIÓN, NORMALIZACIÓN (se mantiene igual pero con los nuevos conjuntos)
#######################################################################################################################################################################

# 1. Separar predictores y variable objetivo
predictors_cols <- setdiff(names(train_dummy_df), "SalePrice_Log")

# 2. Crear el objeto de preprocesamiento (SOLO con train_data)
preprocessParams <- preProcess(train_dummy_df[, predictors_cols], 
                               method = c("nzv", "medianImpute", "center", "scale"))

print(preprocessParams)

# 3. Aplicar transformaciones a Train, Val y Test usando parámetros de Train
train_norm <- predict(preprocessParams, train_dummy_df[, predictors_cols])
val_norm   <- predict(preprocessParams, val_dummy_df[, predictors_cols])
test_norm  <- predict(preprocessParams, test_dummy_df[, predictors_cols])

# 4. Volver a añadir la variable objetivo
train_norm$SalePrice_Log <- train_dummy_df$SalePrice_Log
val_norm$SalePrice_Log   <- val_dummy_df$SalePrice_Log
test_norm$SalePrice_Log  <- test_dummy_df$SalePrice_Log

# Verificación rápida
cat("Media de GrLivArea (Train):", mean(train_norm$GrLivArea, na.rm = TRUE), "\n")
cat("SD de GrLivArea (Train):", sd(train_norm$GrLivArea, na.rm = TRUE), "\n")

#######################################################################################################################################################################
# PCA, LASSO, RIDGE (TODO ESTO SE MANTIENE IGUAL)
#######################################################################################################################################################################

# Instalar librería glmnet si no está
if(!require(glmnet)) install.packages("glmnet")
library(glmnet)

################################################################################
# 1. GENERACIÓN DE COMPONENTES PRINCIPALES (PCA)
################################################################################

predictors_cols_norm <- setdiff(names(train_norm), "SalePrice_Log")

cat("Número de predictores después del preprocesamiento:", length(predictors_cols_norm), "\n")

# Calculamos PCA solo con el set de entrenamiento normalizado
pca_obj <- prcomp(train_norm[, predictors_cols_norm], center = FALSE, scale. = FALSE)

# --- INTERPRETACIÓN DE COMPONENTES PCA ---
pca_importance <- summary(pca_obj)$importance
cat("Varianza explicada por componentes:\n")
print(pca_importance[, 1:10])

# Loadings de los primeros 5 componentes
pca_loadings <- pca_obj$rotation[, 1:5]
cat("\nLoadings de los primeros 5 componentes (primeras 10 variables):\n")
print(pca_loadings[1:10, ])

# Decisión: Nos quedamos con los componentes que expliquen el 95% de la varianza
cum_var <- cumsum(pca_obj$sdev^2 / sum(pca_obj$sdev^2))
num_comp <- which(cum_var >= 0.95)[1]
cat("Número de componentes para 95% varianza:", num_comp, "\n")

# Proyectar los datos (Train, Val, Test) en el espacio de PCA
train_pca <- data.frame(predict(pca_obj, train_norm[, predictors_cols_norm]))[, 1:num_comp]
val_pca   <- data.frame(predict(pca_obj, val_norm[, predictors_cols_norm]))[, 1:num_comp]
test_pca  <- data.frame(predict(pca_obj, test_norm[, predictors_cols_norm]))[, 1:num_comp]

# Añadir la variable objetivo a los datasets de PCA
train_pca$SalePrice_Log <- train_norm$SalePrice_Log
val_pca$SalePrice_Log   <- val_norm$SalePrice_Log
test_pca$SalePrice_Log  <- test_norm$SalePrice_Log

################################################################################
# 2. ENTRENAMIENTO DE MODELOS (TODO SE MANTIENE IGUAL)
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
lambda_grid <- 10^seq(-3, 1, length = 100)

model_lasso <- train(SalePrice_Log ~ ., 
                     data = train_norm, 
                     method = "glmnet",
                     trControl = ctrl,
                     tuneGrid = expand.grid(alpha = 1, lambda = lambda_grid))

# --- MODELO 3: Ridge (Sobre datos originales normalizados) ---
cat("Entrenando Modelo 3: Ridge (Original)...\n")
model_ridge <- train(SalePrice_Log ~ ., 
                     data = train_norm, 
                     method = "glmnet",
                     trControl = ctrl,
                     tuneGrid = expand.grid(alpha = 0, lambda = lambda_grid))

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
results_df <- results_df[order(results_df$RMSE), ]

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