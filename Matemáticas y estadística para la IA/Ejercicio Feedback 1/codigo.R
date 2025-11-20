# Instalar librerías si no las tienes
if(!require(tidyverse)) install.packages("tidyverse") # Manipulación y gráficos
if(!require(corrplot)) install.packages("corrplot")   # Matriz de correlaciones
if(!require(caret)) install.packages("caret")         # Machine Learning workflow
if(!require(moments)) install.packages("moments")     # Para calcular skewness (asimetría)
if(!require(gridExtra)) install.packages("gridExtra") # Organizar gráficos

# Cargar librerías
library(tidyverse)
library(corrplot)
library(caret)
library(moments)
library(gridExtra)

# Cargar datos (asumiendo que tienes train.csv en tu directorio de trabajo)
df <- read.csv("/home/marco/Master/Matemáticas y estadística para la IA/Ejercicio Feedback 1/train.csv", stringsAsFactors = FALSE)

# Verificación inicial
dim(df)
glimpse(df)

# 1. Limpieza específica basada en data_description.txt
# Para estas variables, NA significa "Ausencia de la característica", no dato perdido.

df_clean <- df %>%
  mutate(
    # Variables donde NA = "None" o "No Access"
    Alley = replace_na(Alley, "None"),             # [cite: 218]
    BsmtQual = replace_na(BsmtQual, "None"),       # [cite: 234]
    BsmtCond = replace_na(BsmtCond, "None"),       # [cite: 234]
    BsmtExposure = replace_na(BsmtExposure, "None"),
    BsmtFinType1 = replace_na(BsmtFinType1, "None"),
    BsmtFinType2 = replace_na(BsmtFinType2, "None"),
    FireplaceQu = replace_na(FireplaceQu, "None"), # [cite: 241]
    GarageType = replace_na(GarageType, "None"),   # [cite: 242]
    GarageFinish = replace_na(GarageFinish, "None"),
    GarageQual = replace_na(GarageQual, "None"),
    GarageCond = replace_na(GarageCond, "None"),
    PoolQC = replace_na(PoolQC, "None"),           # [cite: 244]
    Fence = replace_na(Fence, "None"),             # [cite: 245]
    MiscFeature = replace_na(MiscFeature, "None"), # [cite: 245]
    
    # Variables numéricas donde NA implica 0 (ej. si no hay garaje, el área es 0)
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

# Verificamos si quedan NAs "reales" (ej. LotFrontage suele tener NAs reales)
colSums(is.na(df_clean))[colSums(is.na(df_clean)) > 0]

# Análisis de SalePrice
# 1. Histograma y curva de densidad
p1 <- ggplot(df_clean, aes(x = SalePrice)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Distribución Original de SalePrice", x = "Precio ($)", y = "Densidad") +
  theme_minimal()

# 2. QQ-Plot para verificar normalidad visualmente
p2 <- ggplot(df_clean, aes(sample = SalePrice)) +
  stat_qq() + stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot (Original)") +
  theme_minimal()

# Visualización conjunta
grid.arrange(p1, p2, ncol = 2)

# Calcular Skewness (Asimetría)
cat("Asimetría original:", skewness(df_clean$SalePrice), "\n")
# Nota: Si es > 1, está muy sesgada y justifica la transformación logarítmica.

# Seleccionar solo variables numéricas
nums <- unlist(lapply(df_clean, is.numeric))
df_nums <- df_clean[, nums]

# Calcular matriz de correlación
cor_matrix <- cor(df_nums, use = "complete.obs")

# Filtrar para ver solo las que tienen alta correlación con SalePrice
# Esto simplifica el gráfico y ayuda al informe
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


# 1. Transformación Logarítmica de SalePrice
# Usamos log() que en R es logaritmo natural.
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


# 2. Imputación de LotFrontage basada en la mediana por vecindario
df_clean <- df_clean %>%
  group_by(Neighborhood) %>%
  mutate(LotFrontage = ifelse(is.na(LotFrontage),
                              median(LotFrontage, na.rm = TRUE),
                              LotFrontage)) %>%
  ungroup()

# 3. Imputación de restos menores (Electrical, MasVnrType) usando la Moda
# A veces quedan 1 o 2 valores nulos sueltos
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

df_clean$Electrical[is.na(df_clean$Electrical)] <- get_mode(df_clean$Electrical)
df_clean$MasVnrType[is.na(df_clean$MasVnrType)] <- get_mode(df_clean$MasVnrType)

# Verificación final de NAs
sum(is.na(df_clean)) # Debería ser 0


# Función para mapear calidades a números
qual_map <- c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)

# Variables que comparten esta escala exacta
qual_cols <- c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", 
               "HeatingQC", "KitchenQual", "FireplaceQu", 
               "GarageQual", "GarageCond", "PoolQC")

# Aplicar el mapeo
for(col in qual_cols){
  df_clean[[col]] <- as.numeric(recode(df_clean[[col]], !!!qual_map))
}

# También hay otras con escalas propias (ej. BsmtFinType1)
# GLQ = Good Living Quarters (6) ... Unf = Unfinished (1)
bsmt_fin_map <- c("None" = 0, "Unf" = 1, "LwQ" = 2, "Rec" = 3, "BLQ" = 4, "ALQ" = 5, "GLQ" = 6)
df_clean$BsmtFinType1 <- as.numeric(recode(df_clean$BsmtFinType1, !!!bsmt_fin_map))
df_clean$BsmtFinType2 <- as.numeric(recode(df_clean$BsmtFinType2, !!!bsmt_fin_map))

# Functional (Home functionality)
func_map <- c("Sal" = 0, "Sev" = 1, "Maj2" = 2, "Maj1" = 3, "Mod" = 4, 
              "Min2" = 5, "Min1" = 6, "Typ" = 7)
df_clean$Functional <- as.numeric(recode(df_clean$Functional, !!!func_map))

# Verificar que se han convertido a numéricas
str(df_clean[, qual_cols])


# Eliminar columnas innecesarias
# 'Id' no predice nada. 'Utilities' suele ser constante en este dataset (casi todas AllPub).
df_model <- df_clean %>% select(-Id, -Utilities)

# Crear variables Dummy para las categóricas restantes (Nominales)
# Usamos dummyVars de caret, que gestiona esto muy bien
dummies_model <- dummyVars(~ ., data = df_model)
df_processed <- predict(dummies_model, newdata = df_model)
df_processed <- as.data.frame(df_processed)

# IMPORTANTE: Al crear dummies, la variable SalePrice y SalePrice_Log también están ahí.
# Asegurémonos de que las dimensiones sean correctas.
dim(df_processed)


# Seleccionamos algunas de las variables más importantes que convertimos
vars_to_check <- c("ExterQual", "KitchenQual", "BsmtQual", "HeatingQC")

# Creamos una lista de gráficos (Boxplots)
plot_list <- list()

for(var in vars_to_check){
  # Corrección:
  # 1. Usamos .data[[var]] para leer la columna cuyo nombre está en la variable 'var'
  # 2. Quitamos las comillas de SalePrice_Log para que lea la columna numérica
  
  p <- ggplot(df_clean, aes(x = factor(.data[[var]]), y = SalePrice_Log)) +
    geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
    labs(title = paste("Precio vs", var), x = "Calidad (1-5)", y = "Log(Precio)") +
    theme_minimal()
  
  plot_list[[var]] <- p
}

# Visualizar en una cuadrícula 2x2
grid.arrange(grobs = plot_list, ncol = 2)

# --- Validación Numérica (Correlación) ---
# Verificamos qué tan fuerte es la relación lineal ahora que son números
new_numeric_cors <- cor(df_clean[, vars_to_check], df_clean$SalePrice_Log, use = "complete.obs")
print("Correlación de variables convertidas con SalePrice_Log:")
print(new_numeric_cors)


# --- FASE 3: PREPARACIÓN PARA MODELOS ---

# 1. Preparamos el dataset final
# Nos aseguramos de tener solo variables numéricas (las dummies y las ordinales convertidas)
# Excluimos 'SalePrice' y 'SalePrice_Log' de las predictoras (X)
df_final <- df_processed
target_log <- df_clean$SalePrice_Log # Guardamos el target transformado

# Comprobamos dimensiones
dim(df_final)

# 2. División Train / Test (80% Train - 20% Test)
set.seed(123) # Semilla para reproducibilidad
trainIndex <- createDataPartition(target_log, p = .8, 
                                  list = FALSE, 
                                  times = 1)

# Creamos los dataframes de X (predictoras) e Y (target)
X_train_raw <- df_final[ trainIndex, ] %>% select(-SalePrice, -SalePrice_Log)
X_test_raw  <- df_final[-trainIndex, ] %>% select(-SalePrice, -SalePrice_Log)

y_train <- target_log[trainIndex]
y_test  <- target_log[-trainIndex]

# 3. Estandarización (Scaling)
# IMPORTANTE: "Aprendemos" la media y desviación SOLO del Train set
preProcValues <- preProcess(X_train_raw, method = c("center", "scale"))

# Aplicamos esa transformación a Train y a Test
X_train_scaled <- predict(preProcValues, X_train_raw)
X_test_scaled  <- predict(preProcValues, X_test_raw)

# Verificación: La media de train debe ser 0 (o casi 0)
# mean(X_train_scaled$GrLivArea) 

# --- FASE 3.1: PCA (Análisis de Componentes Principales) ---

# Calculamos PCA sobre los datos de entrenamiento escalados
pca_model <- prcomp(X_train_scaled, center = FALSE, scale. = FALSE) 
# (Ya escalamos antes, por eso ponemos FALSE aquí)

# Resumen de varianza explicada
pca_summary <- summary(pca_model)

# VISUALIZACIÓN: Scree Plot (Varianza acumulada)
# Esto nos ayuda a decidir cuántos componentes quedarnos
var_explained <- pca_model$sdev^2 / sum(pca_model$sdev^2)
cum_var <- cumsum(var_explained)

# Creamos un dataframe para el gráfico
pca_plot_df <- data.frame(
  Component = 1:length(cum_var),
  CumulativeVariance = cum_var
)

# Gráfico de codo (Scree Plot Acumulado)
ggplot(pca_plot_df[1:100,], aes(x = Component, y = CumulativeVariance)) +
  geom_line(color = "blue", size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.90, linetype = "dashed", color = "red") +
  annotate("text", x = 0, y = 0.95, label = "90% Varianza", color = "red", hjust = -0.1) +
  labs(title = "Varianza Explicada Acumulada por PCA", x = "Componentes", y = "Varianza") +
  theme_minimal()

# Ver cuántos componentes necesitamos para explicar el 90% o 95%
num_comp_90 <- which(cum_var >= 0.90)[1]
cat("Número de componentes para explicar el 90% de la varianza:", num_comp_90, "\n")

# --- MODELO 1: OLS + PCA ---

# 1. Preparamos los datos proyectados (Solo tomamos los primeros 122 componentes)
k <- 122
X_train_pca <- as.data.frame(pca_model$x[, 1:k])
X_test_pca  <- as.data.frame(predict(pca_model, newdata = X_test_scaled)[, 1:k])

# Añadimos la variable objetivo para entrenar el modelo lineal
train_data_pca <- cbind(X_train_pca, SalePrice_Log = y_train)

# 2. Entrenamos la Regresión Lineal
model_pca_lm <- lm(SalePrice_Log ~ ., data = train_data_pca)

# 3. Predecimos
preds_pca <- predict(model_pca_lm, newdata = X_test_pca)

# 4. Evaluamos (RMSE en escala logarítmica)
rmse_pca <- RMSE(preds_pca, y_test)
cat("RMSE Modelo PCA + Regresión Lineal:", rmse_pca, "\n")

library(glmnet)

# glmnet necesita matrices, no dataframes
X_train_mat <- as.matrix(X_train_scaled)
X_test_mat  <- as.matrix(X_test_scaled)

# --- MODELO 2: LASSO (Alpha = 1) ---
set.seed(123)
# cv.glmnet busca automáticamente el lambda óptimo
lasso_cv <- cv.glmnet(X_train_mat, y_train, alpha = 1)
best_lambda_lasso <- lasso_cv$lambda.min

# Predecir con el mejor lambda
preds_lasso <- predict(lasso_cv, s = best_lambda_lasso, newx = X_test_mat)
rmse_lasso <- RMSE(preds_lasso, y_test)

cat("Mejor Lambda Lasso:", best_lambda_lasso, "\n")
cat("RMSE Modelo Lasso:", rmse_lasso, "\n")


# --- MODELO 3: RIDGE (Alpha = 0) ---
set.seed(123)
ridge_cv <- cv.glmnet(X_train_mat, y_train, alpha = 0)
best_lambda_ridge <- ridge_cv$lambda.min

# Predecir
preds_ridge <- predict(ridge_cv, s = best_lambda_ridge, newx = X_test_mat)
rmse_ridge <- RMSE(preds_ridge, y_test)

cat("Mejor Lambda Ridge:", best_lambda_ridge, "\n")
cat("RMSE Modelo Ridge:", rmse_ridge, "\n")

# --- MODELO 4: PCA + REGULARIZACIÓN (Lasso y Ridge sobre los Componentes) ---

# Convertimos los datos PCA a matriz (necesario para glmnet)
# Recuerda: X_train_pca ya tiene las 122 columnas seleccionadas
X_train_pca_mat <- as.matrix(X_train_pca)
X_test_pca_mat  <- as.matrix(X_test_pca)

# 4.1 PCA + LASSO
set.seed(123)
pca_lasso_cv <- cv.glmnet(X_train_pca_mat, y_train, alpha = 1)
best_lambda_pca_lasso <- pca_lasso_cv$lambda.min

preds_pca_lasso <- predict(pca_lasso_cv, s = best_lambda_pca_lasso, newx = X_test_pca_mat)
rmse_pca_lasso <- RMSE(preds_pca_lasso, y_test)

cat("RMSE Modelo PCA + Lasso:", rmse_pca_lasso, "\n")

# 4.2 PCA + RIDGE
set.seed(123)
pca_ridge_cv <- cv.glmnet(X_train_pca_mat, y_train, alpha = 0)
best_lambda_pca_ridge <- pca_ridge_cv$lambda.min

preds_pca_ridge <- predict(pca_ridge_cv, s = best_lambda_pca_ridge, newx = X_test_pca_mat)
rmse_pca_ridge <- RMSE(preds_pca_ridge, y_test)

cat("RMSE Modelo PCA + Ridge:", rmse_pca_ridge, "\n")