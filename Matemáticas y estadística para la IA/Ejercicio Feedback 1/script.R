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
# LIMPIEZA DE LOS DATOS
#######################################################################################################################################################################
# Transformación de los NA no Nulos
df_clean <- df %>%
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

# Verificamos si quedan NAs "reales"
colSums(is.na(df_clean))[colSums(is.na(df_clean)) > 0]

##### Mapeo de las variables categóricas ordinales
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

# Seleccionamos variables categóricas ordinales que hemos transformado a numéricas
vars_to_check <- c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", 
                   "HeatingQC", "KitchenQual", "FireplaceQu", "GarageQual", 
                   "GarageCond", "PoolQC", "BsmtFinType1", "BsmtFinType2", "Functional")
df_subset <- df_clean[, c(vars_to_check, "SalePrice_Log")]
cor_matrix <- cor(df_subset, use = "complete.obs")
high_cor_cols <- names(which(abs(cor_matrix[,"SalePrice_Log"]) > 0.5))
high_cor_cols <- high_cor_cols[-5]
high_cor_cols

# Creamos una lista de gráficos (Boxplots)
plot_list <- list()

for(var in high_cor_cols){
  # Corrección:
  # 1. Usamos .data[[var]] para leer la columna cuyo nombre está en la variable 'var'
  # 2. Quitamos las comillas de SalePrice_Log para que lea la columna numérica
  
  p <- ggplot(df_clean, aes(x = factor(.data[[var]]), y = SalePrice_Log)) +
    geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
    labs(title = paste("Precio vs", var), x = "Calidad", y = "Log(Precio)") +
    theme_minimal()
  
  plot_list[[var]] <- p
}

#######################################################################################################################################################################
# División de datos
#######################################################################################################################################################################

#######################################################################################################################################################################
# Estandarización normalización
#######################################################################################################################################################################

#######################################################################################################################################################################
# PCA
#######################################################################################################################################################################