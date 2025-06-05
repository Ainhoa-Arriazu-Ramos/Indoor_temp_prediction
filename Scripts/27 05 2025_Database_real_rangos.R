# Cargar paquetes necesarios
install.packages("caret")  # solo si no está instalado aún

library(readxl)
library(dplyr)
library(randomForest)
library(caret)

#1. IMPORTAR BASE DE DATOS

#2. CAMBIAR NOMBRE
df_alarma <- Verano_diario_vivreales


#2. Crear variable de riesgo térmico en base a grados_hora
df_alarma <- df_alarma %>%
  mutate(
    riesgo = case_when(
      grados_hora <= 25 ~ "bajo",
      grados_hora > 25 & grados_hora <= 65 ~ "medio",
      grados_hora > 65 ~ "alto"
    ),
    riesgo = factor(riesgo, levels = c("bajo", "medio", "alto"))
  )

# 3. Seleccionar variables predictoras y objetivo
vars_modelo <- c("Ext_T", "Ext_RAD", "Int_T_1", "Int_T_2", "Int_T_3",
                 "Ext_T_1", "Ext_T_2", "Ext_T_3", "Delta_T", "riesgo")

df_modelo <- df_alarma %>%
  select(all_of(vars_modelo)) %>%
  na.omit()  # eliminar filas con NA

# 4. Separar en conjunto de entrenamiento y prueba
set.seed(123)
trainIndex <- createDataPartition(df_modelo$riesgo, p = 0.8, list = FALSE)
train_data <- df_modelo[trainIndex, ]
test_data <- df_modelo[-trainIndex, ]

# 5. Entrenar el modelo Random Forest
modelo_rf <- randomForest(riesgo ~ ., data = train_data, ntree = 500, importance = TRUE)

# 6. Predicción y evaluación
predicciones <- predict(modelo_rf, newdata = test_data)

conf_mat <- confusionMatrix(predicciones, test_data$riesgo)

# 7. Mostrar resultados
print(conf_mat)

# 8. Importancia de variables
varImpPlot(modelo_rf)



