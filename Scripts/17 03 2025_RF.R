install.packages("randomForest")
library(randomForest)

install.packages("caTools")
library(caTools)

# Dividir el conjunto de datos en entrenamiento y prueba (70%-30%)
set.seed(123)  # Para asegurar la replicabilidad
split <- sample.split(dataset_tem_daily_ARX$Temp, SplitRatio = 0.7)

# Crear los conjuntos de entrenamiento y prueba
train_data <- subset(dataset_tem_daily_ARX, split == TRUE)
test_data <- subset(dataset_tem_daily_ARX, split == FALSE)



# Entrenar un modelo de regresión lineal
modelo_lr <- lm(Temp ~ Outdoor_Temperature + Outdoor_GlobRadiation + 
                  In_temp_1 + In_temp_2 + In_temp_3 + 
                  Out_temp_1 + Out_temp_2 + Out_temp_3, 
                data = train_data)

# Resumen del modelo
summary(modelo_lr)

# Hacer predicciones con el conjunto de prueba
predicciones <- predict(modelo_lr, newdata = test_data)

# Ver las primeras predicciones
head(predicciones)

# Graficar predicciones vs. valores reales
plot(test_data$Temp, predicciones, main="Predicciones vs Valores Reales",
     xlab="Valores Reales (Temperatura)", ylab="Predicciones (Temperatura)",
     pch=19, col="blue")
abline(a=0, b=1, col="red", lwd=2)  # Línea de referencia
