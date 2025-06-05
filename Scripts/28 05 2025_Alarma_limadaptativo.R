#IMPORTAR BASE DE DATOS DE VIVIENDAS REALES ==============================================
dataset <- Verano_diario_vivreales







#CREAR ALARMA EN BASE A LIMITE ADAPTATIVO DIARIO EN 16798-1:2019 =========================
#1. CREAR LA VARIABLE: Límite diario en base al confort adaptativo EN 16798-1:2019
library(dplyr)

dataset <- dataset %>%
  mutate(trm = (1 - 0.8) * (Ext_T_1 + 0.8 * Ext_T_2 + 0.8^2 * Ext_T_3))
dataset <- dataset %>%
  mutate(limiteadap = (0.33 * trm)+21.8)

#2. CREAR VARIABLE: Alarma
#si:
#trm > 30 o
#Int_T > limiteadap 
dataset <- dataset %>%
  mutate(
    alarma_real = if_else(trm > 30 | Int_T > limiteadap, 1, 0)
  )

#Miramos cómo es la distribución dentro de la variable dicotómica "alarma"
table(dataset$alarma_real)






#MODELO PREDICTIVO Y DESEMPEÑO: RLM ==============================================
install.packages("rsample")
library(rsample)

set.seed(123)  # Para reproducibilidad
split <- initial_split(dataset, prop = 0.8)  # 80% train, 20% test
train_data <- training(split)
test_data <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_rlm <- lm(Int_T ~ Ext_T + Ext_RAD + 
                   Ext_T_1 + Ext_T_2 + Ext_T_3 
                 + Int_T_1 + Int_T_2 + Int_T_3, 
                 data = train_data)

summary(modelo_rlm)



# Predecir Int_T sobre los datos test
test_data$Int_T_pred <- predict(modelo_rlm, newdata = test_data)

#Calcular alarma usando la fórmula, pero con las variables de test:
test_data$alarma_test <- ifelse(test_data$trm > 30 | test_data$Int_T_pred > test_data$limiteadap, 1, 0)


#DESEMPEÑO

#Residuos
test_data$residuos_alarma <- test_data$alarma_real - test_data$alarma_test

# Tabla de contingencia para variable dicotomica (Alarma)
table(test_data$alarma_real, test_data$alarma_test)

#Gráfico de dispersion para variable alarma
library(ggplot2)
ggplot(test_data, aes(x = alarma_real, y = alarma_test)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5, color = "blue") +
  labs(x = "Alarma Real", y = "Alarma Predicha (alarma_test)",
       title = "Dispersión: Alarma Real vs Alarma Predicha") +
  scale_x_continuous(breaks = c(0,1)) +
  scale_y_continuous(breaks = c(0,1)) +
  theme_minimal()


# Crear tabla de contingencia
conf_mat <- table(Real = test_data$alarma_real, Predicho = test_data$alarma_test) %>% 
  as.data.frame()
# Graficar heatmap
ggplot(conf_mat, aes(x = Predicho, y = Real, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Matriz de Confusión Heatmap",
       x = "Alarma Predicha",
       y = "Alarma Real") +
  theme_minimal() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        plot.title=element_text(size=18, face="bold"))



# Métricas para variable continua (Int_T)
mae <- mean(abs(test_data$Int_T_pred - test_data$Int_T))
rmse <- sqrt(mean((test_data$Int_T_pred - test_data$Int_T)^2))
cat("MAE:", mae, "\nRMSE:", rmse)

#Gráfico de dispersion para variable Int_T
ggplot(test_data, aes(x = Int_T, y = Int_T_pred)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
  labs(title = "Dispersión: Temperatura Interior Real vs Predicha",
       x = "Temperatura Interior Real (°C)",
       y = "Temperatura Interior Predicha (°C)") +
  theme_minimal()




