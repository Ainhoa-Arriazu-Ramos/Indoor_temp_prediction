#Vamos a mejorar la detección de alarmas

# 1. AJUSTAR EL UMBRAL DE PREDICCIÓN ================================
modelo_logit <- glm(alarma_real ~ Ext_T + Ext_RAD + Ext_T_1 + Ext_T_2 + Ext_T_3 +
                      Int_T_1 + Int_T_2 + Int_T_3,
                    data = train_data, family = binomial)


#Crear una columna con la probabilidad de alarma
test_data$prob_alarma <- predict(modelo_logit, newdata = test_data, type = "response")


umbrales <- seq(0, 1, by = 0.01)

metricas <- sapply(umbrales, function(umbral) {
  pred <- ifelse(test_data$prob_alarma > umbral, 1, 0)
  TP <- sum(pred == 1 & test_data$alarma_real == 1)
  TN <- sum(pred == 0 & test_data$alarma_real == 0)
  FP <- sum(pred == 1 & test_data$alarma_real == 0)
  FN <- sum(pred == 0 & test_data$alarma_real == 1)
  
  precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
  recall <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
  f1 <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0,
               NA, 2 * (precision * recall) / (precision + recall))
  
  c(precision = precision, recall = recall, F1 = f1)
})

metricas_df <- as.data.frame(t(metricas))
metricas_df$umbral <- umbrales


#Gráfico
library(ggplot2)
ggplot(metricas_df, aes(x = umbral)) +
  geom_line(aes(y = precision, color = "Precisión")) +
  geom_line(aes(y = recall, color = "Recall")) +
  geom_line(aes(y = F1, color = "F1-score")) +
  labs(title = "Métricas según umbral de decisión",
       y = "Valor de la métrica",
       x = "Umbral") +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme_minimal()

#Viendo el gráfico: 
test_data$alarma_test_ajus<- NULL
