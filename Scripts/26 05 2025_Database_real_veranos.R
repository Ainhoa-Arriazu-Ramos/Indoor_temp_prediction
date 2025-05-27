#CREAR LA BASE DE DATOS

#Importar las bases de datos

#Copiar dataset
viv_todo <- ClimateReady_10_min_interval
#Borrar columnas no necesarias
viv_todo <- viv_verano[, !(names(viv_verano) %in% c("tout", "co2", "tg", "ta2", "pm25"))]

#Reordenar columnas
viv_todo <- viv_todo[, c("10_minute_interval", "year", "month", "day", "hour",
                             "city", "dwell_numb", "room", "dwelling", 
                             "ta1", "rh")]

#Fusionamos la base de datos interior con exterior
# Convertir fechas
viv_todo$`10_minute_interval` <- as.POSIXct(viv_todo$`10_minute_interval`, format = "%Y-%m-%d %H:%M:%S")
Ext_2021_2022$Fecha <- as.POSIXct(Ext_2021_2022$Fecha, format = "%d-%m-%Y %H:%M")

# Fusionar por columna de fecha
viv_todo_completo <- merge(viv_todo, Ext_2021_2022, by.x = "10_minute_interval", by.y = "Fecha", all.x = TRUE)

# Borramos los dias que no tengan variables exteriores
viv_todo_completo <- na.omit(viv_todo_completo)

#Extraemos solo el verano
viv_verano <- subset(viv_todo_completo, 
                     month %in% c(5, 6, 7, 8, 9) & city == "Pamplona" & room == "Salon")

viv_verano <- viv_verano[, !(names(viv_verano) %in% c("10_minute_interval", "dwelling"))]

#Convertir en numero el contenido de esas columnas
viv_verano$dwell_numb <- as.numeric(viv_verano$dwell_numb)
viv_verano$Ext_T <- as.numeric(viv_verano$Ext_T)
viv_verano$Ext_RAD <- as.numeric(viv_verano$Ext_RAD)

#Rename
viv_verano <- viv_verano %>%
  rename(Int_RH = Int_rh)


#Combinamos para sacar las variables horariamente
library(dplyr)
viv_verano_horario <- viv_verano %>%
  group_by(year, month, day, hour, dwell_numb) %>%
  summarise(
    Int_T = mean(Int_T, na.rm = TRUE),
    Int_RH = mean(Int_RH, na.rm = TRUE), 
    Ext_T = mean(Ext_T, na.rm = TRUE),
    Ext_HR = mean(Ext_HR, na.rm = TRUE), 
    Ext_RAD = mean(Ext_RAD, na.rm = TRUE)
  ) %>%
  ungroup()



#SEVERIDAD DEL SOBRECALENTAMIENTO: grados_hora por dia por encima del 26ºC============================

#1: Calcular la variable: Grado-horas >26°C
library(dplyr)
library(tidyr)
library(lubridate)

viv_verano_horario <- viv_verano_horario %>%
  mutate( grados_hora = pmax(Int_T - 26, 0)  # si está por debajo de 26, cuenta como 0
  )

#2: Agrupar todas las variables por dia
viv_verano_diario <- viv_verano_horario %>%
  group_by(year, month, day, dwell_numb) %>%
   summarise(
    Int_T = mean(Int_T, na.rm = TRUE),
    Int_RH = mean(Int_RH, na.rm = TRUE), 
    Ext_T = mean(Ext_T, na.rm = TRUE),
    Ext_HR = mean(Ext_HR, na.rm = TRUE), 
    Ext_RAD = mean(Ext_RAD, na.rm = TRUE), 
    grados_hora = sum(grados_hora, na.rm = TRUE)
  ) %>%
  ungroup()

#3: Variables desfasadas
viv_verano_diario <- viv_verano_diario %>%
  mutate(
    Delta_T = Int_T - Ext_T,
    
    Int_T_1 = lag(Int_T, 1), 
    Int_T_2 = lag(Int_T, 2), 
    Int_T_3 = lag(Int_T, 3), 
    
    Ext_T_1 = lag(Ext_T, 1), 
    Ext_T_2 = lag(Ext_T, 2), 
    Ext_T_3 = lag(Ext_T, 3)
  )

#4: Quitar filas con entradas NA
viv_verano_diario <- na.omit(viv_verano_diario)

#5. Modelo de Random Forest ===================================================
set.seed(123)  # Para reproducibilidad

# Dividir en train/test (70% train)
n <- nrow(viv_verano_diario)
train_indices <- sample(1:n, size = 0.7 * n)

train_data <- viv_verano_diario[train_indices, ]
test_data  <- viv_verano_diario[-train_indices, ]

# Cargar paquete randomForest (instalar solo si no está)
# install.packages("randomForest")
library(randomForest)

# Entrenar Random Forest
modelo_rf_train <- randomForest(
  grados_hora ~ Ext_T + Ext_RAD + Int_T_1 + Int_T_2 + Int_T_3 +
    Ext_T_1 + Ext_T_2 + Ext_T_3 + Delta_T,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

print(modelo_rf_train)

# Predicciones en test
pred_rf <- predict(modelo_rf_train, newdata = test_data)

# Evaluar rendimiento
real <- test_data$grados_hora
mse_rf <- mean((pred_rf - real)^2)
rmse_rf <- sqrt(mse_rf)
r2_rf <- 1 - sum((real - pred_rf)^2) / sum((real - mean(real))^2)

cat("Random Forest - RMSE:", rmse_rf, " | R²:", r2_rf, "\n")
