#Taller 2 BdMl

# 0.Cargar datos  =========
install.packages("pacman")
library(pacman)
p_load(readr,tidyverse,googledrive, skimr, naniar, dplyr, caret, themis, recipes, janitor)


##Importación de datos desde dropbox##
train_hogares <- read.csv("https://www.dropbox.com/scl/fi/y4gn7m3xc196b4t3muf50/train_hogares.csv?rlkey=pclg0plogyqvvio8qgzq8w478&st=ints36kh&dl=1") %>% 
  clean_names()
train_personas <- read.csv("https://www.dropbox.com/scl/fi/ztw34qzjaatqka2mqmop9/train_personas.csv?rlkey=cm1jj1wi2j5z8n2i21gwjnyec&st=saabj5rr&dl=1") %>% 
  clean_names()
test_personas <- read.csv("https://www.dropbox.com/scl/fi/llb9izc4i7ef11z5di92i/test_personas.csv?rlkey=vfaol826sceevmgg9ra6zvwe7&st=oxwg3o4x&dl=1") %>% 
  clean_names()
test_hogares <- read.csv("https://www.dropbox.com/scl/fi/rgox9tm1geg7js959rzza/test_hogares.csv?rlkey=5esm82u2z1lz7iidkhxcqgfvy&st=wrx2n2id&dl=1") %>% 
  clean_names()



# 0.1 Selección de variables ============

# 0.1.2 Variables relevantes Hogares
#Etiquetas Variables Hogares:
# Diccionario variables
diccionario_hogares <- c(
  "P5000" = "n_cuartos",
  "P5010" = "cuartos_dormir",
  "P5090" = "tiene_vivienda",
  "P5100" = "cuota_amortizacion",
  "P5130" = "arriendo_estimado",
  "P5140" = "arriendo_mensual"
)

# Aplicar cambios
names(train_hogares)[names(train_hogares) %in% names(diccionario_hogares)] <- 
  diccionario_hogares[names(train_hogares)[names(train_hogares) %in% names(diccionario_hogares)]]
names(test_hogares)[names(test_hogares) %in% names(diccionario_hogares)] <- 
  diccionario_hogares[names(test_hogares)[names(test_hogares) %in% names(diccionario_hogares)]]


# 0.1.3 Variables relevantes Personas
# Etiquetas Variables Hogares:
# Diccionario variables

diccionario_personas <- c(
  "P6020" = "Sexo",
  "P6040" = "Edad",
  "P6050" = "Jefe_hogar",
  "P6090" = "SS_salud", #SS=Seguridad social
  "P6100" = "Régimen_SS_salud",
  "P6210" = "Nivel_educ", 
  "P6210s1" = "Grado_aprobado",
  "P6240" = "Act_principal_SP", #SP=Semana pasada
  "P6426" = "T_Tra_Emp", #Tiempo trabajado en la empresa (meses)
  "P6430" = "Pos_tra_pri", #Posición trabajo principal
  "P6510" = "Ing_HE", #Ingresos por horas extras
  "P6545" = "Ing_Pr", #Ingresos por primas
  "P6580" = "Ing_Bon", #Ingresos por Bonificaciones
  "P6585s1" = "Sub_Ali", #Subsidio alimentación
  "P6585s2" = "Sub_Trans",# Subsidio transporte
  "P6585s3" = "Sub_Fam", # Subsidio familiar
  "P6585s4" = "Sub_Edu",# Subsidio educativo
  "P6590" = "Ing_esp_ali",# Ingresos en especie por alimentación
  "P6600" = "Ing_esp_viv",# Ingresos en especie por vivienda
  "P6610" = "Trans_emp", # Uso transporte de la empresa
  "P6620" = "Ing_esp_otros",# Otros ingresos en especie
  "P6630s1" = "Pri_serv_12m", # Prima de servicios últimos 12 meses
  "P6630s2" = "Pri_nav_12m",# Prima de navidad últimos 12 meses
  "P6630s3" = "Pri_vac_12m",# Prima de vacaciones últimos 12 meses
  "P6630s4" = "Viat_per_12m", # Viáticos permanentes últimos 12 meses
  "P6630s6" = "Bon_anual_12m",# Bonificaciones anuales últimos 12 meses
  "P6800" = "Hras_sem_trab", # Horas trabajadas normalmente a la semana
  "P6870" = "Tam_empresa",# Tamaño de la empresa donde trabaja
  "P6920" = "Cot_pension",# Cotiza a fondo de pensiones
  "P7040" = "Seg_trab_SP",# Tuvo segundo trabajo la semana pasada
  "P7045" = "Hras_seg_trab",# Horas trabajadas en segundo trabajo
  "P7050" = "Pos_tra_sec",  # Posición ocupacional en segundo trabajo
  "P7090" = "Quiere_mas_horas",# Quiere trabajar más horas
  "P7110" = "Diligencias_mas_horas",# Hizo diligencias para trabajar más horas
  "P7120" = "Disp_mas_horas", # Disponible para trabajar más horas
  "P7150" = "Dilig_camb_trab",# Hizo diligencias para cambiar de trabajo
  "P7160" = "Disp_camb_trab", # Podría empezar nuevo trabajo antes de un mes
  "P7310" = "Busq_trab_primera",# Buscó trabajo por primera vez o había trabajado antes
  "P7350" = "Pos_ult_trab", # Posición ocupacional en último trabajo (desocupados)
  "P7422" = "Ing_trab_mes_desoc", # Ingresos por trabajo mes pasado (desocupados)
  "P7472" = "Ing_trab_mes_desoc2",# Ingresos por trabajo mes pasado (desocupados) - segunda pregunta
  "P7495" = "Ing_arriendo_pension", # Ingresos por arriendos y/o pensiones
  "P7500s2" = "Ing_pension_jub",# Ingresos por pensiones o jubilaciones
  "P7500s3" = "Ing_pension_ali",# Ingresos por pensión alimenticia
  "P7505" = "Ing_no_lab_12m", # Ingresos no laborales últimos 12 meses
  "P7510s1" = "Ing_din_hog_nac",# Ingresos por dinero de otros hogares en el país
  "P7510s2" = "Ing_din_hog_ext",# Ingresos por dinero de otros hogares fuera del país
  "P7510s3" = "Ing_ayuda_inst", # Ingresos por ayudas de instituciones
  "P7510s5" = "Ing_interes_div",# Ingresos por intereses, dividendos, utilidades
  "P7510s6" = "Ing_cesantias",  # Ingresos por cesantías e intereses
  "P7510s7" = "Ing_otras_fuentes" # Ingresos de otras fuentes
)

#Aplicar cambios
names(test_personas)[names(test_personas) %in% names(diccionario_personas)] <- 
  diccionario_personas[names(test_personas)[names(test_personas) %in% names(diccionario_personas)]]
names(train_personas)[names(train_personas) %in% names(diccionario_personas)] <- 
  diccionario_personas[names(train_personas)[names(train_personas) %in% names(diccionario_personas)]]

#Variables irrelevantes
#vars_a_eliminar <- c(
  #"P6500", "P6510s1", "P6510s2", "P6545s1", "P6545s2",
  #"P6580s1", "P6580s2", "P6585s1a1", "P6585s1a2", "P6585s2a1",
  #"P6585s2a2", "P6585s3a1", "P6585s3a2", "P6585s4a1", "P6585s4a2",
  #"P6590s1", "P6600s1", "P6610s1", "P6620s1", "P6630s1a1",
  #"P6630s2a1", "P6630s3a1", "P6630s4a1", "P6630s6a1", "P6750",
  #"P6760", "P550", "P7070", "P7140s1", "P7140s2", "P7422s1", "P7472s1", 
  #"P7500s1", "P7500s1a1","P7500s2a1", "P7500s3a1", "P7510s1a1", 
  #"P7510s2a1", "P7510s3a1","P7510s5a1", "P7510s6a1", "P7510s7a1",
  #"Cclasnr2", "Cclasnr3", "Cclasnr4", "Cclasnr5", "Cclasnr6", "Cclasnr7", "Cclasnr8", "Cclasnr11"
#)

#train_personas <- train_personas %>% 
  #select(-all_of(vars_a_eliminar))


# #variables_finales <- c(
#   # CLAVES PARA UNIR BASES
#   "id", "Orden", "Clase", "Dominio", "Fex_c", "Depto", "Fex_dpto",
#   
#   # VARIABLES PREDICTORAS (solo las que están en AMBOS)
#   "Oc",                  # Ocupado
#   "Nivel_educ",          # Nivel educativo
#   "Edad",                # Edad
#   "Pos_tra_pri",         # Posición ocupacional
#   "Cot_pension",         # Cotiza a pensiones
#   "SS_salud",            # Afiliación salud
#   "Hras_sem_trab",       # Horas trabajadas
#   "Jefe_hogar",          # Parentesco
#   "Act_principal_SP",    # Actividad principal
#   "T_Tra_Emp",           # Tiempo en empresa
#   "Ing_HE",              # Ingreso horas extras
#   "Sub_Trans",           # Subsidio transporte
#   "Pet",                 # Población edad trabajar
#   "Ina",                 # Inactivo
#   "Tam_empresa",         # Tamaño empresa
#   "Régimen_SS_salud",    # Régimen salud
#   "Grado_aprobado",      # Grado aprobado
#   "Sexo",                # Sexo
#   "Des"                  # Desocupado (agregamos para completar)
# )

# Para train_personas
#train_personas <- train_personas[, variables_finales]
# Para test_personas (mismas variables)
#test_personas <- test_personas[, variables_finales]


table(train_personas$Cot_pension)



# 1. Datos - Análisis variables Personas/Hogares===============
# Arreglos

# Modificar la función pre_process_personas para recodificar Cot_pension
pre_process_personas <- function(data) {
  data <- data |> 
    mutate(
      Sexo = ifelse(Sexo == 2, 1, 0),
      Jefe_hogar = ifelse(Jefe_hogar == 1, 1, 0),
      Niños = ifelse(Edad <= 6, 1, 0),
      Nivel_educ = ifelse(Nivel_educ == 9, 0, Nivel_educ),
      Oc = ifelse(is.na(Oc), 0, 1),
      Ina = ifelse(is.na(Ina), 0, 1), # 1=Inactivo, 0=Activo (NA→0)
      # Nueva recodificación para Cot_pension
      Cot_pension = case_when(
        Cot_pension == 1 ~ 1,  # Sí cotiza
        Cot_pension == 3 ~ 1,  # Ya pensionado, lo tratamos como 1
        Cot_pension == 2 ~ 0,  # No cotiza
        TRUE ~ 0  # Cualquier otro caso (NA, etc.) → 0
      )
    )
  return(data)
}

# Aplicar el preprocesamiento actualizado
train_personas <- pre_process_personas(train_personas)
test_personas <- pre_process_personas(test_personas)



#Variables de persona agregadas por hogar TRAIN

TR_personas_nivel_hogar <- train_personas |> 
  group_by(id) |>
  summarize(
    num_women    = sum(Sexo, na.rm = TRUE),
    num_minors   = sum(Niños, na.rm = TRUE),
    cat_maxEduc  = max(Nivel_educ, na.rm = TRUE),
    num_occupied = sum(Oc, na.rm = TRUE),
    # NUEVAS VARIABLES:
    num_inactivos = sum(Ina, na.rm = TRUE),  # Total de inactivos
    num_cotizantes = sum(Cot_pension, na.rm = TRUE)  # Total que cotizan/pensionados
  ) |> 
  ungroup()

##Variables por jefe del hogar Train:
TR_personas_nivel_hogar <- train_personas |> 
  filter(Jefe_hogar == 1) |>
  select(id, Sexo, Nivel_educ, Oc) |>
  rename(bin_headWoman = Sexo,
         bin_occupiedHead = Oc) |>
  left_join(TR_personas_nivel_hogar)

#Variables de persona agregadas por hogar TEST

TE_personas_nivel_hogar <- test_personas |> 
  group_by(id) |>
  summarize(
    num_women    = sum(Sexo, na.rm = TRUE),
    num_minors   = sum(Niños, na.rm = TRUE),
    cat_maxEduc  = max(Nivel_educ, na.rm = TRUE),
    num_occupied = sum(Oc, na.rm = TRUE),
    # NUEVAS VARIABLES:
    num_inactivos = sum(Ina, na.rm = TRUE),  # Total de inactivos
    num_cotizantes = sum(Cot_pension, na.rm = TRUE)  # Total que cotizan/pensionados
  ) |> 
  ungroup()

##Variables por jefe del hogar Test:
TE_personas_nivel_hogar <- test_personas |> 
  filter(Jefe_hogar == 1) |>
  select(id, Sexo, Nivel_educ, Oc) |>
  rename(bin_headWoman = Sexo,
         bin_occupiedHead = Oc) |>
  left_join(TE_personas_nivel_hogar)


#Arreglos a nivel hogar:

train_hogares <- train_hogares |>
  mutate(
    bin_rent = ifelse(tiene_vivienda == 3, 1, 0),
    prop_cuartos = n_cuartos / Nper,
    prop_cuartos_dormir = cuartos_dormir / Nper
  ) |>
  select(id, tiene_vivienda, Pobre, n_cuartos, cuartos_dormir, Nper, 
         prop_cuartos, prop_cuartos_dormir, bin_rent)

test_hogares <- test_hogares |>
  mutate(
    bin_rent = ifelse(tiene_vivienda == 3, 1, 0),
    prop_cuartos = n_cuartos / Nper,
    prop_cuartos_dormir = cuartos_dormir / Nper
  ) |>
  select(id, tiene_vivienda, n_cuartos, cuartos_dormir, Nper, 
         prop_cuartos, prop_cuartos_dormir, bin_rent)


# Crear las variables de proporción en la unión con hogares:
train <- train_hogares |> 
  left_join(TR_personas_nivel_hogar) |>
  mutate(
    # PROPORCIONES EXISTENTES:
    prop_inactivos = num_inactivos / Nper,
    prop_cotizantes = num_cotizantes / Nper,
    prop_ocupados = num_occupied / Nper,
    
    # NUEVA VARIABLE AVANZADA:
    vulnerability_index = (
      (1 - prop_ocupados) +           # Desempleo
        (num_minors / Nper) +           # Carga de menores  
        (1 - prop_cotizantes) +         # Exclusión financiera
        (1 / (prop_cuartos + 0.1))      # Hacinamiento inverso
    ) / 4                             # Normalizar 0-1
  ) |>
  select(-id) # Solo eliminar en train


test <- test_hogares |> 
  left_join(TE_personas_nivel_hogar) |>
  mutate(
    # PROPORCIONES EXISTENTES:
    prop_inactivos = num_inactivos / Nper,
    prop_cotizantes = num_cotizantes / Nper,
    prop_ocupados = num_occupied / Nper,
    
    # NUEVA VARIABLE AVANZADA:
    vulnerability_index = (
      (1 - prop_ocupados) +           # Desempleo
        (num_minors / Nper) +           # Carga de menores  
        (1 - prop_cotizantes) +         # Exclusión financiera
        (1 / (prop_cuartos + 0.1))      # Hacinamiento inverso
    ) / 4                             # Normalizar 0-1
  )
train <- train |> 
  mutate(prop_ocupados = num_occupied / Nper)

test <- test |> 
  mutate(prop_ocupados = num_occupied / Nper)


train <- train |> 
  mutate(Pobre   = factor(Pobre,levels=c(0,1))
  )

# 3. Modelo RF sin balanceo de muestras =================

ctrl <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  savePredictions = TRUE
)

set.seed(2025)

model_rf <- train(
  Pobre ~ .,
  data = train,
  method = "rf",
  trControl = ctrl,
  metric = "Accuracy",
  tuneGrid = expand.grid(mtry = c(3, 5, 7))  # Número de variables por split
)



# 3.2.1 Modelo RF con Down-Sampling N_tree 100 =================
# Definir función F1 para la CV INTERNA

f1_summary <- function(data, lev = NULL, model = NULL) {
  confusion <- caret::confusionMatrix(data$pred, data$obs)
  sensitivity <- confusion$byClass["Sensitivity"]
  precision <- confusion$byClass["Pos Pred Value"]
  f1 <- 2 * (precision * sensitivity) / (precision + sensitivity)
  
  c(F1 = f1,
    Sensibilidad = sensitivity,
    Precision = precision,
    Accuracy = confusion$overall["Accuracy"])
}

# Configurar control con F1 para CV
# Configuración SUPER rápida
ctrl_fast <- trainControl(
  method = "cv",
  number = 3,                    # Solo 3 folds (más rápido)
  classProbs = FALSE,
  savePredictions = FALSE,       # No guarden las predicciones para que les corra
                                 # Más rápido 
  sampling = "down",
  summaryFunction = f1_summary,
  verboseIter = FALSE           # Esto hace que se demore menos
)

set.seed(2025)

model_rf_fast <- train(
  Pobre ~ .,
  data = train,
  method = "rf",
  trControl = ctrl_fast,
  metric = "F1",
  tuneGrid = expand.grid(mtry = c(3, 5)), 
  ntree = 100,                   
  importance = FALSE,            
  do.trace = FALSE             
)


#Predicciones:
predicciones_test <- predict(model_rf_fast, newdata = test)
# Submission
submission <- data.frame(
  id = test$id,
  poverty = as.numeric(predicciones_test) - 1  
)
ruta_descargas <- "C:/Users/Marlon Angulo/Downloads"
best_mtry <- model_rf_fast$bestTune$mtry
nombre_archivo <- paste0("RF_mtry_", best_mtry, "_ntree_100_sampling_down.csv")
ruta_completa <- file.path(ruta_descargas, nombre_archivo)
# Guardar el submission
write.csv(submission, ruta_completa, row.names = FALSE)



predicciones_model_rf_fast_down <- predict(model_rf_fast, newdata = train)
confusion_matrix <- confusionMatrix(predicciones_model_rf_fast_down, train$Pobre)
print(confusion_matrix)


# 3.2.2 Modelo RF con Smoote-Sampling N_tree 100 =================
# Definir función F1 para la CV INTERNA


f1_summary <- function(data, lev = NULL, model = NULL) {
  confusion <- caret::confusionMatrix(data$pred, data$obs)
  sensitivity <- confusion$byClass["Sensitivity"]
  precision <- confusion$byClass["Pos Pred Value"]
  f1 <- 2 * (precision * sensitivity) / (precision + sensitivity)
  
  c(F1 = f1,
    Sensibilidad = sensitivity,
    Precision = precision,
    Accuracy = confusion$overall["Accuracy"])
}

# Configurar control con F1 para CV
# Configuración SUPER rápida
ctrl_fast <- trainControl(
  method = "cv",
  number = 3,                    # Solo 3 folds (más rápido)
  classProbs = FALSE,
  savePredictions = FALSE,       # No guarden las predicciones para que les corra
  # Más rápido 
  sampling = "smoote",
  summaryFunction = f1_summary,
  verboseIter = FALSE           # Esto hace que se demore menos
)

set.seed(2025)

model_rf_fast <- train(
  Pobre ~ .,
  data = train,
  method = "rf",
  trControl = ctrl_fast,
  metric = "F1",
  tuneGrid = expand.grid(mtry = c(3, 5)), 
  ntree = 100,                   
  importance = FALSE,            
  do.trace = FALSE             
)



#Predicciones:
predicciones_test <- predict(model_rf_fast, newdata = test)
# Submission
submission <- data.frame(
  id = test$id,
  poverty = as.numeric(predicciones_test) - 1  
)
ruta_descargas <- "C:/Users/Marlon Angulo/Downloads"
best_mtry <- model_rf_fast$bestTune$mtry
nombre_archivo <- paste0("RF_mtry_", best_mtry, "_ntree_100_sampling_down.csv")
ruta_completa <- file.path(ruta_descargas, nombre_archivo)
# Guardar el submission
write.csv(submission, ruta_completa, row.names = FALSE)



predicciones_model_rf_fast <- predict(model_rf_fast, newdata = train)
confusion_matrix <- confusionMatrix(model_rf_fast, train$Pobre)
print(confusion_matrix)

# 3.2.3 Modelo  RF con Up-Sampling N_tree 150 =================

f1_summary <- function(data, lev = NULL, model = NULL) {
  confusion <- caret::confusionMatrix(data$pred, data$obs)
  sensitivity <- confusion$byClass["Sensitivity"]
  precision <- confusion$byClass["Pos Pred Value"]
  f1 <- 2 * (precision * sensitivity) / (precision + sensitivity)
  
  c(F1 = f1,
    Sensibilidad = sensitivity,
    Precision = precision,
    Accuracy = confusion$overall["Accuracy"])
}

# Configurar control con F1 para CV
# Configuración SUPER rápida
ctrl_fast_up <- trainControl(
  method = "cv",
  number = 3,                    # Solo 3 folds (más rápido)
  classProbs = FALSE,
  savePredictions = FALSE,       # No guarden las predicciones para que les corra
  # Más rápido 
  sampling = "up",
  summaryFunction = f1_summary,
  verboseIter = FALSE           # Esto hace que se demore menos
)

set.seed(2025)

model_rf_fast_up <- train(
  Pobre ~ .,
  data = train,
  method = "rf",
  trControl = ctrl_fast_up,
  metric = "F1",
  tuneGrid = expand.grid(mtry = c(3, 5)), 
  ntree = 100,                   
  importance = FALSE,            
  do.trace = FALSE             
)


predicciones_model_rf_up <- predict(model_rf_fast_up, newdata = train)
confusion_matrix <- confusionMatrix(predicciones_model_rf_up, train$Pobre)
print(confusion_matrix)

#Predicciones:
predicciones_test_up <- predict(model_rf_fast_up, newdata = test)
# Submission
submission_up <- data.frame(
  id = test$id,
  poverty = as.numeric(predicciones_test_up) - 1  
)
ruta_descargas <- "C:/Users/Marlon Angulo/Downloads"
best_mtry <- model_rf_fast_up$bestTune$mtry
nombre_archivo <- paste0("RF_mtry_", best_mtry, "_ntree_100_sampling_up.csv")
ruta_completa <- file.path(ruta_descargas, nombre_archivo)
# Guardar el submission
write.csv(submission_up, ruta_completa, row.names = FALSE)



# 3.3.4 Modelo RF con Down-Sampling N_tree 150 ===============

# Función F1 personalizada que penaliza más los Falsos Negativos
f1_weighted <- function(data, lev = NULL, model = NULL) {
  confusion <- caret::confusionMatrix(data$pred, data$obs)
  
  # Extraer métricas clave
  sensitivity <- confusion$byClass["Sensitivity"]  # Detección de pobres (clase 1)
  precision <- confusion$byClass["Pos Pred Value"]
  specificity <- confusion$byClass["Specificity"]
  
  # Calcular F1 tradicional
  f1_trad <- 2 * (precision * sensitivity) / (precision + sensitivity)
  
  # Penalización por Falsos Negativos (pobres no detectados)
  false_negatives <- confusion$table[1, 2]  # FNs: Reference=1, Prediction=0
  total_positives <- sum(confusion$table[, 2])  # Total pobres reales
  fn_ratio <- false_negatives / total_positives
  
  # F1 weighted: castigar más los Falsos Negativos
  f1_weighted <- f1_trad * (1 - fn_ratio * 0.5)  # Reducción del 50% por FNs
  
  c(F1_weighted = f1_weighted,
    F1_trad = f1_trad,
    Sensitivity = sensitivity,
    Precision = precision,
    Specificity = specificity,
    FN_Count = false_negatives,
    FN_Ratio = fn_ratio)
}


# Configuración de trainControl con F1 weighted
ctrl_weighted <- trainControl(
  method = "cv",
  number = 3,
  classProbs = FALSE,
  savePredictions = FALSE,
  sampling = "smote",
  summaryFunction = f1_weighted,  # Usar nuestra función personalizada
  verboseIter = FALSE
)

set.seed(2025)

# Entrenar modelo con mtry = 2 y F1 weighted
model_rf_weighted <- train(
  Pobre ~ .,
  data = train,
  method = "rf",
  trControl = ctrl_weighted,
  metric = "F1_weighted",  # Optimizar por F1 weighted
  tuneGrid = expand.grid(mtry = 2),  # Solo mtry = 2 para más sensibilidad
  ntree = 150,             # Un poco más de árboles
  importance = FALSE,
  do.trace = FALSE
)


# Predecir en test con el nuevo modelo
predicciones_test_weighted <- predict(model_rf_weighted, newdata = test)

# Crear submission
submission_weighted <- data.frame(
  id = test$id,
  poverty = as.numeric(predicciones_test_weighted) - 1
)

# Guardar con nombre descriptivo
nombre_weighted <- "RF_mtry_2_ntree_150_F1weighted.csv"
write.csv(submission_weighted, 
          file.path("C:/Users/Marlon Angulo/Downloads", nombre_weighted), 
          row.names = FALSE)

# Ver matriz de confusión
predicciones_train_weighted <- predict(model_rf_weighted, newdata = train)
confusion_matrix <- confusionMatrix(predicciones_train_weighted, train$Pobre)
print(confusion_matrix)





# 3.3.5 Modelo con Boosting ==============
p_load(xgboost)

# Calcular peso para clase minoritaria
scale_ratio <- table(train$Pobre)[1] / table(train$Pobre)[2]  # ≈4

model_xgb <- train(
  Pobre ~ .,
  data = train,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 3, classProbs = FALSE),
  metric = "F1_weighted",
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.1,
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  ),
  scale_pos_weight = scale_ratio  # Pesa más la clase pobre
)


predicciones_model_xgb <- predict(model_xgb, newdata = train)
confusion_matrix <- confusionMatrix(predicciones_model_xgb, train$Pobre)
print(confusion_matrix)

# 3.3.6 Modelo Boosting Trashehold 0.3 ===========


train$Pobre <- factor(ifelse(train$Pobre == 1, "Pobre", "NoPobre"),
                      levels = c("NoPobre", "Pobre"))


# Configuración del control
ctrl_xgb <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  verboseIter = FALSE
)

# Modelo XGBoost original
set.seed(2025)
model_xgb <- train(
  Pobre ~ .,
  data = train,
  method = "xgbTree",
  trControl = ctrl_xgb,
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.1,
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  ),
  verbose = FALSE
)

# Obtener probabilidades del modelo original
probabilidades <- predict(model_xgb, train, type = "prob")

# Aplicar custom threshold
threshold_bajo <- 0.33
predicciones_custom <- ifelse(probabilidades$Pobre > threshold_bajo, "Pobre", "NoPobre")
predicciones_custom <- factor(predicciones_custom, levels = c("NoPobre", "Pobre"))

# Matriz de confusión
confusion_custom <- confusionMatrix(predicciones_custom, train$Pobre)
print(confusion_custom)







# CALCULAR F1 PARA EL MODELO ANTERIOR (que da 0.65 en test)
TP_old <- confusion_custom$table[2, 2]  # Pobre predicho como Pobre
FP_old <- confusion_custom$table[2, 1]  # NoPobre predicho como Pobre  
FN_old <- confusion_custom$table[1, 2]  # Pobre predicho como NoPobre

precision_old <- TP_old / (TP_old + FP_old)
recall_old <- TP_old / (TP_old + FN_old)
f1_old <- 2 * (precision_old * recall_old) / (precision_old + recall_old)

cat("🔍 MODELO ANTERIOR (que da 0.65 en Kaggle):\n")
cat("- True Positives:", TP_old, "\n")
cat("- False Positives:", FP_old, "\n") 
cat("- False Negatives:", FN_old, "\n")
cat("- Precision Pobre:", round(precision_old, 4), "\n")
cat("- Recall Pobre:", round(recall_old, 4), "\n")
cat("- F1 Train:", round(f1_old, 4), "\n")
cat("- F1 Kaggle: 0.65\n")
cat("- Overfitting:", round(f1_old - 0.65, 4), "\n")










# Obtener probabilidades del modelo original
probabilidades <- predict(model_xgb, test, type = "prob")

# Aplicar custom threshold
threshold_bajo <- 0.33
predicciones_custom <- ifelse(probabilidades$Pobre > threshold_bajo, "Pobre", "NoPobre")


# Submission
submission_xgb <- data.frame(
  id = test$id,
  poverty = as.numeric(predicciones_custom == "Pobre")  # 1 si Pobre, 0 si NoPobre
)

ruta_descargas <- "C:/Users/Marlon Angulo/Downloads"

# Puedes incluir parámetros del modelo en el nombre si quieres, por ejemplo el threshold usado
nombre_archivo <- paste0("XGB_threshold_", threshold_bajo, "_nrounds_100.csv")
ruta_completa <- file.path(ruta_descargas, nombre_archivo)

# Guardar el submission
write.csv(submission_xgb, ruta_completa, row.names = FALSE)















































































































# 3.3.7 Modelo solo con top 8 variables ===========
vars_top <- c("Pobre", "prop_cotizantes", "prop_cuartos", "prop_ocupados", 
              "num_cotizantes", "tiene_vivienda", "cat_maxEduc", 
              "prop_cuartos_dormir", "Nivel_educ")

train_top <- train[, vars_top]

set.seed(2025)
model_xgb_top <- train(
  Pobre ~ .,
  data = train_top,
  method = "xgbTree",
  trControl = ctrl_xgb,
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.1,
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  ),
  verbose = FALSE
)

# Probar en test
probabilidades_top <- predict(model_xgb_top, test[, vars_top[-1]], type = "prob")
predicciones_top <- ifelse(probabilidades_top$Pobre > 0.3, 1, 0)


# Matriz de confusión en entrenamiento
probabilidades_train_top <- predict(model_xgb_top, train_top, type = "prob")
predicciones_train_top <- ifelse(probabilidades_train_top$Pobre > 0.3, "Pobre", "NoPobre")
predicciones_train_top <- factor(predicciones_train_top, levels = c("NoPobre", "Pobre"))

confusion_train_top <- confusionMatrix(predicciones_train_top, train_top$Pobre)
print(confusion_train_top)

# Exportar resultados del test
submission_top <- data.frame(
  id = test$id,
  poverty = predicciones_top
)

write.csv(submission_top, "C:/Users/Marlon Angulo/Downloads/XGB_top_variables_threshold_0.3.csv", row.names = FALSE)







# 3.3.8 Modelo con variables avanzadas ==========


# Crear nuevas variables basadas en las relaciones más importantes
train_enhanced <- train %>%
  mutate(
    # Ratios sofisticados (como mencionan en "Feature Refinement")
    ratio_efectividad_ocupacional = num_occupied / (num_inactivos + 1),
    ratio_proteccion_social = num_cotizantes / (Nper + 1),
    indice_capital_humano = (cat_maxEduc * prop_ocupados) / (num_minors + 1),
    
    # Interacciones entre variables top del varImp
    interaccion_vivienda_educ = tiene_vivienda * Nivel_educ,
    interaccion_cotizantes_ocupados = prop_cotizantes * prop_ocupados,
    
    # Segmentación estratégica
    segmento_estrategico = case_when(
      prop_cotizantes < 0.2 & num_minors > 1 ~ "familias_vulnerables",
      prop_ocupados < 0.3 & vulnerability_index > 0.6 ~ "hogares_criticos",
      prop_cotizantes > 0.7 & prop_ocupados > 0.7 ~ "hogares_estables",
      TRUE ~ "hogares_medianos"
    ),
    
    # Variables de desigualdad interna del hogar
    dispersion_educativa = cat_maxEduc - Nivel_educ, # Jefe vs máximo
    brecha_genero_ocupacion = (num_women / Nper) - prop_ocupados
  )

# Convertir segmento a factor
train_enhanced$segmento_estrategico <- factor(train_enhanced$segmento_estrategico)

# Verificar nuevas variables
summary(train_enhanced[, c("ratio_efectividad_ocupacional", "ratio_proteccion_social", "indice_capital_humano")])


# Crear quintiles basados en el índice de vulnerabilidad (como hicieron con income)
train_enhanced <- train_enhanced %>%
  mutate(vulnerability_quintile = ntile(vulnerability_index, 5))

# Ver distribución de Pobre por quintil
table(train_enhanced$vulnerability_quintile, train_enhanced$Pobre)

# Estratificar la muestra - tomar misma cantidad por quintil
set.seed(2025)
train_stratified <- train_enhanced %>%
  group_by(vulnerability_quintile) %>%
  sample_n(min(30000, n())) %>%  # Ajustar según tamaño disponible
  ungroup()

# Verificar nuevo balance
table(train_stratified$vulnerability_quintile, train_stratified$Pobre)




# Configuración mejorada con validación
ctrl_xgb_enhanced <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = FALSE,
  allowParallel = TRUE,
  sampling = "up"  # Upsampling para balancear dentro de folds
)

# Grid de parámetros optimizado
tune_grid_enhanced <- expand.grid(
  nrounds = 150,
  max_depth = 6,
  eta = 0.05,        # Learning rate más bajo
  gamma = 1,         # Regularización
  colsample_bytree = 0.7,
  min_child_weight = 2,
  subsample = 0.8
)

# Entrenar modelo mejorado
set.seed(2025)

# Corregir los niveles del factor Pobre
train_stratified$Pobre <- factor(train_stratified$Pobre, 
                                 levels = c(0, 1),
                                 labels = c("NoPobre", "Pobre"))

# Verificar
table(train_stratified$Pobre)
levels(train_stratified$Pobre)

# Ahora ejecutar el modelo
set.seed(2025)
model_xgb_enhanced <- train(
  Pobre ~ .,
  data = train_stratified %>% select(-vulnerability_quintile),
  method = "xgbTree",
  trControl = ctrl_xgb_enhanced,
  tuneGrid = tune_grid_enhanced,
  metric = "ROC",
  verbose = FALSE
)


# Obtener probabilidades del modelo mejorado
probabilidades_enhanced <- predict(model_xgb_enhanced, train_stratified, type = "prob")

# Función para evaluar por quintiles
evaluar_por_quintiles <- function(probs, true_labels, vulnerability_quintiles, threshold = 0.3) {
  predictions <- ifelse(probs$Pobre > threshold, "Pobre", "NoPobre")
  
  results <- data.frame(
    quintil = vulnerability_quintiles,
    verdadero = true_labels,
    prediccion = predictions
  )
  
  confusion_por_quintil <- results %>%
    group_by(quintil) %>%
    summarise(
      accuracy = mean(prediccion == verdadero),
      recall_pobre = sum(prediccion == "Pobre" & verdadero == "Pobre") / sum(verdadero == "Pobre"),
      precision_pobre = sum(prediccion == "Pobre" & verdadero == "Pobre") / sum(prediccion == "Pobre"),
      n_pobres_reales = sum(verdadero == "Pobre"),
      n_pobres_predichos = sum(prediccion == "Pobre")
    )
  
  return(confusion_por_quintil)
}

# Evaluar con threshold 0.3
resultados_quintiles <- evaluar_por_quintiles(
  probabilidades_enhanced, 
  train_stratified$Pobre, 
  train_stratified$vulnerability_quintile
)

print(resultados_quintiles)








# Crear ensemble
crear_ensemble <- function(probs, thresholds = c(0.2, 0.3, 0.4)) {
  ensemble_score <- rowMeans(sapply(thresholds, function(th) {
    ifelse(probs$Pobre > th, 1, 0)
  }))
  return(ensemble_score)
}

ensemble_scores <- crear_ensemble(probabilidades_enhanced)

# Encontrar mejor threshold para ensemble
encontrar_mejor_threshold_ensemble <- function(ensemble_scores, true_labels) {
  thresholds <- seq(0.1, 0.6, by = 0.05)
  resultados <- data.frame()
  
  for(th in thresholds) {
    pred_temp <- ifelse(ensemble_scores > th, "Pobre", "NoPobre")
    cm_temp <- confusionMatrix(factor(pred_temp, levels = c("NoPobre", "Pobre")), true_labels)
    
    resultados <- rbind(resultados, data.frame(
      threshold = th,
      recall_pobre = cm_temp$byClass["Sensitivity"],
      precision_pobre = cm_temp$byClass["Pos Pred Value"],
      f1_pobre = cm_temp$byClass["F1"]
    ))
  }
  
  return(resultados)
}

resultados_ensemble <- encontrar_mejor_threshold_ensemble(ensemble_scores, train_stratified$Pobre)
print(resultados_ensemble)


























# Preparar test set con las mismas features
test_enhanced <- test %>%
  mutate(
    ratio_efectividad_ocupacional = num_occupied / (num_inactivos + 1),
    ratio_proteccion_social = num_cotizantes / (Nper + 1),
    indice_capital_humano = (cat_maxEduc * prop_ocupados) / (num_minors + 1),
    interaccion_vivienda_educ = tiene_vivienda * Nivel_educ,
    interaccion_cotizantes_ocupados = prop_cotizantes * prop_ocupados,
    segmento_estrategico = case_when(
      prop_cotizantes < 0.2 & num_minors > 1 ~ "familias_vulnerables",
      prop_ocupados < 0.3 & vulnerability_index > 0.6 ~ "hogares_criticos",
      prop_cotizantes > 0.7 & prop_ocupados > 0.7 ~ "hogares_estables",
      TRUE ~ "hogares_medianos"
    ),
    dispersion_educativa = cat_maxEduc - Nivel_educ,
    brecha_genero_ocupacion = (num_women / Nper) - prop_ocupados
  )

test_enhanced$segmento_estrategico <- factor(test_enhanced$segmento_estrategico)

# Obtener probabilidades del test
probabilidades_test <- predict(model_xgb_enhanced, test_enhanced, type = "prob")

# Aplicar ensemble con mejor threshold (0.35)
ensemble_test <- crear_ensemble(probabilidades_test, thresholds = c(0.2, 0.3, 0.4))
predicciones_finales <- ifelse(ensemble_test > 0.35, 1, 0)

# Submission final
submission_final <- data.frame(
  id = test$id,
  poverty = predicciones_finales
)

write.csv(submission_final, "C:/Users/Marlon Angulo/Downloads/XGB_enhanced_ensemble_0.77F1.csv", row.names = FALSE)



# 3.3.9 Modelo Final XGB Optimizado 0.70 (Mejor modelo) ========
# ENRIQUECIMIENTO MASIVO DE VARIABLES DESDE DATOS ORIGINALES

cat("🚀 AGREGANDO 50+ VARIABLES NUEVAS DESDE DATOS ORIGINALES\n")

# 1. AGREGAR MÁS VARIABLES DE PERSONAS A NIVEL HOGAR
train_personas_enriched <- train_personas |> 
  group_by(id) |>
  summarize(
    # ===== VARIABLES BÁSICAS MEJORADAS 
    edad_promedio = mean(Edad, na.rm = TRUE),
    edad_maxima = max(Edad, na.rm = TRUE),
    edad_minima = min(Edad, na.rm = TRUE),
    rango_edad = max(Edad, na.rm = TRUE) - min(Edad, na.rm = TRUE),
    
    # ===== VARIABLES DE EMPLEO DETALLADAS 
    total_horas_trabajo = sum(Hras_sem_trab, na.rm = TRUE),
    horas_promedio_trabajo = mean(Hras_sem_trab, na.rm = TRUE),
    num_trabajadores_tiempo_completo = sum(Hras_sem_trab >= 40, na.rm = TRUE),
    num_trabajadores_medio_tiempo = sum(Hras_sem_trab >= 20 & Hras_sem_trab < 40, na.rm = TRUE),
    
    # ===== VARIABLES DE TIPO DE EMPLEO 
    num_empleados_formales = sum(Pos_tra_pri == 3 & Cot_pension == 1, na.rm = TRUE),
    num_empleados_informales = sum(Pos_tra_pri == 3 & Cot_pension == 0, na.rm = TRUE),
    num_independientes = sum(Pos_tra_pri == 4, na.rm = TRUE),
    num_patrones = sum(Pos_tra_pri == 1, na.rm = TRUE),
    num_trabajadores_domesticos = sum(Pos_tra_pri == 5, na.rm = TRUE),
    
    # ===== VARIABLES DE EDUCACIÓN DETALLADAS 
    promedio_educacion = mean(Nivel_educ, na.rm = TRUE),
    max_educacion = max(Nivel_educ, na.rm = TRUE),
    num_sin_educacion = sum(Nivel_educ == 0, na.rm = TRUE),
    num_educacion_basica = sum(Nivel_educ %in% c(1, 2, 3), na.rm = TRUE),
    num_educacion_media = sum(Nivel_educ %in% c(4, 5), na.rm = TRUE),
    num_educacion_superior = sum(Nivel_educ %in% c(6, 7, 8, 9), na.rm = TRUE),
    
    # ===== VARIABLES DE SEGURIDAD SOCIAL 
    num_salud_subsidiado = sum(Régimen_SS_salud == 2, na.rm = TRUE),
    num_salud_contributivo = sum(Régimen_SS_salud == 1, na.rm = TRUE),
    num_salud_especial = sum(Régimen_SS_salud == 3, na.rm = TRUE),
    num_sin_salud = sum(is.na(Régimen_SS_salud) | Régimen_SS_salud == 0, na.rm = TRUE),
    
    # ===== VARIABLES DE BÚSQUEDA DE EMPLEO 
    num_buscando_trabajo = sum(Des == 1, na.rm = TRUE),
    num_disponibles_trabajar = sum(Disp_mas_horas == 1, na.rm = TRUE),
    num_quieren_mas_horas = sum(Quiere_mas_horas == 1, na.rm = TRUE),
    
    # ===== VARIABLES DE SUBSIDIOS 
    num_recibe_subsidio_transporte = sum(Sub_Trans == 1, na.rm = TRUE),
    num_recibe_subsidio_familiar = sum(Sub_Fam == 1, na.rm = TRUE),
    num_recibe_subsidio_educativo = sum(Sub_Edu == 1, na.rm = TRUE),
    
    # ===== VARIABLES DE INGRESOS (indicadores binarios) 
    num_ingreso_horas_extra = sum(Ing_HE > 0, na.rm = TRUE),
    num_ingreso_bonificaciones = sum(Ing_Bon > 0, na.rm = TRUE),
    num_ingreso_primas = sum(Ing_Pr > 0, na.rm = TRUE),
    
    # ===== VARIABLES DE ESTABILIDAD LABORAL 
    promedio_tiempo_empresa = mean(T_Tra_Emp, na.rm = TRUE),
    max_tiempo_empresa = max(T_Tra_Emp, na.rm = TRUE),
    num_empleados_estables = sum(T_Tra_Emp > 12, na.rm = TRUE), # +1 año en empresa
    
    # ===== VARIABLES DE TAMAÑO EMPRESA ===
    num_empresas_grandes = sum(Tam_empresa %in% c(4, 5), na.rm = TRUE),
    num_empresas_pequenas = sum(Tam_empresa %in% c(1, 2), na.rm = TRUE),
    
    # ===== VARIABLES DE ACTIVIDAD ECONÓMICA ===
    num_agricultura = sum(Act_principal_SP == 1, na.rm = TRUE),
    num_industria = sum(Act_principal_SP == 2, na.rm = TRUE),
    num_servicios = sum(Act_principal_SP %in% c(3, 4, 5, 6), na.rm = TRUE),
    
    # ===== VARIABLES DE JEFATURA Y GÉNERO ===
    edad_jefe_hogar = first(Edad[Jefe_hogar == 1]),
    educacion_jefe = first(Nivel_educ[Jefe_hogar == 1]),
    sexo_jefe = first(Sexo[Jefe_hogar == 1]),
    ocupacion_jefe = first(Oc[Jefe_hogar == 1])
  ) |>
  ungroup()

# 2. HACER LO MISMO PARA TEST
test_personas_enriched <- test_personas |> 
  group_by(id) |>
  summarize(
    edad_promedio = mean(Edad, na.rm = TRUE),
    edad_maxima = max(Edad, na.rm = TRUE),
    edad_minima = min(Edad, na.rm = TRUE),
    rango_edad = max(Edad, na.rm = TRUE) - min(Edad, na.rm = TRUE),
    total_horas_trabajo = sum(Hras_sem_trab, na.rm = TRUE),
    horas_promedio_trabajo = mean(Hras_sem_trab, na.rm = TRUE),
    num_trabajadores_tiempo_completo = sum(Hras_sem_trab >= 40, na.rm = TRUE),
    num_trabajadores_medio_tiempo = sum(Hras_sem_trab >= 20 & Hras_sem_trab < 40, na.rm = TRUE),
    num_empleados_formales = sum(Pos_tra_pri == 3 & Cot_pension == 1, na.rm = TRUE),
    num_empleados_informales = sum(Pos_tra_pri == 3 & Cot_pension == 0, na.rm = TRUE),
    num_independientes = sum(Pos_tra_pri == 4, na.rm = TRUE),
    num_patrones = sum(Pos_tra_pri == 1, na.rm = TRUE),
    num_trabajadores_domesticos = sum(Pos_tra_pri == 5, na.rm = TRUE),
    promedio_educacion = mean(Nivel_educ, na.rm = TRUE),
    max_educacion = max(Nivel_educ, na.rm = TRUE),
    num_sin_educacion = sum(Nivel_educ == 0, na.rm = TRUE),
    num_educacion_basica = sum(Nivel_educ %in% c(1, 2, 3), na.rm = TRUE),
    num_educacion_media = sum(Nivel_educ %in% c(4, 5), na.rm = TRUE),
    num_educacion_superior = sum(Nivel_educ %in% c(6, 7, 8, 9), na.rm = TRUE),
    num_salud_subsidiado = sum(Régimen_SS_salud == 2, na.rm = TRUE),
    num_salud_contributivo = sum(Régimen_SS_salud == 1, na.rm = TRUE),
    num_salud_especial = sum(Régimen_SS_salud == 3, na.rm = TRUE),
    num_sin_salud = sum(is.na(Régimen_SS_salud) | Régimen_SS_salud == 0, na.rm = TRUE),
    num_buscando_trabajo = sum(Des == 1, na.rm = TRUE),
    num_disponibles_trabajar = sum(Disp_mas_horas == 1, na.rm = TRUE),
    num_quieren_mas_horas = sum(Quiere_mas_horas == 1, na.rm = TRUE),
    num_recibe_subsidio_transporte = sum(Sub_Trans == 1, na.rm = TRUE),
    num_recibe_subsidio_familiar = sum(Sub_Fam == 1, na.rm = TRUE),
    num_recibe_subsidio_educativo = sum(Sub_Edu == 1, na.rm = TRUE),
    num_ingreso_horas_extra = sum(Ing_HE > 0, na.rm = TRUE),
    num_ingreso_bonificaciones = sum(Ing_Bon > 0, na.rm = TRUE),
    num_ingreso_primas = sum(Ing_Pr > 0, na.rm = TRUE),
    promedio_tiempo_empresa = mean(T_Tra_Emp, na.rm = TRUE),
    max_tiempo_empresa = max(T_Tra_Emp, na.rm = TRUE),
    num_empleados_estables = sum(T_Tra_Emp > 12, na.rm = TRUE),
    num_empresas_grandes = sum(Tam_empresa %in% c(4, 5), na.rm = TRUE),
    num_empresas_pequenas = sum(Tam_empresa %in% c(1, 2), na.rm = TRUE),
    num_agricultura = sum(Act_principal_SP == 1, na.rm = TRUE),
    num_industria = sum(Act_principal_SP == 2, na.rm = TRUE),
    num_servicios = sum(Act_principal_SP %in% c(3, 4, 5, 6), na.rm = TRUE),
    edad_jefe_hogar = first(Edad[Jefe_hogar == 1]),
    educacion_jefe = first(Nivel_educ[Jefe_hogar == 1]),
    sexo_jefe = first(Sexo[Jefe_hogar == 1]),
    ocupacion_jefe = first(Oc[Jefe_hogar == 1])
  ) |>
  ungroup()



train_with_id <- train_hogares |> 
  left_join(TR_personas_nivel_hogar) |>
  mutate(
    # PROPORCIONES EXISTENTES:
    prop_inactivos = num_inactivos / Nper,
    prop_cotizantes = num_cotizantes / Nper,
    prop_ocupados = num_occupied / Nper,
    
    # NUEVA VARIABLE AVANZADA:
    vulnerability_index = (
      (1 - prop_ocupados) +           # Desempleo
        (num_minors / Nper) +           # Carga de menores  
        (1 - prop_cotizantes) +         # Exclusión financiera
        (1 / (prop_cuartos + 0.1))      # Hacinamiento inverso
    ) / 4,                             # Normalizar 0-1
    
    Pobre = factor(ifelse(Pobre == 1, "Pobre", "NoPobre"), 
                   levels = c("NoPobre", "Pobre"))
  )




# 3. COMBINAR CON LOS DATOS EXISTENTES
train_enriched <- train_with_id |> 
  left_join(train_personas_enriched, by = "id")

test_enriched <- test |> 
  left_join(test_personas_enriched, by = "id")

# Reemplazar NAs con 0 en estas variables
train_enriched_clean <- train_enriched %>%
  mutate(
    horas_promedio_trabajo = ifelse(is.na(horas_promedio_trabajo), 0, horas_promedio_trabajo),
    promedio_tiempo_empresa = ifelse(is.na(promedio_tiempo_empresa), 0, promedio_tiempo_empresa)
  )

# Aplicar misma transformación a test
test_enriched_clean <- test_enriched %>%
  mutate(
    horas_promedio_trabajo = ifelse(is.na(horas_promedio_trabajo), 0, horas_promedio_trabajo),
    promedio_tiempo_empresa = ifelse(is.na(promedio_tiempo_empresa), 0, promedio_tiempo_empresa)
  )

train_enriched_final <- train_enriched_clean %>%
  mutate(max_tiempo_empresa = ifelse(is.infinite(max_tiempo_empresa), 0, max_tiempo_empresa))
test_enriched_final <- test_enriched_clean %>%
  mutate(max_tiempo_empresa = ifelse(is.infinite(max_tiempo_empresa), 0, max_tiempo_empresa))


# Verificar que no hay NAs
sum(is.na(train_enriched_clean))




# VERSIÓN MEJORADA CON ANTI-OVERFITTING
set.seed(2025)
model_enriched_improved <- train(
  Pobre ~ .,
  data = train_enriched_final %>% select(-id),
  method = "xgbTree",
  trControl = trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    verboseIter = TRUE
  ),
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.1,
    gamma = 1,           # ✅ REGULARIZACIÓN AÑADIDA
    colsample_bytree = 0.7, # ✅ MÁS CONSERVADOR
    min_child_weight = 3,   # ✅ MÁS RESTRICTIVO  
    subsample = 0.8
  ),
  verbose = FALSE
)

table(train_enriched$Pobre)


# VER RESULTADOS DEL MODELO ORIGINAL CON THRESHOLD 0.33
prob_original <- predict(model_enriched_improved, train_enriched_final, type = "prob")$Pobre
pred_original <- ifelse(prob_original > 0.33, "Pobre", "NoPobre")
cm_original <- confusionMatrix(factor(pred_original, levels = c("NoPobre", "Pobre")), 
                               train_enriched_final$Pobre)

# CALCULAR F1 PARA LA CLASE "POBRE" CORRECTAMENTE
# Para la clase "Pobre":
TP <- cm_original$table[2, 2]  # Verdaderos positivos: Pobre predicho como Pobre
FP <- cm_original$table[2, 1]  # Falsos positivos: NoPobre predicho como Pobre  
FN <- cm_original$table[1, 2]  # Falsos negativos: Pobre predicho como NoPobre

precision_pobre_correct <- TP / (TP + FP)
recall_pobre_correct <- TP / (TP + FN)
f1_pobre_correct <- 2 * (precision_pobre_correct * recall_pobre_correct) / 
  (precision_pobre_correct + recall_pobre_correct)

cat("🎯 MÉTRICAS CORRECTAS PARA CLASE 'POBRE':\n")
cat("- True Positives (Pobre correcto):", TP, "\n")
cat("- False Positives:", FP, "\n") 
cat("- False Negatives:", FN, "\n")
cat("- Precision Pobre:", round(precision_pobre_correct, 4), "\n")
cat("- Recall Pobre:", round(recall_pobre_correct, 4), "\n")
cat("- F1 Pobre:", round(f1_pobre_correct, 4), "\n")


#En teoría es mejor con 0.34
prob_train <- predict(model_enriched_improved, train_enriched_final, type = "prob")$Pobre
pred_train <- ifelse(prob_train > 0.34, "Pobre", "NoPobre")
confusionMatrix(factor(pred_train, levels = c("NoPobre", "Pobre")), train_enriched_final$Pobre)


# BUCLE PARA ENCONTRAR THRESHOLD ÓPTIMO QUE MAXIMIZA F1-SCORE
prob_train <- predict(model_enriched_improved, train_enriched_final, type = "prob")$Pobre

# Probar diferentes thresholds
thresholds <- seq(0.1, 0.5, by = 0.01)
results <- data.frame(threshold = numeric(), 
                      f1_score = numeric(),
                      recall = numeric(),
                      precision = numeric(),
                      TP = numeric(),
                      FP = numeric(),
                      FN = numeric())

for (th in thresholds) {
  pred_temp <- ifelse(prob_train > th, "Pobre", "NoPobre")
  cm_temp <- confusionMatrix(factor(pred_temp, levels = c("NoPobre", "Pobre")), 
                             train_enriched_final$Pobre)
  
  # Calcular métricas para clase "Pobre"
  TP <- cm_temp$table[2, 2]
  FP <- cm_temp$table[2, 1]
  FN <- cm_temp$table[1, 2]
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Guardar resultados
  results <- rbind(results, data.frame(
    threshold = th,
    f1_score = f1_score,
    recall = recall,
    precision = precision,
    TP = TP,
    FP = FP,
    FN = FN
  ))
}

# Encontrar el threshold óptimo
optimal_threshold <- results[which.max(results$f1_score), ]
print(optimal_threshold)


# 1. Modelo F1 0.7083 (threshold 0.34)
prob_test_1 <- predict(model_enriched_improved, test_enriched_final, type = "prob")$Pobre
pred_test_1 <- ifelse(prob_test_1 > 0.34, "Pobre", "NoPobre")

submission_1 <- data.frame(
  id = test_enriched_final$id,
  poverty = as.numeric(pred_test_1 == "Pobre")
)

write.csv(submission_1, paste0(output_path, "XGB_threshold_034_depth_6_eta_01_gamma_1.csv"), row.names = FALSE)

# 3.3.10 Modelo XGB optmizado con advanced featuring 0.71===========
train_enriched_advanced <- train_enriched_final %>%
  mutate(
    # INTERACCIONES CLAVE
    interaccion_educ_ocupacion = promedio_educacion * prop_ocupados,
    densidad_por_educacion = Nper / (promedio_educacion + 0.1),
    ratio_formalidad = num_empleados_formales / (num_empleados_informales + 1),
    
    # NUEVAS VARIABLES COMPUESTAS
    score_empleabilidad = (prop_ocupados * 0.4) + (prop_cotizantes * 0.3) + 
      (num_educacion_superior / Nper * 0.3),
    
    indice_vulnerabilidad_mejorado = (
      (1 - prop_ocupados) * 0.3 +           # Desempleo
        (num_minors / Nper) * 0.2 +           # Dependencia
        (1 - prop_cotizantes) * 0.25 +        # Informalidad
        (1 / (prop_cuartos + 0.1)) * 0.15 +   # Hacinamiento
        (1 - (num_educacion_superior / Nper)) * 0.1  # Educación baja
    ),
    
    # VARIABLES DE COMPOSICIÓN FAMILIAR MEJORADAS
    prop_menores = num_minors / Nper,
    prop_adultos_mayores = ifelse(edad_maxima > 60, 1, 0),
    diversidad_ocupacional = num_empleados_formales + num_independientes + num_trabajadores_domesticos,
    
    # INDICADORES DE ESTABILIDAD
    estabilidad_laboral = num_empleados_estables / (num_occupied + 1),  # Corregido
    variabilidad_ingresos = (num_ingreso_horas_extra + num_ingreso_bonificaciones) / Nper
  )

# Aplicar a test
test_enriched_advanced <- test_enriched_final %>%
  mutate(
    interaccion_educ_ocupacion = promedio_educacion * prop_ocupados,
    densidad_por_educacion = Nper / (promedio_educacion + 0.1),
    ratio_formalidad = num_empleados_formales / (num_empleados_informales + 1),
    score_empleabilidad = (prop_ocupados * 0.4) + (prop_cotizantes * 0.3) + 
      (num_educacion_superior / Nper * 0.3),
    indice_vulnerabilidad_mejorado = (
      (1 - prop_ocupados) * 0.3 + (num_minors / Nper) * 0.2 + 
        (1 - prop_cotizantes) * 0.25 + (1 / (prop_cuartos + 0.1)) * 0.15 + 
        (1 - (num_educacion_superior / Nper)) * 0.1
    ),
    prop_menores = num_minors / Nper,
    prop_adultos_mayores = ifelse(edad_maxima > 60, 1, 0),
    diversidad_ocupacional = num_empleados_formales + num_independientes + num_trabajadores_domesticos,
    estabilidad_laboral = num_empleados_estables / (num_occupied + 1),
    variabilidad_ingresos = (num_ingreso_horas_extra + num_ingreso_bonificaciones) / Nper
  )

# Continuar con el modelo...


# Modelo con mejor feature engineering
set.seed(2025)
model_advanced <- train(
  Pobre ~ .,
  data = train_enriched_advanced %>% select(-id),
  method = "xgbTree",
  trControl = trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    verboseIter = TRUE
  ),
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.1,
    gamma = 1,
    colsample_bytree = 0.7,
    min_child_weight = 3,
    subsample = 0.8
  ),
  verbose = FALSE
)

# Evaluar
prob_advanced <- predict(model_advanced, train_enriched_advanced, type = "prob")$Pobre

# Encontrar mejor threshold para el nuevo modelo
thresholds <- seq(0.01, 0.5, 0.01)
f1_scores_advanced <- numeric(length(thresholds))

for(i in seq_along(thresholds)) {
  pred <- ifelse(prob_advanced > thresholds[i], "Pobre", "NoPobre")
  pred <- factor(pred, levels = c("NoPobre", "Pobre"))
  cm <- confusionMatrix(pred, train_enriched_advanced$Pobre)
  
  TP <- cm$table[2, 2]
  FP <- cm$table[2, 1]  
  FN <- cm$table[1, 2]
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1_scores_advanced[i] <- 2 * (precision * recall) / (precision + recall)
}

best_threshold_advanced <- thresholds[which.max(f1_scores_advanced)]
best_f1_advanced <- max(f1_scores_advanced)

cat("🎯 RESULTADOS CON MEJOR FEATURE ENGINEERING:\n")
cat("- Mejor threshold:", best_threshold_advanced, "\n")
cat("- Mejor F1:", round(best_f1_advanced, 4), "\n")
cat("- Mejora vs anterior:", round(best_f1_advanced - 0.7083, 4), "\n")


# 2. Modelo F1 0.7104 (threshold 0.36) - Feature engineering avanzado
prob_test_2 <- predict(model_advanced, test_enriched_advanced, type = "prob")$Pobre
pred_test_2 <- ifelse(prob_test_2 > 0.36, "Pobre", "NoPobre")

submission_2 <- data.frame(
  id = test_enriched_advanced$id,
  poverty = as.numeric(pred_test_2 == "Pobre")
)

write.csv(submission_2, paste0(output_path, "XGB_threshold_036_depth_6_eta_01_gamma_1_feateng.csv"), row.names = FALSE)





# 3.3.11 Otros modelos entrenados: Reg Log, Naive Bayes, Elastic Net, RF, XGBoost ====


if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr, tidyverse, googledrive, skimr, naniar, dplyr, 
  caret, 
  ranger, 
  pROC, e1071, themis, recipes, 
  glmnet,      
  xgboost,     
  rpart,      
  klaR,        
  doParallel,   
  
  # <<< FUSIÓN: Paquetes añadidos de v_Max_Performance >>>
  data.table, tidyr, stringr, tibble, purrr,
  Matrix, tictoc
)

# <<< MODIFICACIÓN: Añadida variable de control de v_Max_Performance >>>
TARGET_POS   <- "Si"    # clase positiva (Pobre=1)

googledrive::drive_auth(email = "elianmoreno58@gmail.com") 

folder <- drive_get("ProblemSet2")
files <- drive_ls(folder)
train_hogares <- read_csv(drive_download(files[files$name == "train_hogares.csv",]$id, path = tempfile(fileext = ".csv"), overwrite = TRUE)$local_path, show_col_types = FALSE)
train_personas <- read_csv(drive_download(files[files$name == "train_personas.csv",]$id, path = tempfile(fileext = ".csv"), overwrite = TRUE)$local_path, show_col_types = FALSE)
test_hogares <- read_csv(drive_download(files[files$name == "test_hogares.csv",]$id, path = tempfile(fileext = ".csv"), overwrite = TRUE)$local_path, show_col_types = FALSE)
test_personas <- read_csv(drive_download(files[files$name == "test_personas.csv",]$id, path = tempfile(fileext = ".csv"), overwrite = TRUE)$local_path, show_col_types = FALSE)
cat("¡Datos cargados!\n\n")

# --- <<< FUSIÓN: Funciones Auxiliares de v_Max_Performance >>> ---
cat("Cargando funciones auxiliares (Winsorize, Drop Corr, Opt_Threshold)...\n")
# Winsorización
winsorize_vec <- function(x, p_low=.01, p_high=.99){
  if(!is.numeric(x) || all(is.na(x))) return(x)
  qs <- quantile(x, probs=c(p_low,p_high), na.rm=TRUE, names = FALSE)
  if(anyNA(qs)) return(x)
  pmin(pmax(x, qs[1]), qs[2])
}

# Eliminar variables correlacionadas (Robusta)
drop_high_corr <- function(df_num, thr=0.95){
  cnum <- df_num %>% dplyr::select(where(is.numeric))
  if(ncol(cnum) < 2) return(list(df=df_num, dropped=character(0)))
  variances <- sapply(cnum, function(x) var(x, na.rm = TRUE)); zero_var_cols <- names(variances[variances < 1e-10 | is.na(variances)])
  if (length(zero_var_cols) > 0) { cnum_valid_var <- cnum %>% dplyr::select(-any_of(zero_var_cols)); if(ncol(cnum_valid_var) < 2) return(list(df=dplyr::select(df_num, -any_of(zero_var_cols)), dropped=zero_var_cols))
  } else { cnum_valid_var <- cnum }
  corM <- suppressWarnings(cor(cnum_valid_var, use="pairwise.complete.obs")); corM[is.na(corM)] <- 0
  upper <- corM; upper[lower.tri(upper, diag=TRUE)] <- 0
  todrop_logical <- apply(upper, 2, function(col) any(abs(col) > thr, na.rm = TRUE))
  todrop_corr <- colnames(upper)[todrop_logical]; todrop_corr <- todrop_corr[!is.na(todrop_corr)]
  dropped_final <- unique(c(todrop_corr, zero_var_cols))
  list(df = dplyr::select(df_num, -any_of(dropped_final)), dropped = dropped_final)
}

# Optimizar umbral F1
opt_threshold <- function(y_true, p, grid = seq(0.05,0.95,by=.01), positive = TARGET_POS){
  best <- list(thr = 0.5, f1 = -1); lvls <- levels(y_true); neg_level <- setdiff(lvls, positive)[1]
  if(length(lvls) != 2 || !positive %in% lvls) { warning("opt_threshold: y_true no es factor binario."); return(best) }
  if(anyNA(p)) { warning("opt_threshold: NAs en probabilidades 'p'."); return(best)}
  for(th in grid){
    pred <- factor(ifelse(p >= th, positive, neg_level), levels = lvls)
    cm_res <- try(caret::confusionMatrix(pred, y_true, positive = positive), silent = TRUE)
    if(inherits(cm_res, "try-error")) next
    cm <- cm_res; P <- cm$byClass["Pos Pred Value"]; R <- cm$byClass["Sensitivity"]
    F1 <- ifelse(is.na(P) || is.na(R) || P+R==0, NA, 2*(P*R)/(P+R))
    if(!is.na(F1) && F1 > best$f1) best <- list(thr = th, f1 = F1)
  }
  best
}

# Reporte de métricas
metric_report <- function(y_true, p, thr, positive = TARGET_POS){
  lvls <- levels(y_true); neg_level <- setdiff(lvls, positive)[1]
  if(length(lvls) != 2 || !positive %in% lvls) { warning("metric_report: y_true no es factor binario."); return(tibble(F1=NA, Precision=NA, Recall=NA, Accuracy=NA)) }
  if(anyNA(p)) { warning("metric_report: NAs en probabilidades 'p'."); return(tibble(F1=NA, Precision=NA, Recall=NA, Accuracy=NA))}
  pred <- factor(ifelse(p >= thr, positive, neg_level), levels = lvls)
  cm_res <- try(caret::confusionMatrix(pred, y_true, positive = positive), silent = TRUE)
  if(inherits(cm_res, "try-error")) return(tibble(F1=NA, Precision=NA, Recall=NA, Accuracy=NA))
  cm <- cm_res; P <- cm$byClass["Pos Pred Value"]; R <- cm$byClass["Sensitivity"]
  F1_val <- ifelse(is.na(P) || is.na(R) || P+R == 0, NA, 2 * (P * R) / (P + R))
  tibble( F1 = as.numeric(F1_val), Precision = as.numeric(P), Recall = as.numeric(R), Accuracy = as.numeric(cm$overall["Accuracy"]) )
}
cat("Funciones cargadas.\n\n")
# --- FIN DE FUNCIONES AUXILIARES ---


# PASO 2: INGENIERÍA DE VARIABLES (Corregida v4)
cat("Paso 2: Procesando y limpiando datos (Ingeniería de Variables Mejorada)...\n")
process_personas_agg <- function(df_personas){
  p <- df_personas %>%
    mutate(
      SexoMujer = as.integer(P6020 == 2), Edad = P6040, Menor18 = as.integer(Edad < 18),
      Adulto_18a64 = as.integer(Edad >= 18 & Edad < 65), Mayor65 = as.integer(Edad >= 65),
      Nivel_educ = ifelse(is.na(P6210) | P6210 == 9, 0L, P6210), Oc = dplyr::coalesce(Oc, 0L),
      Ina = dplyr::coalesce(Ina, 0L), Des = dplyr::coalesce(Des, 0L),
      Cot_pension = dplyr::case_when(P6920 %in% c(1,3) ~ 1L, P6920 == 2 ~ 0L, TRUE ~ 0L),
      Afiliado_salud = as.integer(P6090 == 1), Jefe_hogar = as.integer(P6050 == 1)
    )
  agg <- p %>% group_by(id) %>%
    summarise( Npers = n(), N_mujeres = sum(SexoMujer, na.rm=TRUE), N_menores18 = sum(Menor18, na.rm=TRUE),
               N_adultos_18a64 = sum(Adulto_18a64, na.rm=TRUE), N_mayores65 = sum(Mayor65, na.rm=TRUE),
               N_ocupados = sum(Oc, na.rm=TRUE), N_inactivos = sum(Ina, na.rm=TRUE),
               N_desocupados = sum(Des, na.rm=TRUE), N_cotizantes = sum(Cot_pension, na.rm=TRUE),
               N_afiliados_salud = sum(Afiliado_salud, na.rm=TRUE), Educ_max = max(Nivel_educ, na.rm=TRUE),
               Edad_mean = mean(Edad, na.rm=TRUE), Edad_sd = sd(Edad, na.rm=TRUE),
               .groups="drop" )
  jefe <- p %>% filter(Jefe_hogar == 1) %>%
    dplyr::select(id, Jefe_mujer = SexoMujer, Jefe_edad = Edad, Jefe_educ = Nivel_educ, Jefe_ocupado = Oc,
                  Jefe_cotiza = Cot_pension, Jefe_afiliado = Afiliado_salud) %>% 
    dplyr::distinct(id, .keep_all = TRUE)
  agg <- agg %>% left_join(jefe, by="id") %>%
    mutate( Prop_mujeres = N_mujeres / pmax(Npers,1), Prop_menores18 = N_menores18 / pmax(Npers,1),
            Prop_mayores65 = N_mayores65 / pmax(Npers,1),
            Razon_dependencia = ifelse(N_adultos_18a64 > 0, (N_menores18 + N_mayores65)/N_adultos_18a64, N_menores18 + N_mayores65),
            PEA = N_ocupados + N_desocupados, Tasa_ocupacion = ifelse(N_adultos_18a64 > 0, N_ocupados/N_adultos_18a64, 0),
            Tasa_desocupacion = ifelse(PEA > 0, N_desocupados/PEA, 0), Tasa_inactividad = ifelse(N_adultos_18a64 > 0, N_inactivos/N_adultos_18a64, 0),
            Prop_cotizantes = ifelse(N_adultos_18a64 > 0, N_cotizantes/N_adultos_18a64, 0), Prop_afiliados = N_afiliados_salud / pmax(Npers,1) ) %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)))
  return(agg)
}
agg_train <- process_personas_agg(train_personas); agg_test <- process_personas_agg(test_personas)
mk_bin <- function(x) as.integer(!is.na(x) & x > 0 & x != 9)
add_bins_scores <- function(df){
  keep <- c("P5090","P5100","P5110","P5120","P5130","P5140","P5150","P5160","P5170","P5180","Estrato1","P6008","P6010")
  for(k in keep) if(k %in% names(df)) df[[paste0("bin_",k)]] <- mk_bin(df[[k]])
  serv_cols <- grep("^bin_P51", names(df), value = TRUE)
  act_cols <- intersect(paste0("bin_", c("P6008","P6010","Estrato1")), names(df))
  mat_cols <- intersect(paste0("bin_", c("P5090","P5100","P5110")), names(df))
  if(length(serv_cols) > 0) df$score_servicios <- rowSums(df[, serv_cols, drop = FALSE], na.rm=TRUE) else df$score_servicios <- 0
  if(length(act_cols) > 0) df$score_activos    <- rowSums(df[, act_cols, drop = FALSE], na.rm=TRUE) else df$score_activos <- 0
  if(length(mat_cols) > 0) df$score_material   <- rowSums(df[, mat_cols, drop = FALSE], na.rm=TRUE) else df$score_material <- 0
  df
}
trh <- train_hogares %>% mutate( Hacinamiento_dormir = ifelse(P5010 > 0, Nper/P5010, Nper), Prop_dormir_cuartos = ifelse(P5000 > 0, P5010/P5000, 0)) %>% add_bins_scores()
teh <- test_hogares %>% mutate( Hacinamiento_dormir = ifelse(P5010 > 0, Nper/P5010, Nper), Prop_dormir_cuartos = ifelse(P5000 > 0, P5010/P5000, 0)) %>% add_bins_scores()
train <- trh %>% left_join(agg_train, by="id"); 
test <- teh %>% left_join(agg_test %>% dplyr::select(-any_of("Pobre")), by="id")
train <- train %>% mutate(Pobre = factor(ifelse(Pobre==1,"Si","No"), levels=c("No","Si")))
add_interactions <- function(df){
  if(all(c("Educ_max", "Npers") %in% names(df))) df$inter_Educ_Npers <- df$Educ_max * df$Npers
  if(all(c("Jefe_edad", "Razon_dependencia") %in% names(df))) df$inter_JefeEdad_Dep <- df$Jefe_edad * df$Razon_dependencia
  if(all(c("Hacinamiento_dormir", "score_material") %in% names(df))) df$inter_Hacina_Material <- df$Hacinamiento_dormir * df$score_material
  if(all(c("Prop_menores18", "score_servicios") %in% names(df))) df$inter_Menores_Serv <- df$Prop_menores18 * df$score_servicios
  df
}
train <- add_interactions(train); test <- add_interactions(test)
num_cols_all <- train %>% dplyr::select(where(is.numeric)) %>% names(); num_cols_all <- setdiff(num_cols_all, c("id"))
train[num_cols_all] <- lapply(train[num_cols_all], winsorize_vec)
common_test <- intersect(num_cols_all, names(test))
if(length(common_test) > 0) test[common_test] <- lapply(test[common_test], winsorize_vec)

# <<< CORRECCIÓN v7: Corregido "de}" por "df}" >>>
make_ratios <- function(df){
  add <- list( ratio_mujeres = c("N_mujeres","Npers"), ratio_menores = c("N_menores18","Npers"),
               ratio_mayores = c("N_mayores65","Npers"), pea_percapita = c("PEA","Npers") )
  for(nm in names(add)){ a <- add[[nm]][1]; b <- add[[nm]][2]; if(all(c(a,b) %in% names(df))) df[[nm]] <- df[[a]]/pmax(df[[b]],1) }
  df
}
# <<< FIN CORRECCIÓN v7 >>>

train <- make_ratios(train); test <- make_ratios(test)
cat_cols <- intersect("Dominio", names(train)) %>% as.character()
feature_cols <- setdiff(names(train), c("Pobre","id", "Ingtotug", "Ingpcug", "Ingtotugarr", "Indigente", "Npobres", "Nindigentes"))
num_cols <- setdiff(feature_cols, cat_cols); num_cols <- intersect(num_cols, names(train %>% dplyr::select(where(is.numeric))))
drop_res <- drop_high_corr(train[, intersect(num_cols, names(train)), drop=FALSE], thr = 0.95)
kept_nums <- names(drop_res$df); dropped <- drop_res$dropped
feature_cols_final <- c(kept_nums, cat_cols)
X_all <- train[, feature_cols_final, drop=FALSE]
y_all <- train$Pobre
X_test <- test[, intersect(feature_cols_final, names(test)), drop=FALSE]
missing_cols <- setdiff(names(X_all), names(X_test))
if(length(missing_cols) > 0){ for(mcol in missing_cols){ if(is.factor(X_all[[mcol]])) X_test[[mcol]] <- as.character(NA) else X_test[[mcol]] <- NA } }
X_test <- X_test[, names(X_all)]
# --- FIN DE INGENIERÍA DE VARIABLES ---


# <<< MODIFICACIÓN: PASO 3: DIVISIÓN Train/Holdout + RECIPE >>>
cat("Paso 3: Creando división Train/Holdout (85/15) y Recipe...\n")
set.seed(2025)
TEST_SIZE <- 0.15 # 15% para holdout
train_idx <- caret::createDataPartition(y_all, p = 1 - TEST_SIZE, list = FALSE)

# --- Sets de datos para entrenamiento (85%) ---
X_train_fold <- X_all[train_idx, ]
y_train_fold <- y_all[train_idx]
# --- Sets de datos para validación/holdout (15%) ---
X_valid_fold <- X_all[-train_idx, ]
y_valid_fold <- y_all[-train_idx]
y_va_factor <- factor(y_valid_fold, levels = c("No", "Si")) # Factor para evaluación

cat(sprintf("División creada: %d para entrenar, %d para validar umbral.\n", nrow(X_train_fold), nrow(X_valid_fold)))

formula_rec <- as.formula(paste("Pobre ~", paste(names(X_all), collapse = " + ")))

# <<< MODIFICACIÓN: Recipe se PREPARA solo con el fold de ENTRENAMIENTO (85%) >>>
train_recipe <- recipes::recipe(formula_rec, data = cbind(Pobre = y_train_fold, X_train_fold)) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors(), new_level = "DESCONOCIDO") %>%
  step_other(all_nominal_predictors(), threshold = 0.01, other = "OTROS_DOM") %>%
  step_novel(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  themis::step_smote(Pobre, over_ratio = 0.5, neighbors = 5) # SMOTE solo se aplica aquí

# Preparamos la recipe y creamos el set de entrenamiento balanceado
rec_prep <- prep(train_recipe)
train_balanced <- bake(rec_prep, new_data = NULL) # Datos del 85%, balanceados
X_valid_baked <- bake(rec_prep, new_data = X_valid_fold) # Datos del 15%, procesados (sin SMOTE)
X_test_baked <- bake(rec_prep, new_data = as.data.frame(X_test)) # Datos de test, procesados

cat("¡Recipe preparado y datos balanceados/procesados!\n\n")
# --- FIN DE RECIPE HÍBRIDA ---


# PASO 4: ENTRENAMIENTO DE MODELOS (en datos 85% balanceados)
cat("Paso 4: Entrenando 5 modelos (en 85% balanceado)...\n")
set.seed(2025)

# <<< MODIFICACIÓN: Se usa la función F1 robusta de v_Max_Performance >>>
f1_summary <- function(data, lev = c("No","Si"), model = NULL){
  F1 <- 0; Sens <- NA; Prec <- NA; Accuracy <- NA
  obs_levels_ok <- all(lev %in% levels(data$obs)); pred_levels_ok <- all(lev %in% levels(data$pred))
  if (!obs_levels_ok || !pred_levels_ok || length(intersect(levels(data$pred), levels(data$obs))) < 2) {
    return(c(F1=F1, Sens=Sens, Prec=Prec, Accuracy=Accuracy))
  }
  data$obs <- factor(data$obs, levels = lev); data$pred <- factor(data$pred, levels = lev)
  cm_res <- try(caret::confusionMatrix(data$pred, data$obs, positive = "Si"), silent=TRUE)
  if(!inherits(cm_res, "try-error")){
    cm <- cm_res; Accuracy <- cm$overall["Accuracy"]; P <- cm$byClass["Pos Pred Value"]; R <- cm$byClass["Sensitivity"]
    if(!is.na(P) && !is.na(R) && (P + R > 0)) F1 <- 2 * (P * R) / (P + R)
    Sens <- R; Prec <- P
  }
  c(F1=as.numeric(F1), Sens=as.numeric(Sens), Prec=as.numeric(Prec), Accuracy=as.numeric(Accuracy))
}

ctrl_tuning_opt <- trainControl(
  method = "cv", number = 3, summaryFunction = f1_summary, classProbs = TRUE,
  savePredictions = "final", 
  verboseIter = TRUE, allowParallel = TRUE
)

cl <- makePSOCKcluster(detectCores() - 1); registerDoParallel(cl)
cat(paste("Paralelización activada con", detectCores() - 1, "núcleos.\n"))

model_list_opt <- list()
# --- Modelo 1: Random Forest ---
cat("\nEntrenando Random Forest (opt)...\n")
try({
  model_list_opt$RandomForest <- train( Pobre ~ ., data = train_balanced, method = "ranger", trControl = ctrl_tuning_opt, metric = "F1",
                                        tuneGrid = expand.grid(mtry = c(floor(sqrt(ncol(train_balanced)-1)), floor((ncol(train_balanced)-1)/3)), splitrule = "gini", min.node.size = 15),
                                        num.trees = 100, importance = 'none' )
})
# --- Modelo 2: Red Elástica ---
cat("\nEntrenando Red Elástica (opt)...\n")
try({
  model_list_opt$ElasticNet <- train( Pobre ~ ., data = train_balanced, method = "glmnet", trControl = ctrl_tuning_opt, metric = "F1",
                                      tuneGrid = expand.grid(alpha = c(0.5, 0.8, 1.0), lambda = 10^seq(-4, -2, length.out = 5)) )
})
# --- Modelo 3: Boosting (XGBoost)---
cat("\nEntrenando XGBoost (optimizado)...\n")
try({
  model_list_opt$XGBoost <- train( Pobre ~ ., data = train_balanced, method = "xgbTree", trControl = ctrl_tuning_opt, metric = "F1",
                                   tuneGrid = expand.grid(nrounds = 100, max_depth = c(3, 5), eta = 0.1, gamma = 0, colsample_bytree = 0.8, min_child_weight = 1, subsample = 0.8),
                                   verbosity = 0
  )
})
# --- Modelo 4: Naive Bayes ---
cat("\nEntrenando Naive Bayes...\n")
try({
  model_list_opt$NaiveBayes <- train( Pobre ~ ., data = train_balanced, method = "naive_bayes", trControl = ctrl_tuning_opt, metric = "F1",
                                      tuneGrid = expand.grid(laplace = 0, usekernel = TRUE, adjust = 1) )
})
# --- Modelo 5: Regresión Logística ---
cat("\nEntrenando Regresión Logística...\n")
try({
  model_list_opt$Logit <- train( Pobre ~ ., data = train_balanced, method = "glm", family = "binomial", trControl = ctrl_tuning_opt, metric = "F1" )
})
# --- Detener paralelización ---
stopCluster(cl); registerDoSEQ()
cat("\n¡Entrenamiento optimizado completado!\n\n")
successful_models_opt <- model_list_opt[!sapply(model_list_opt, is.null)]
if (length(successful_models_opt) == 0) stop("¡ERROR FATAL: Ningún modelo pudo ser entrenado!")


# <<< MODIFICACIÓN: NUEVO PASO 10 (Evaluación Holdout y Umbral) >>>
cat("Paso 10: Evaluando modelos en Holdout (15%) y optimizando umbral F1...\n")

# Lista para guardar las probabilidades del holdout (para el ensamble)
holdout_probs_list <- list()

results <- purrr::map_dfr(names(successful_models_opt), function(nm){
  mod <- successful_models_opt[[nm]]
  
  p_res <- tryCatch({
    predict(mod, newdata = X_valid_baked, type = "prob")[, TARGET_POS]
  }, error = function(e) { rep(NA_real_, nrow(X_valid_baked)) })
  
  holdout_probs_list[[nm]] <<- p_res # Guardar probabilidades para el ensamble
  
  if(anyNA(p_res) || length(p_res) != length(y_va_factor)){ 
    best <- list(thr=0.5, f1=NA)
    met <- tibble(F1=NA,Precision=NA,Recall=NA,Accuracy=NA)
  } else { 
    best <- opt_threshold(y_va_factor, p_res)
    met <- metric_report(y_va_factor, p_res, best$thr)
  }
  tibble(name = nm, thr = best$thr, F1 = met$F1, Precision = met$Precision, Recall = met$Recall, Accuracy = met$Accuracy)
})

perf <- results %>% arrange(desc(F1))
cat("\n🏁 Resultados holdout (ordenado por F1):\n")
print(perf %>% mutate(across(where(is.numeric), ~round(., 4))))

if(all(is.na(perf$F1))) stop("Todos los modelos fallaron en la evaluación holdout.")

best_idx <- which(!is.na(perf$F1))[1]
best_name <- perf$name[best_idx]
best_thr <- perf$thr[best_idx]
best_model_object_opt <- successful_models_opt[[best_name]] # El mejor modelo, ya entrenado

cat(sprintf("🥇 Mejor modelo: %s | Umbral Óptimo=%.3f | F1 en Holdout=%.4f\n", best_name, best_thr, perf$F1[best_idx]))


# <<< MODIFICACIÓN: NUEVO PASO 11 (Predicción con Mejor Modelo y Umbral) >>>
cat("Paso 11: Generando predicción final (Mejor Modelo Individual)...\n")

# Predecir probabilidades en el set de TEST
p_test_probs_best <- predict(best_model_object_opt, newdata = X_test_baked, type = "prob")[, TARGET_POS]

# Aplicar el UMBRAL ÓPTIMO (no 0.5)
pred_test_best <- as.integer(p_test_probs_best >= best_thr)

resultados_finales_best <- data.frame( id = test$id, Pobre = pred_test_best )
nombre_archivo_best <- paste0("predicciones_MEJOR_", gsub("[^A-Za-z0-9]", "_", best_name), "_thr", round(best_thr,2), ".csv")
write.csv(resultados_finales_best, nombre_archivo_best, row.names = FALSE)

cat(paste("\n✅ ¡LISTO (Mejor Modelo)! Archivo guardado:", nombre_archivo_best, "\n"))
cat(paste("📊 Total predichos como pobres:", sum(resultados_finales_best$Pobre), "\n\n"))


# <<< MODIFICACIÓN: NUEVO PASO 12 (Ensamble Top-K) >>>
cat("Paso 12: Generando predicción (Ensamble Top-3)...\n")
topk <- head(perf %>% filter(!is.na(F1)), min(3, sum(!is.na(perf$F1))))

if(nrow(topk) > 0) {
  weights <- topk$F1 / sum(topk$F1) # Ponderar por F1
  
  # --- 1. Calcular probabilidades del ensamble en HOLDOUT (para umbral) ---
  ens_probs_va <- rep(0, nrow(X_valid_baked))
  for(i in seq_len(nrow(topk))){
    current_name <- topk$name[i]
    pr_va <- holdout_probs_list[[current_name]] # Reusar probabilidades
    if(!anyNA(pr_va)){
      ens_probs_va <- ens_probs_va + as.numeric(weights[i]) * pr_va
    }
  }
  
  # --- 2. Optimizar umbral para el ENSAMBLE ---
  ens_best_thr_info <- opt_threshold(y_va_factor, ens_probs_va)
  ens_thr <- ens_best_thr_info$thr
  ens_f1_holdout <- ens_best_thr_info$f1
  cat(sprintf("  Umbral óptimo para ensamble (holdout): %.3f (F1 estimado: %.4f)\n", ens_thr, ens_f1_holdout))
  
  # --- 3. Calcular probabilidades del ensamble en TEST ---
  ens_probs_test <- rep(0, nrow(X_test_baked))
  cat("  Calculando probabilidades del ensamble en test...\n")
  pb_test <- txtProgressBar(min = 0, max = nrow(topk), style = 3)
  
  for(i in seq_len(nrow(topk))){
    current_name <- topk$name[i]
    setTxtProgressBar(pb_test, i)
    
    # Reusar predicción del mejor modelo; calcular las otras
    pr_test_res <- tryCatch({
      if(current_name == best_name){ 
        p_test_probs_best # Reusar del Paso 11
      } else {
        model_to_predict_ref <- successful_models_opt[[current_name]]
        predict(model_to_predict_ref, newdata = X_test_baked, type = "prob")[, TARGET_POS]
      }
    }, error = function(e) { rep(NA_real_, nrow(X_test_baked)) })
    
    if(!anyNA(pr_test_res)){
      ens_probs_test <- ens_probs_test + as.numeric(weights[i]) * pr_test_res
    }
  }
  close(pb_test)
  
  # --- 4. Aplicar umbral del ensamble y guardar ---
  ens_pred <- as.integer(ens_probs_test >= ens_thr)
  submission_ens <- tibble(id = test$id, Pobre = ens_pred)
  out_ens <- sprintf("predicciones_ENSAMBLE_Top%d_thr%.2f.csv", nrow(topk), ens_thr)
  readr::write_csv(submission_ens, out_ens)
  cat(sprintf("\n✅ ¡LISTO (Ensamble)! Archivo guardado: %s | Pobres=%d/%d\n", out_ens, sum(submission_ens$Pobre), nrow(submission_ens)))
  
} else {
  cat("\n⚠️ No se pudo generar ensamble (no hay modelos válidos en topk).\n")
}

# --- FIN DEL SCRIPT ---
cat("\n--- Script Finalizado ---\n")











# 4. Sección de análisis de datos para modelo final =============

p_load(broom, ggplot2)

# Estadísticas descriptivas por grupo de pobreza
estadisticas <- train %>%
  group_by(Pobre) %>%
  summarise(
    Nper_mean = mean(Nper, na.rm = TRUE),
    prop_ocupados_mean = mean(prop_ocupados, na.rm = TRUE),
    prop_inactivos_mean = mean(prop_inactivos, na.rm = TRUE),
    prop_cotizantes_mean = mean(prop_cotizantes, na.rm = TRUE),
    cat_maxEduc_mean = mean(cat_maxEduc, na.rm = TRUE),
    prop_cuartos_mean = mean(prop_cuartos, na.rm = TRUE)
  )

# Promedios totales
totales <- train %>%
  summarise(
    Nper_total = mean(Nper, na.rm = TRUE),
    prop_ocupados_total = mean(prop_ocupados, na.rm = TRUE),
    prop_inactivos_total = mean(prop_inactivos, na.rm = TRUE),
    prop_cotizantes_total = mean(prop_cotizantes, na.rm = TRUE),
    cat_maxEduc_total = mean(cat_maxEduc, na.rm = TRUE),
    prop_cuartos_total = mean(prop_cuartos, na.rm = TRUE)
  )

# 3. Pruebas de significancia (t-test)
variables_test <- c("Nper", "prop_ocupados", "prop_inactivos", 
                    "prop_cotizantes", "cat_maxEduc", "prop_cuartos")

pruebas_significancia <- list()
for(var in variables_test) {
  formula <- as.formula(paste(var, "~ Pobre"))
  prueba <- t.test(formula, data = train)
  pruebas_significancia[[var]] <- tidy(prueba)
}

# 4. Resultados para exportar
estadisticas
totales
pruebas_significancia

# Variable objetivo:
# Calcular frecuencias y porcentajes
frecuencias <- table(train$Pobre)
porcentajes <- prop.table(frecuencias) * 100

# Crear etiquetas para las barras
# Modificar solo esta línea:
etiquetas <- paste0(format(frecuencias, big.mark = ".", decimal.mark = ","), 
                    "\n(", round(porcentajes, 1), "%)")
# Gráfico con frecuencias y porcentajes
barplot(frecuencias,
        main = "Distribución de la Variable Objetivo: Pobreza",
        xlab = "Condición de Pobreza", 
        ylab = "Número de Hogares",
        col = c("gray70", "gray40"),
        names.arg = c("No Pobre (0)", "Pobre (1) "),
        ylim = c(0, max(frecuencias) * 1.1),
        border = "black")

# Agregar etiquetas encima de las barras
text(x = c(0.7, 1.9), 
     y = frecuencias + max(frecuencias) * 0.05,
     labels = etiquetas,
     cex = 0.8)












# CÓDIGO PARA ESTADÍSTICAS COMPLETAS (ANEXO)
library(tidyverse)

# Lista de las 20 variables principales que ya tenemos
variables_principales <- c("max_tiempo_empresa", "total_horas_trabajo", "edad_minima", 
                           "promedio_tiempo_empresa", "edad_promedio", "rango_edad",
                           "horas_promedio_trabajo", "edad_jefe_hogar", "edad_maxima",
                           "num_salud_especial", "Nper", "num_salud_contributivo",
                           "num_educacion_basica", "num_sin_salud", "promedio_educacion",
                           "Nivel_educ", "educacion_jefe", "num_servicios", "num_women", 
                           "num_cotizantes")

# Obtener todas las variables del dataset
todas_variables <- names(train_enriched_final %>% select(-id, -Pobre) %>% select(where(is.numeric)))

# Filtrar las variables que NO están en las principales
variables_anexo <- setdiff(todas_variables, variables_principales)

cat("=== VARIABLES PARA EL ANEXO (", length(variables_anexo), " variables) ===\n")

# Calcular estadísticas para las variables del anexo
anexo_completo <- map_dfr(variables_anexo, function(var) {
  formula <- as.formula(paste(var, "~ Pobre"))
  test <- t.test(formula, data = train_enriched_final)
  
  data.frame(
    variable = var,
    media_total = round(mean(train_enriched_final[[var]], na.rm = TRUE), 3),
    media_nopobres = round(mean(train_enriched_final[[var]][train_enriched_final$Pobre == "NoPobre"], na.rm = TRUE), 3),
    media_pobres = round(mean(train_enriched_final[[var]][train_enriched_final$Pobre == "Pobre"], na.rm = TRUE), 3),
    diferencia = round(diff(test$estimate), 3),
    p_valor = test$p.value,
    significancia = ifelse(test$p.value < 0.01, "***", 
                           ifelse(test$p.value < 0.05, "**", 
                                  ifelse(test$p.value < 0.1, "*", "")))
  )
})

# Organizar por categorías para el anexo
categorias_anexo <- list(
  "Vivienda" = c("tiene_vivienda", "cuartos_dormir", "prop_cuartos_dormir"),
  "Composición Familiar" = c("bin_headWoman", "bin_occupiedHead"),
  "Educación Detallada" = c("max_educacion", "num_sin_educacion", "num_educacion_media", 
                            "num_educacion_superior"),
  "Empleo Detallado" = c("num_occupied", "num_inactivos", "prop_inactivos", 
                         "num_trabajadores_tiempo_completo", "num_trabajadores_medio_tiempo",
                         "num_empleados_informales", "num_independientes", "num_patrones",
                         "num_trabajadores_domesticos"),
  "Seguridad Social Detallada" = c("num_salud_subsidiado", "num_sin_salud"),
  "Búsqueda de Empleo" = c("num_buscando_trabajo", "num_disponibles_trabajar", 
                           "num_quieren_mas_horas"),
  "Subsidios" = c("num_recibe_subsidio_transporte", "num_recibe_subsidio_familiar",
                  "num_recibe_subsidio_educativo"),
  "Ingresos Adicionales" = c("num_ingreso_horas_extra", "num_ingreso_bonificaciones",
                             "num_ingreso_primas"),
  "Estabilidad Laboral" = c("num_empleados_estables", "num_empresas_grandes", 
                            "num_empresas_pequenas"),
  "Sector Económico" = c("num_agricultura", "num_industria"),
  "Índices" = c("vulnerability_index")
)

# Imprimir resultados organizados por categorías
for(categoria in names(categorias_anexo)) {
  vars_categoria <- categorias_anexo[[categoria]]
  
  tabla_categoria <- anexo_completo %>%
    filter(variable %in% vars_categoria) %>%
    select(variable, media_total, media_nopobres, media_pobres, diferencia, significancia)
  
  cat("\n", paste0("=== ", categoria, " ==="), "\n")
  print(tabla_categoria)
  cat("Número de variables:", nrow(tabla_categoria), "\n")
}

# También mostrar todas las variables del anexo en una sola tabla
cat("\n=== TODAS LAS VARIABLES DEL ANEXO ===\n")
print(anexo_completo %>% arrange(desc(abs(diferencia))))

# Verificar que no nos faltó ninguna variable
variables_procesadas <- c(variables_principales, anexo_completo$variable)
variables_faltantes <- setdiff(todas_variables, variables_procesadas)

cat("\n=== VERIFICACIÓN ===\n")
cat("Variables procesadas:", length(variables_procesadas), "\n")
cat("Variables en dataset:", length(todas_variables), "\n")
cat("Variables faltantes:", length(variables_faltantes), "\n")
if(length(variables_faltantes) > 0) {
  cat("Variables que faltan:", paste(variables_faltantes, collapse = ", "), "\n")
}






















# CÓDIGO CORREGIDO PARA DESCRIPTIVAS
library(tidyverse)

# Calcular estadísticas básicas directamente
descriptivas_simples <- train_enriched_final %>%
  select(-id) %>%
  group_by(Pobre) %>%
  summarise(across(where(is.numeric), 
                   list(media = ~mean(., na.rm = TRUE)), 
                   .names = "{.col}")) %>%
  pivot_longer(cols = -Pobre, names_to = "variable", values_to = "media") %>%
  pivot_wider(names_from = Pobre, values_from = media, names_prefix = "media_")

# Calcular diferencias con prueba t
diferencias <- map_dfr(names(train_enriched_final %>% select(-id, -Pobre) %>% select(where(is.numeric))), 
                       function(var) {
                         formula <- as.formula(paste(var, "~ Pobre"))
                         test <- t.test(formula, data = train_enriched_final)
                         
                         data.frame(
                           variable = var,
                           media_total = mean(train_enriched_final[[var]], na.rm = TRUE),
                           media_nopobres = mean(train_enriched_final[[var]][train_enriched_final$Pobre == "NoPobre"], na.rm = TRUE),
                           media_pobres = mean(train_enriched_final[[var]][train_enriched_final$Pobre == "Pobre"], na.rm = TRUE),
                           diferencia = diff(test$estimate),
                           p_valor = test$p.value,
                           significancia = ifelse(test$p.value < 0.01, "***", 
                                                  ifelse(test$p.value < 0.05, "**", 
                                                         ifelse(test$p.value < 0.1, "*", "")))
                         )
                       })

# Mostrar resultados en consola
cat("=== VARIABLES MÁS RELEVANTES ===\n")
print(diferencias %>% 
        arrange(desc(abs(diferencia))) %>%
        select(variable, media_total, media_nopobres, media_pobres, diferencia, significancia) %>%
        head(20))

# Mostrar por categorías específicas
cat("\n=== VARIABLES DEMOGRÁFICAS ===\n")
demograficas <- c("Nper", "num_women", "num_minors", "edad_promedio", "edad_jefe_hogar")
print(diferencias %>% filter(variable %in% demograficas))

cat("\n=== VARIABLES EDUCACIÓN ===\n")
educacion <- c("cat_maxEduc", "promedio_educacion", "max_educacion", "educacion_jefe")
print(diferencias %>% filter(variable %in% educacion))

cat("\n=== VARIABLES EMPLEO ===\n")
empleo <- c("prop_ocupados", "prop_inactivos", "num_empleados_formales", "num_empleados_informales")
print(diferencias %>% filter(variable %in% empleo))

cat("\n=== VARIABLES SEGURIDAD SOCIAL ===\n")
ss <- c("prop_cotizantes", "num_salud_contributivo", "num_salud_subsidiado")
print(diferencias %>% filter(variable %in% ss))

cat("\n=== VARIABLES VIVIENDA ===\n")
vivienda <- c("prop_cuartos", "n_cuartos", "bin_rent")
print(diferencias %>% filter(variable %in% vivienda))










































