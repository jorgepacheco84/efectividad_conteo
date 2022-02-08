# Cargar librerías
library(readxl)
library(MASS)
library(splines)
library(tidyr)
library(ggplot2)
library(effects)
library(dplyr)
theme_set(theme_minimal())

# Cargar base de datos
ejemplo <- read_excel("~/GitHub/metodo_tamizaje/ejemplo_long.xlsx")
View(ejemplo)

#Definir variables categóricas
ejemplo$edad <- as.factor(ejemplo$edad)
ejemplo$desenlace <- as.factor(ejemplo$desenlace)
ejemplo$semana <- as.factor(ejemplo$semana)
ejemplo$dosis <- as.factor(ejemplo$dosis)

#Dividir base de datos
ejemplo_desenlace <- split(ejemplo, ejemplo$desenlace)

# Modelo de Poisson para caso
RTI_caso_ajustada <- glm(freq ~ offset(log(poblacion)) + semana + dosis + edad, family = "poisson", data = ejemplo_desenlace[["caso"]])
summary(RTI_caso_ajustada)
VE_caso_2 <- (1-exp(as.numeric(RTI_caso_ajustada[["coefficients"]][["dosis2"]])))
VE_caso_3 <- (1-exp(as.numeric(RTI_caso_ajustada[["coefficients"]][["dosis3"]])))
VE_caso_ic <- 1-exp(confint(RTI_caso_ajustada))
                    
# Modelo de Poisson para uci
RTI_uci_ajustada <- glm(freq ~ offset(log(poblacion)) + dosis + edad + semana, family = "poisson", data = ejemplo_desenlace[["uci"]])
summary(RTI_uci_ajustada)
VE_uci_2 <- (1-exp(as.numeric(RTI_uci_ajustada[["coefficients"]][["dosis2"]])))
VE_uci_3 <- (1-exp(as.numeric(RTI_uci_ajustada[["coefficients"]][["dosis3"]])))
VE_uci_ic <- 1-exp(confint(RTI_uci_ajustada))

# Modelo de Poisson para defunciones
RTI_def_ajustada <- glm(freq ~ offset(log(poblacion)) + dosis + edad + semana, family = "poisson", data = ejemplo_desenlace[["def"]])
summary(RTI_def_ajustada)
VE_def_2 <- (1-exp(as.numeric(RTI_def_ajustada[["coefficients"]][["dosis2"]])))
VE_def_3 <- (1-exp(as.numeric(RTI_def_ajustada[["coefficients"]][["dosis3"]])))
VE_def_ic <- 1-exp(confint(RTI_def_ajustada))

# Modelo de binomial negativo para caso
RTI_caso_ajustada_nb <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad + semana, data = ejemplo_desenlace[["caso"]])
summary(RTI_caso_ajustada_nb)
VE_caso_nb_2 <- (1-exp(as.numeric(RTI_caso_ajustada_nb[["coefficients"]][["dosis2"]])))
VE_caso_nb_3 <- (1-exp(as.numeric(RTI_caso_ajustada_nb[["coefficients"]][["dosis3"]])))
VE_caso_ic_nb <- 1-exp(confint(RTI_caso_ajustada_nb))

# Modelo binomial negativo para uci
RTI_uci_ajustada_nb <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad + semana, data = ejemplo_desenlace[["uci"]])
summary(RTI_uci_ajustada_nb)
VE_uci_nb_2 <- (1-exp(as.numeric(RTI_uci_ajustada_nb[["coefficients"]][["dosis2"]])))
VE_uci_nb_3 <- (1-exp(as.numeric(RTI_uci_ajustada_nb[["coefficients"]][["dosis3"]])))
VE_uci_ic_nb <- 1-exp(confint(RTI_uci_ajustada_nb))

# Modelo binomial negativo para defunciones
RTI_def_ajustada_nb <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad + semana, data = ejemplo_desenlace[["def"]])
summary(RTI_def_ajustada_nb)
VE_def_nb_2 <- (1-exp(as.numeric(RTI_def_ajustada_nb[["coefficients"]][["dosis2"]])))
VE_def_nb_3 <- (1-exp(as.numeric(RTI_def_ajustada_nb[["coefficients"]][["dosis3"]])))
VE_def_ic_nb <- 1-exp(confint(RTI_def_ajustada_nb))

# Comparación de ajuste entre modelos para caso
logLik(RTI_caso_ajustada)
logLik(RTI_caso_ajustada_nb)
pchisq(2 * (logLik(RTI_caso_ajustada_nb) - logLik(RTI_caso_ajustada)), df = 1, lower.tail = FALSE)

# Comparación de ajuste entre modelos para uci
logLik(RTI_uci_ajustada)
logLik(RTI_uci_ajustada_nb)
pchisq(2 * (logLik(RTI_uci_ajustada_nb) - logLik(RTI_uci_ajustada)), df = 1, lower.tail = FALSE)

# Comparación de ajuste entre modelos para defunciones
logLik(RTI_def_ajustada)
logLik(RTI_def_ajustada_nb)
pchisq(2 * (logLik(RTI_def_ajustada_nb) - logLik(RTI_def_ajustada)), df = 1, lower.tail = FALSE)

# Tabla de efectividad
efectividad <- data.frame(Esquema = rep(c("Dos dosis", "Tres dosis"), times = 3),
                          Desenlace = as.factor(c("Caso", "Caso", "UCI", "UCI", "Fallecer", "Fallecer")),
                          Efectividad = c(VE_caso_nb_2, VE_caso_nb_3, VE_uci_nb_2, VE_uci_nb_3, VE_def_nb_2, VE_def_nb_3),
                          lb = c(VE_caso_ic_nb[2,2], VE_caso_ic_nb[3,2], VE_uci_ic_nb[2,2], VE_uci_ic_nb[3,2], VE_def_ic_nb[2,2], VE_def_ic_nb[3,2]),
                          ub = c(VE_caso_ic_nb[2,1], VE_caso_ic_nb[3,1], VE_uci_ic_nb[2,1], VE_uci_ic_nb[3,1], VE_def_ic_nb[2,1], VE_def_ic_nb[3,1]))
efectividad$Desenlace <- factor(efectividad$Desenlace, levels = c("Caso", "UCI", "Fallecer")) 

g.efectividad <- ggplot(efectividad, aes(x = Esquema, y=Efectividad)) + 
  geom_errorbar(aes(ymin=lb, ymax=ub, color=Esquema), width=.1) +
  ggtitle ("Efectividad de la vacunación para esquema de dos y tres dosis ") +
  geom_point(aes(fill=Esquema),size=2, shape=23) +
  theme(text=element_text(size=20)) +
  theme(axis.text=element_text(size=20)) +
  theme(axis.title=element_text(size=20)) +
  theme(plot.title=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limit = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, by = 0.2), labels = scales::percent) +
  facet_wrap(~Desenlace)                                    

ggsave(plot = g.efectividad,
       filename = "/Users/Usuario/Desktop/efectividad ajustada.png",
       device = "png",
       dpi = "retina",
       width = 14, height = 12)

# Modelo binomial negativo para caso con interacción entre dosis/edad y dosis/semana
RTI_caso_ajustada_nb_int <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad + dosis*edad + semana, data = ejemplo_desenlace[["caso"]])
summary(RTI_caso_ajustada_nb_int)
VE_caso_nb_2_int <- (1-exp(as.numeric(RTI_caso_ajustada_nb_int[["coefficients"]][["dosis2"]])))
VE_caso_nb_3_int <- (1-exp(as.numeric(RTI_caso_ajustada_nb_int[["coefficients"]][["dosis3"]])))
VE_caso_ic_nb_int <- 1-exp(confint(RTI_caso_ajustada_nb_int))

# Modelo binomial negativo para uci con interacción entre dosis/edad y dosis/semana
RTI_uci_ajustada_nb_int <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad + dosis*edad + semana, data = ejemplo_desenlace[["uci"]])
summary(RTI_uci_ajustada_nb_int)
VE_uci_nb_2_int <- (1-exp(as.numeric(RTI_uci_ajustada_nb_int[["coefficients"]][["dosis2"]])))
VE_uci_nb_3_int <- (1-exp(as.numeric(RTI_uci_ajustada_nb_int[["coefficients"]][["dosis3"]])))
VE_uci_ic_nb_int <- 1-exp(confint(RTI_uci_ajustada_nb_int))

# Modelo binomial negativo para defunciones con interacción entre dosis y edad
RTI_def_ajustada_nb_int <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad + dosis*edad + semana, data = ejemplo_desenlace[["def"]])
summary(RTI_def_ajustada_nb_int)
VE_def_nb_2_int <- (1-exp(as.numeric(RTI_def_ajustada_nb_int[["coefficients"]][["dosis2"]])))
VE_def_nb_3_int <- (1-exp(as.numeric(RTI_def_ajustada_nb_int[["coefficients"]][["dosis3"]])))
VE_def_ic_nb_int <- 1-exp(confint(RTI_def_ajustada_nb_int))

# Comparación de ajuste entre modelos para caso
logLik(RTI_caso_ajustada_nb)
logLik(RTI_caso_ajustada_nb_int)
pchisq(2 * (logLik(RTI_caso_ajustada_nb_int) - logLik(RTI_caso_ajustada_nb)), df = 1, lower.tail = FALSE)

# Se considera el modelo con interacción para los casos y sin interacción para las hospitalizaciones en UCI y defunciones

plot(allEffects(RTI_caso_ajustada_nb), axes=list(grid=FALSE,  y=list(transform=exp, lab="N° de casos (escala aritmética)")))
plot(allEffects(RTI_caso_ajustada_nb_int), axes=list(grid=FALSE,  y=list(transform=exp, lab="N° de casos (escala aritmética)")))
plot(allEffects(RTI_uci_ajustada_nb), axes=list(grid=FALSE,  y=list(transform=exp, lab="N° de casos (escala aritmética)")))
plot(allEffects(RTI_def_ajustada_nb), axes=list(grid=FALSE,  y=list(transform=exp, lab="N° de casos (escala aritmética)")))
plot(predictorEffects(RTI_caso_ajustada_nb, ~ dosis), main = "Casos", axes=list(grid=FALSE,  y=list(transform=exp, lab="N° de casos (escala aritmética)")))
plot(predictorEffects(RTI_caso_ajustada_nb_int, ~ dosis), main = "Casos", axes=list(grid=FALSE,  y=list(transform=exp, lab="N° de casos (escala aritmética)")))
plot(predictorEffects(RTI_uci_ajustada_nb, ~ dosis), main = "UCI", axes=list(grid=FALSE,  y=list(transform=exp, lab="N° de casos (escala aritmética)")))
plot(predictorEffects(RTI_def_ajustada_nb, ~ dosis), main = "Defunciones", axes=list(grid=FALSE,  y=list(transform=exp, lab="N° de casos (escala aritmética)")))

# Modelos mensuales
ejemplo$mensuales <- NA
ejemplo <- mutate(ejemplo, mensual = ifelse(semana %in% 31:34, "31-34",
                                          ifelse(semana %in% 35:38, "35-38",
                                                ifelse(semana %in% 39:42, "39-42", 
                                                       ifelse(semana %in% 43:46, "43-46",
                                                              ifelse(semana %in% 47:50, "47-50", 
                                                                     ifelse(semana %in% 51:54, "51-54", NA)))))))
ejemplo$mensual <- as.factor(ejemplo$mensual)
ejemplo %>% 
  group_by(dosis, desenlace, mensual, edad) %>% 
  summarise(freq = sum(freq), poblacion = sum(poblacion))

ejemplo_desenlace <- split(ejemplo, ejemplo$desenlace)

# Modelo de binomial negativo para caso (mensual)
RTI_caso_ajustada_nb_mensual <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad + mensual, data = ejemplo_desenlace[["caso"]])
summary(RTI_caso_ajustada_nb_mensual)
VE_caso_nb_2_mensual <- (1-exp(as.numeric(RTI_caso_ajustada_nb_mensual[["coefficients"]][["dosis2"]])))
VE_caso_nb_3_mensual <- (1-exp(as.numeric(RTI_caso_ajustada_nb_mensual[["coefficients"]][["dosis3"]])))
VE_caso_ic_nb_mensual <- 1-exp(confint(RTI_caso_ajustada_nb_mensual))

# Modelo binomial negativo para uci (mensual)
RTI_uci_ajustada_nb_mensual <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad + mensual, data = ejemplo_desenlace[["uci"]])
summary(RTI_uci_ajustada_nb_mensual)
VE_uci_nb_2_mensual <- (1-exp(as.numeric(RTI_uci_ajustada_nb_mensual[["coefficients"]][["dosis2"]])))
VE_uci_nb_3_mensual <- (1-exp(as.numeric(RTI_uci_ajustada_nb_mensual[["coefficients"]][["dosis3"]])))
VE_uci_ic_nb_mensual <- 1-exp(confint(RTI_uci_ajustada_nb_mensual))

# Modelo binomial negativo para defunciones (mensual)
RTI_def_ajustada_nb_mensual <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad + mensual, data = ejemplo_desenlace[["def"]])
summary(RTI_def_ajustada_nb_mensual)
VE_def_nb_2_mensual <- (1-exp(as.numeric(RTI_def_ajustada_nb_mensual[["coefficients"]][["dosis2"]])))
VE_def_nb_3_mensual <- (1-exp(as.numeric(RTI_def_ajustada_nb_mensual[["coefficients"]][["dosis3"]])))
VE_def_ic_nb_mensual <- 1-exp(confint(RTI_def_ajustada_nb_mensual))

# Tabla de efectividad (mensual)
efectividad_mensual <- data.frame(Esquema = rep(c("Dos dosis", "Tres dosis"), times = 3),
                          Desenlace = as.factor(c("Caso", "Caso", "UCI", "UCI", "Fallecer", "Fallecer")),
                          Efectividad = c(VE_caso_nb_2_mensual, VE_caso_nb_3_mensual, VE_uci_nb_2_mensual, VE_uci_nb_3_mensual, VE_def_nb_2_mensual, VE_def_nb_3_mensual),
                          lb = c(VE_caso_ic_nb_mensual[2,2], VE_caso_ic_nb_mensual[3,2], VE_uci_ic_nb_mensual[2,2], VE_uci_ic_nb_mensual[3,2], VE_def_ic_nb_mensual[2,2], VE_def_ic_nb_mensual[3,2]),
                          ub = c(VE_caso_ic_nb_mensual[2,1], VE_caso_ic_nb_mensual[3,1], VE_uci_ic_nb_mensual[2,1], VE_uci_ic_nb_mensual[3,1], VE_def_ic_nb_mensual[2,1], VE_def_ic_nb_mensual[3,1]))
efectividad_mensual$Desenlace <- factor(efectividad$Desenlace, levels = c("Caso", "UCI", "Fallecer")) 

g.efectividad_mensual <- ggplot(efectividad_mensual, aes(x = Esquema, y=Efectividad)) + 
  geom_errorbar(aes(ymin=lb, ymax=ub, color=Esquema), width=.1) +
  ggtitle ("Efectividad de la vacunación para esquema de dos y tres dosis (mensual)") +
  geom_point(aes(fill=Esquema),size=2, shape=23) +
  theme(text=element_text(size=20)) +
  theme(axis.text=element_text(size=20)) +
  theme(axis.title=element_text(size=20)) +
  theme(plot.title=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limit = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, by = 0.2), labels = scales::percent) +
  facet_wrap(~Desenlace)                                    

ggsave(plot = g.efectividad_mensual,
       filename = "/Users/Usuario/Desktop/efectividad ajustada (mensual).png",
       device = "png",
       dpi = "retina",
       width = 14, height = 12)

# Predichos para caso

ejemplo <- read_excel("~/GitHub/metodo_tamizaje/ejemplo_long.xlsx")
View(ejemplo)
ejemplo$edad <- as.factor(ejemplo$edad)
ejemplo$desenlace <- as.factor(ejemplo$desenlace)
ejemplo$semana <- as.factor(ejemplo$semana)
ejemplo$dosis <- as.factor(ejemplo$dosis)

#Dividir base de datos
ejemplo_desenlace <- split(ejemplo, ejemplo$desenlace)

ejemplo_desenlace_caso <- (ejemplo_desenlace$caso[complete.cases(ejemplo_desenlace$caso), ])
predichos_casos <- predict.glm(RTI_caso_ajustada_nb_int, newdata = ejemplo_desenlace_caso, type = "response", se.fit = TRUE)
ejemplo_desenlace_caso <- cbind(ejemplo_desenlace_caso, predichos_casos)

ejemplo_desenlace_caso <- ejemplo_desenlace_caso %>% 
                          group_by(dosis, semana) %>% 
                          summarise(freq = sum(freq), fit = sum(fit), se.fit = sum(se.fit), poblacion = sum(poblacion))

ejemplo_desenlace_caso$tasa <- (ejemplo_desenlace_caso$freq / ejemplo_desenlace_caso$poblacion)*10^5
ejemplo_desenlace_caso$pred_lb <- ejemplo_desenlace_caso$fit - 1.96*ejemplo_desenlace_caso$se.fit
ejemplo_desenlace_caso$pred_ub <- ejemplo_desenlace_caso$fit + 1.96*ejemplo_desenlace_caso$se.fit
ejemplo_desenlace_caso$tasa_pred <- (ejemplo_desenlace_caso$fit / ejemplo_desenlace_caso$poblacion)*10^5
ejemplo_desenlace_caso$tasa_pred_lb <- (ejemplo_desenlace_caso$pred_lb / ejemplo_desenlace_caso$poblacion)*10^5
ejemplo_desenlace_caso$tasa_pred_ub <- (ejemplo_desenlace_caso$pred_ub / ejemplo_desenlace_caso$poblacion)*10^5

#Gráfico tasas ajustadas

g.caso <- ggplot(data = ejemplo_desenlace_caso, aes(x = semana, y = tasa_pred, group = dosis, color = dosis)) +
  geom_line(linetype = "dashed") +
  geom_point(data = ejemplo_desenlace_caso, aes(x = semana, y = tasa, group = dosis, color = dosis), shape = 4) +
  ggtitle ("Tasa de casos ajustada por edad para cada semana epidemiológica según esquema") +
  ylab ("Tasa ajustada por edad (100.000 habitantes)") +
  theme(text=element_text(size=40), axis.text=element_text(size = 40), axis.title=element_text(size = 40), plot.title = element_text(hjust = 0.5, size = 40)) +
  geom_ribbon( aes(ymin = tasa_pred_lb, ymax = tasa_pred_ub, fill = dosis, color = NULL), alpha = .15) +
  theme_minimal()

ggsave(plot = g.caso,
       filename = "/Users/Usuario/Desktop/tasa casos ajustada por edad.png",
       device = "png",
       dpi = "retina",
       width = 14, height = 12)

# Predichos para UCI

ejemplo_desenlace_uci <- (ejemplo_desenlace$uci[complete.cases(ejemplo_desenlace$uci), ])
predichos_uci <- predict.glm(RTI_uci_ajustada_nb, newdata = ejemplo_desenlace_uci, type = "response", se.fit = TRUE)
ejemplo_desenlace_uci <- cbind(ejemplo_desenlace_uci, predichos_uci)

ejemplo_desenlace_uci <- ejemplo_desenlace_uci %>% 
  group_by(dosis, semana) %>% 
  summarise(freq = sum(freq), fit = sum(fit), se.fit = sum(se.fit), poblacion = sum(poblacion))

ejemplo_desenlace_uci$tasa <- (ejemplo_desenlace_uci$freq / ejemplo_desenlace_uci$poblacion)*10^5
ejemplo_desenlace_uci$pred_lb <- ejemplo_desenlace_uci$fit - 1.96*ejemplo_desenlace_uci$se.fit
ejemplo_desenlace_uci$pred_ub <- ejemplo_desenlace_uci$fit + 1.96*ejemplo_desenlace_uci$se.fit
ejemplo_desenlace_uci$tasa_pred <- (ejemplo_desenlace_uci$fit / ejemplo_desenlace_uci$poblacion)*10^5
ejemplo_desenlace_uci$tasa_pred_lb <- (ejemplo_desenlace_uci$pred_lb / ejemplo_desenlace_uci$poblacion)*10^5
ejemplo_desenlace_uci$tasa_pred_ub <- (ejemplo_desenlace_uci$pred_ub / ejemplo_desenlace_uci$poblacion)*10^5

#Gráfico tasas ajustadas
g.uci <- ggplot(data = ejemplo_desenlace_uci, aes(x = semana, y = tasa_pred, group = dosis, color = dosis)) +
  geom_line(linetype = "dashed") +
  geom_point(data = ejemplo_desenlace_uci, aes(x = semana, y = tasa, group = dosis, color = dosis), shape = 4) +
  ggtitle ("Tasa de UCI ajustada por edad para cada semana epidemiológica según esquema") +
  ylab ("Tasa ajustada por edad (100.000 habitantes)") +
  theme(text=element_text(size=30)) +
  theme(axis.text=element_text(size=30)) +
  theme(axis.title=element_text(size=30)) +
  theme(plot.title=element_text(size=30)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_ribbon( aes(ymin = tasa_pred_lb, ymax = tasa_pred_ub, fill = dosis, color = NULL), alpha = .15) +
  theme_minimal()

ggsave(plot = g.uci,
       filename = "/Users/Usuario/Desktop/tasa uci ajustada por edad.png",
       device = "png",
       dpi = "retina",
       width = 14, height = 12)

# Predichos para defunciones

ejemplo_desenlace_def <- (ejemplo_desenlace$def[complete.cases(ejemplo_desenlace$def), ])
predichos_def <- predict.glm(RTI_def_ajustada_nb, newdata = ejemplo_desenlace_def, type = "response", se.fit = TRUE)
ejemplo_desenlace_def <- cbind(ejemplo_desenlace_def, predichos_def)

ejemplo_desenlace_def <- ejemplo_desenlace_def %>% 
  group_by(dosis, semana) %>% 
  summarise(freq = sum(freq), fit = sum(fit), se.fit = sum(se.fit), poblacion = sum(poblacion))

ejemplo_desenlace_def$tasa <- (ejemplo_desenlace_def$freq / ejemplo_desenlace_def$poblacion)*10^5
ejemplo_desenlace_def$pred_lb <- ejemplo_desenlace_def$fit - 1.96*ejemplo_desenlace_def$se.fit
ejemplo_desenlace_def$pred_ub <- ejemplo_desenlace_def$fit + 1.96*ejemplo_desenlace_def$se.fit
ejemplo_desenlace_def$tasa_pred <- (ejemplo_desenlace_def$fit / ejemplo_desenlace_def$poblacion)*10^5
ejemplo_desenlace_def$tasa_pred_lb <- (ejemplo_desenlace_def$pred_lb / ejemplo_desenlace_def$poblacion)*10^5
ejemplo_desenlace_def$tasa_pred_ub <- (ejemplo_desenlace_def$pred_ub / ejemplo_desenlace_def$poblacion)*10^5

#Gráfico tasas ajustadas
g.def <- ggplot(data = ejemplo_desenlace_def, aes(x = semana, y = tasa_pred, group = dosis, color = dosis)) +
  geom_line(linetype = "dashed") +
  geom_point(data = ejemplo_desenlace_def, aes(x = semana, y = tasa, group = dosis, color = dosis), shape = 4) +
  ggtitle ("Tasa de defunciones ajustada por edad para cada semana epidemiológica según esquema") +
  ylab ("Tasa ajustada por edad (100.000 habitantes)") +
  theme(text=element_text(size=30)) +
  theme(axis.text=element_text(size=30)) +
  theme(axis.title=element_text(size=30)) +
  theme(plot.title=element_text(size=30)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_ribbon( aes(ymin = tasa_pred_lb, ymax = tasa_pred_ub, fill = dosis, color = NULL), alpha = .15) +
  theme_minimal()

ggsave(plot = g.def,
       filename = "/Users/Usuario/Desktop/tasa defunciones ajustada por edad.png",
       device = "png",
       dpi = "retina",
       width = 14, height = 12)

# Modelo con semana como splines

# Cargar base de datos
ejemplo <- read_excel("~/GitHub/metodo_tamizaje/ejemplo_long.xlsx")
View(ejemplo)

ejemplo$edad <- as.factor(ejemplo$edad)
ejemplo$desenlace <- as.factor(ejemplo$desenlace)
ejemplo$dosis <- as.factor(ejemplo$dosis)

#Dividir base de datos
ejemplo_desenlace <- split(ejemplo, ejemplo$desenlace)

# Modelo de binomial negativo para caso con semana como spline
RTI_caso_ajustada_nb_spline <- glm.nb(freq ~ offset(log(poblacion)) + dosis + dosis*edad + edad + dosis*ns(x = semana, df = 6) + ns(x = semana, df = 8), data = ejemplo_desenlace[["caso"]])
summary(RTI_caso_ajustada_nb_spline)

# Predichos para casos con semana como spline
ejemplo_desenlace_caso <- (ejemplo_desenlace$caso[complete.cases(ejemplo_desenlace$caso), ])
predichos_casos <- predict.glm(RTI_caso_ajustada_nb_spline, newdata = ejemplo_desenlace_caso, type = "response", se.fit = TRUE)
ejemplo_desenlace_caso <- cbind(ejemplo_desenlace_caso, predichos_casos)

ejemplo_desenlace_caso <- ejemplo_desenlace_caso %>% 
  group_by(dosis, semana) %>% 
  summarise(freq = sum(freq), fit = sum(fit), se.fit = sum(se.fit), poblacion = sum(poblacion))

ejemplo_desenlace_caso$tasa <- (ejemplo_desenlace_caso$freq / ejemplo_desenlace_caso$poblacion)*10^5
ejemplo_desenlace_caso$pred_lb <- ejemplo_desenlace_caso$fit - 1.96*ejemplo_desenlace_caso$se.fit
ejemplo_desenlace_caso$pred_ub <- ejemplo_desenlace_caso$fit + 1.96*ejemplo_desenlace_caso$se.fit
ejemplo_desenlace_caso$tasa_pred <- (ejemplo_desenlace_caso$fit / ejemplo_desenlace_caso$poblacion)*10^5
ejemplo_desenlace_caso$tasa_pred_lb <- (ejemplo_desenlace_caso$pred_lb / ejemplo_desenlace_caso$poblacion)*10^5
ejemplo_desenlace_caso$tasa_pred_ub <- (ejemplo_desenlace_caso$pred_ub / ejemplo_desenlace_caso$poblacion)*10^5

g.caso.spline <- ggplot(data = ejemplo_desenlace_caso, aes(x = semana, y = tasa_pred, group = dosis, color = dosis)) +
  geom_line(linetype = "dashed") +
  geom_point(data = ejemplo_desenlace_caso, aes(x = semana, y = tasa, group = dosis, color = dosis), shape = 4) +
  ggtitle ("Tasa de casos ajustada por edad para cada semana epidemiológica según esquema") +
  ylab ("Tasa ajustada por edad (100.000 habitantes)") +
  theme(text=element_text(size=20)) +
  theme(axis.text=element_text(size=20)) +
  theme(axis.title=element_text(size=20)) +
  theme(plot.title=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_ribbon( aes(ymin = tasa_pred_lb, ymax = tasa_pred_ub, fill = dosis, color = NULL), alpha = .15)

ggsave(plot = g.caso.spline,
       filename = "/Users/Usuario/Desktop/tasa de casos ajustados (spline).png",
       device = "png",
       dpi = "retina",
       width = 14, height = 12)

# Comparación de ajuste entre modelos para caso
logLik(RTI_caso_ajustada_nb_int)
logLik(RTI_caso_ajustada_nb_spline)
pchisq(2 * (logLik(RTI_caso_ajustada_nb_spline) - logLik(RTI_caso_ajustada_nb_int)), df = 1, lower.tail = FALSE)

# Modelo binomial negativo para uci con semana como spline
RTI_uci_ajustada_nb_spline <- glm.nb(freq ~ offset(log(poblacion)) + dosis + dosis*ns(x = semana, df = 6) + edad + ns(x = semana, df = 8), data = ejemplo_desenlace[["uci"]])
summary(RTI_uci_ajustada_nb_spline)

ejemplo_desenlace_uci <- (ejemplo_desenlace$uci[complete.cases(ejemplo_desenlace$uci), ])
predichos_uci <- predict.glm(RTI_uci_ajustada_nb_spline, newdata = ejemplo_desenlace_uci, type = "response", se.fit = TRUE)
ejemplo_desenlace_uci <- cbind(ejemplo_desenlace_uci, predichos_uci)

ejemplo_desenlace_uci <- ejemplo_desenlace_uci %>% 
  group_by(dosis, semana) %>% 
  summarise(freq = sum(freq), fit = sum(fit), se.fit = sum(se.fit), poblacion = sum(poblacion))

ejemplo_desenlace_uci$tasa <- (ejemplo_desenlace_uci$freq / ejemplo_desenlace_uci$poblacion)*10^5
ejemplo_desenlace_uci$pred_lb <- ejemplo_desenlace_uci$fit - 1.96*ejemplo_desenlace_uci$se.fit
ejemplo_desenlace_uci$pred_ub <- ejemplo_desenlace_uci$fit + 1.96*ejemplo_desenlace_uci$se.fit
ejemplo_desenlace_uci$tasa_pred <- (ejemplo_desenlace_uci$fit / ejemplo_desenlace_uci$poblacion)*10^5
ejemplo_desenlace_uci$tasa_pred_lb <- (ejemplo_desenlace_uci$pred_lb / ejemplo_desenlace_uci$poblacion)*10^5
ejemplo_desenlace_uci$tasa_pred_ub <- (ejemplo_desenlace_uci$pred_ub / ejemplo_desenlace_uci$poblacion)*10^5

ejemplo_desenlace_uci <- ejemplo_desenlace_uci %>% 
  group_by(dosis, semana) %>% 
  summarise(freq = sum(freq), tasa = sum(tasa), tasa_pred = sum(tasa_pred), tasa_pred_lb = sum(tasa_pred_lb), tasa_pred_ub = sum(tasa_pred_ub), pred_lb = sum(pred_lb), pred_ub = sum(pred_ub))

#Gráfico tasas ajustadas
g.uci.spline <- ggplot(data = ejemplo_desenlace_uci, aes(x = semana, y = tasa_pred, group = dosis, color = dosis)) +
  geom_line(linetype = "dashed") +
  geom_point(data = ejemplo_desenlace_uci, aes(x = semana, y = tasa, group = dosis, color = dosis), shape = 4) +
  ggtitle ("Tasa de UCI ajustada por edad para cada semana epidemiológica según esquema") +
  ylab ("Tasa ajustada por edad (100.000 habitantes)") +
  theme(text=element_text(size=20)) +
  theme(axis.text=element_text(size=20)) +
  theme(axis.title=element_text(size=20)) +
  theme(plot.title=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_ribbon( aes(ymin = tasa_pred_lb, ymax = tasa_pred_ub, fill = dosis, color = NULL), alpha = .15)

ggsave(plot = g.uci.spline,
       filename = "/Users/Usuario/Desktop/tasa de uci ajustados (spline).png",
       device = "png",
       dpi = "retina",
       width = 14, height = 12)

# Comparación de ajuste entre modelos para caso
logLik(RTI_uci_ajustada_nb)
logLik(RTI_uci_ajustada_nb_spline)
pchisq(2 * (logLik(RTI_uci_ajustada_nb_spline) - logLik(RTI_uci_ajustada_nb)), df = 1, lower.tail = FALSE)

# Modelo binomial negativo para defunciones
RTI_def_ajustada_nb_spline <- glm.nb(freq ~ offset(log(poblacion)) + dosis + dosis*ns(x = semana, df = 6) + edad + ns(x = semana, df = 8), data = ejemplo_desenlace[["def"]])
summary(RTI_def_ajustada_nb)

ejemplo_desenlace_def <- (ejemplo_desenlace$def[complete.cases(ejemplo_desenlace$def), ])
predichos_def <- predict.glm(RTI_def_ajustada_nb_spline, newdata = ejemplo_desenlace_def, type = "response", se.fit = TRUE)
ejemplo_desenlace_def <- cbind(ejemplo_desenlace_def, predichos_def)

ejemplo_desenlace_def <- ejemplo_desenlace_def %>% 
  group_by(dosis, semana) %>% 
  summarise(freq = sum(freq), fit = sum(fit), se.fit = sum(se.fit), poblacion = sum(poblacion))

ejemplo_desenlace_def$tasa <- (ejemplo_desenlace_def$freq / ejemplo_desenlace_def$poblacion)*10^5
ejemplo_desenlace_def$pred_lb <- ejemplo_desenlace_def$fit - 1.96*ejemplo_desenlace_def$se.fit
ejemplo_desenlace_def$pred_ub <- ejemplo_desenlace_def$fit + 1.96*ejemplo_desenlace_def$se.fit
ejemplo_desenlace_def$tasa_pred <- (ejemplo_desenlace_def$fit / ejemplo_desenlace_def$poblacion)*10^5
ejemplo_desenlace_def$tasa_pred_lb <- (ejemplo_desenlace_def$pred_lb / ejemplo_desenlace_def$poblacion)*10^5
ejemplo_desenlace_def$tasa_pred_ub <- (ejemplo_desenlace_def$pred_ub / ejemplo_desenlace_def$poblacion)*10^5

#Gráfico tasas ajustadas

g.def.spline <- ggplot(data = ejemplo_desenlace_def, aes(x = semana, y = tasa_pred, group = dosis, color = dosis)) +
  geom_line(linetype = "dashed") +
  geom_point(data = ejemplo_desenlace_def, aes(x = semana, y = tasa, group = dosis, color = dosis), shape = 4) +
  ggtitle ("Tasa de defunciones ajustada por edad para cada semana epidemiológica según esquema") +
  ylab ("Tasa ajustada por edad (100.000 habitantes)") +
  theme(text=element_text(size=20)) +
  theme(axis.text=element_text(size=20)) +
  theme(axis.title=element_text(size=20)) +
  theme(plot.title=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_ribbon( aes(ymin = tasa_pred_lb, ymax = tasa_pred_ub, fill = dosis, color = NULL), alpha = .15)

ggsave(plot = g.def.spline,
       filename = "/Users/Usuario/Desktop/tasa defunciones ajustada por edad (spline).png",
       device = "png",
       dpi = "retina",
       width = 14, height = 12)

# Comparación de ajuste entre modelos para caso
logLik(RTI_def_ajustada_nb)
logLik(RTI_def_ajustada_nb_spline)
pchisq(2 * (logLik(RTI_def_ajustada_nb_spline) - logLik(RTI_def_ajustada_nb)), df = 1, lower.tail = FALSE)

# Comparación de escenario con vacuna y sin vacuna para casos

ejemplo_desenlace_caso <- (ejemplo_desenlace$caso[complete.cases(ejemplo_desenlace$caso), ])
predichos_casos <- predict.glm(RTI_caso_ajustada_nb_spline, newdata = ejemplo_desenlace_caso, type = "response", se.fit = TRUE)
predichos_casos$residual.scale <- NULL
ejemplo_desenlace_caso <- ejemplo_desenlace_caso %>% mutate(dosis = "0")
predichos_casos_0 <- predict.glm(RTI_caso_ajustada_nb_spline, newdata = ejemplo_desenlace_caso, type = "response", se.fit = TRUE)
predichos_casos_0$residual.scale <- NULL
predichos_casos_0$fit0 <- predichos_casos_0$fit
predichos_casos_0$se.fit0 <- predichos_casos_0$se.fit
predichos_casos_0$fit <- NULL
predichos_casos_0$se.fit <- NULL
ejemplo_desenlace_caso <- cbind(ejemplo_desenlace_caso, predichos_casos)
ejemplo_desenlace_caso <- cbind(ejemplo_desenlace_caso, predichos_casos_0)
ejemplo_desenlace_caso$residual.scale <- NULL

ejemplo_desenlace_caso <- ejemplo_desenlace_caso %>% 
  group_by(semana) %>% 
  summarise(freq = sum(freq), fit = sum(fit), fit0 = sum(fit0), se.fit = sum(se.fit), se.fit0 = sum(se.fit0))

ejemplo_desenlace_caso$pred_lb <- ejemplo_desenlace_caso$fit - 1.96*ejemplo_desenlace_caso$se.fit
ejemplo_desenlace_caso$pred_ub <- ejemplo_desenlace_caso$fit + 1.96*ejemplo_desenlace_caso$se.fit
ejemplo_desenlace_caso$pred_lb0 <- ejemplo_desenlace_caso$fit0 - 1.96*ejemplo_desenlace_caso$se.fit0
ejemplo_desenlace_caso$pred_ub0 <- ejemplo_desenlace_caso$fit0 + 1.96*ejemplo_desenlace_caso$se.fit0

ggplot(data = ejemplo_desenlace_caso) +
  geom_line(aes(x = semana, y = fit, group = 1, color = "red")) +
  geom_line(aes(x = semana, y = fit0, group = 1, color = "blue")) +
  geom_ribbon(aes(x = semana, ymin = pred_lb, ymax = pred_ub, group = 1, color = "red"), alpha = .15) +
  geom_ribbon(aes(x = semana, ymin = pred_lb0, ymax = pred_ub0, group = 1, color = "blue"), alpha = .15) +
  ggtitle("N° de casos semanales en escenario con vacunación (azul) y sin vacunación (rojo)") +
  ylab("N° de casos") +
  theme_minimal()

# Comparación de escenario con vacuna y sin vacuna para UCI

ejemplo_desenlace_uci <- (ejemplo_desenlace$uci[complete.cases(ejemplo_desenlace$uci), ])
predichos_uci <- predict.glm(RTI_uci_ajustada_nb_spline, newdata = ejemplo_desenlace_uci, type = "response", se.fit = TRUE)
predichos_uci$residual.scale <- NULL
ejemplo_desenlace_uci <- ejemplo_desenlace_uci %>% mutate(dosis = "0")
predichos_uci_0 <- predict.glm(RTI_uci_ajustada_nb_spline, newdata = ejemplo_desenlace_uci, type = "response", se.fit = TRUE)
predichos_uci_0$residual.scale <- NULL
predichos_uci_0$fit0 <- predichos_uci_0$fit
predichos_uci_0$se.fit0 <- predichos_uci_0$se.fit
predichos_uci_0$fit <- NULL
predichos_uci_0$se.fit <- NULL
ejemplo_desenlace_uci <- cbind(ejemplo_desenlace_uci, predichos_uci)
ejemplo_desenlace_uci <- cbind(ejemplo_desenlace_uci, predichos_uci_0)
ejemplo_desenlace_uci$residual.scale <- NULL

ejemplo_desenlace_uci <- ejemplo_desenlace_uci %>% 
  group_by(semana) %>% 
  summarise(freq = sum(freq), fit = sum(fit), fit0 = sum(fit0), se.fit = sum(se.fit), se.fit0 = sum(se.fit0))

ejemplo_desenlace_uci$pred_lb <- ejemplo_desenlace_uci$fit - 1.96*ejemplo_desenlace_uci$se.fit
ejemplo_desenlace_uci$pred_ub <- ejemplo_desenlace_uci$fit + 1.96*ejemplo_desenlace_uci$se.fit
ejemplo_desenlace_uci$pred_lb0 <- ejemplo_desenlace_uci$fit0 - 1.96*ejemplo_desenlace_uci$se.fit0
ejemplo_desenlace_uci$pred_ub0 <- ejemplo_desenlace_uci$fit0 + 1.96*ejemplo_desenlace_uci$se.fit0

ggplot(data = ejemplo_desenlace_uci) +
  geom_line(aes(x = semana, y = fit, group = 1, color = "red")) +
  geom_line(aes(x = semana, y = fit0, group = 1, color = "blue")) +
  geom_ribbon(aes(x = semana, ymin = pred_lb, ymax = pred_ub, group = 1, color = "red"), alpha = .15) +
  geom_ribbon(aes(x = semana, ymin = pred_lb0, ymax = pred_ub0, group = 1, color = "blue"), alpha = .15) +
  ggtitle("N° de casos en UCI semanales en escenario con vacunación (azul) y sin vacunación (rojo)") +
  ylab("N° de casos UCI") +
  theme_minimal()

# Comparación de escenario con vacuna y sin vacuna para defunciones

ejemplo_desenlace_def <- (ejemplo_desenlace$def[complete.cases(ejemplo_desenlace$def), ])
predichos_def <- predict.glm(RTI_def_ajustada_nb_spline, newdata = ejemplo_desenlace_def, type = "response", se.fit = TRUE)
predichos_def$residual.scale <- NULL
ejemplo_desenlace_def <- ejemplo_desenlace_def %>% mutate(dosis = "0")
predichos_def_0 <- predict.glm(RTI_def_ajustada_nb_spline, newdata = ejemplo_desenlace_def, type = "response", se.fit = TRUE)
predichos_def_0$residual.scale <- NULL
predichos_def_0$fit0 <- predichos_def_0$fit
predichos_def_0$se.fit0 <- predichos_def_0$se.fit
predichos_def_0$fit <- NULL
predichos_def_0$se.fit <- NULL
ejemplo_desenlace_def <- cbind(ejemplo_desenlace_def, predichos_def)
ejemplo_desenlace_def <- cbind(ejemplo_desenlace_def, predichos_def_0)
ejemplo_desenlace_def$residual.scale <- NULL

ejemplo_desenlace_def <- ejemplo_desenlace_def %>% 
  group_by(semana) %>% 
  summarise(freq = sum(freq), fit = sum(fit), fit0 = sum(fit0), se.fit = sum(se.fit), se.fit0 = sum(se.fit0))

ejemplo_desenlace_def$pred_lb <- ejemplo_desenlace_def$fit - 1.96*ejemplo_desenlace_def$se.fit
ejemplo_desenlace_def$pred_ub <- ejemplo_desenlace_def$fit + 1.96*ejemplo_desenlace_def$se.fit
ejemplo_desenlace_def$pred_lb0 <- ejemplo_desenlace_def$fit0 - 1.96*ejemplo_desenlace_def$se.fit0
ejemplo_desenlace_def$pred_ub0 <- ejemplo_desenlace_def$fit0 + 1.96*ejemplo_desenlace_def$se.fit0

ggplot(data = ejemplo_desenlace_def) +
  geom_line(aes(x = semana, y = fit, group = 1, color = "red")) +
  geom_line(aes(x = semana, y = fit0, group = 1, color = "blue")) +
  geom_ribbon(aes(x = semana, ymin = pred_lb, ymax = pred_ub, group = 1, color = "red"), alpha = .15) +
  geom_ribbon(aes(x = semana, ymin = pred_lb0, ymax = pred_ub0, group = 1, color = "blue"), alpha = .15) +
  ggtitle("N° de defunciones semanales en escenario con vacunación (azul) y sin vacunación (rojo)") +
  ylab("N° de defunciones") +
  theme_minimal()

# Modelo de efectividad por semana #

# Cargar base de datos
ejemplo <- read_excel("~/GitHub/metodo_tamizaje/ejemplo_long.xlsx")

#Definir variables categóricas
ejemplo$edad <- as.factor(ejemplo$edad)
ejemplo$desenlace <- as.factor(ejemplo$desenlace)
ejemplo$semana <- as.factor(ejemplo$semana)
ejemplo$dosis <- as.factor(ejemplo$dosis)

#Dividir base de datos
ejemplo_desenlace <- split(ejemplo, ejemplo[["desenlace"]])

ejemplo_desenlace_caso <- split(ejemplo_desenlace[["caso"]], ejemplo_desenlace[["caso"]][["semana"]])
ejemplo_desenlace_caso[["31"]] <- NULL
ejemplo_desenlace_caso[["32"]] <- NULL
ejemplo_desenlace_caso[["33"]] <- NULL
ejemplo_desenlace_caso[["34"]] <- NULL

size <- length(ejemplo_desenlace_caso)

VE_caso_ic <- vector('list', length = size)

lista_caso <- list(modelo = vector('list', size),
                   VE_caso_2 = vector('numeric', size),
                   VE_caso_3 = vector('numeric', size),
                   VE_caso_ic = VE_caso_ic)
VE_caso_2_ic_inf <- vector()
VE_caso_2_ic_sup <- vector()
VE_caso_3_ic_inf <- vector()
VE_caso_3_ic_sup <- vector()

for (i in 1:length(ejemplo_desenlace_caso)) {
  lista_caso$modelo[[i]] <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad,
                           data = ejemplo_desenlace_caso[[i]])
  lista_caso$VE_caso_2[[i]] <- 1-exp(as.numeric(lista_caso[["modelo"]][[i]][["coefficients"]][["dosis2"]]))
  lista_caso$VE_caso_3[[i]] <- 1-exp(as.numeric(lista_caso[["modelo"]][[i]][["coefficients"]][["dosis3"]]))
  lista_caso$VE_caso_ic[[i]] <- 1-exp(confint(lista_caso[["modelo"]][[i]]))
  VE_caso_2_ic_inf[[i]] <- lista_caso[["VE_caso_ic"]][[i]][2,2]
  VE_caso_2_ic_sup[[i]] <- lista_caso[["VE_caso_ic"]][[i]][2,1]
  VE_caso_3_ic_inf[[i]] <- lista_caso[["VE_caso_ic"]][[i]][3,2]
  VE_caso_3_ic_sup[[i]] <- lista_caso[["VE_caso_ic"]][[i]][3,1]
}

efectividad_caso <- data.frame(semana = rep(c(35:54), times = 2),
                               esquema = rep(c("Dos dosis", "Tres dosis"), each = 20),
                               efectividad = c(lista_caso$VE_caso_2, lista_caso$VE_caso_3),
                               ic_inf = c(VE_caso_2_ic_inf, VE_caso_3_ic_inf),
                               ic_sup = c(VE_caso_2_ic_sup, VE_caso_3_ic_sup))

ggplot(efectividad_caso, aes(x = semana, y = efectividad, colour = esquema)) + 
                      geom_line() +
                      geom_ribbon(aes(x = semana, ymin =ic_inf, ymax = ic_sup, colour = esquema), alpha = .15) +
                      ggtitle ("Efectividad ajustada para cada semana") +
                      scale_y_continuous(limit = c(0, 1), expand = c(0, 0), labels = scales::percent)

# Efectividad Delta versus Omicron

ejemplo <- read_excel("~/GitHub/metodo_tamizaje/ejemplo_long.xlsx")

ejemplo$variante <- NA
ejemplo <- mutate(ejemplo, variante = ifelse(semana %in% 35:52, "Delta",
                                            ifelse(semana %in% 53:56, "Omicron", NA)))
ejemplo <- ejemplo[ejemplo$semana >34,]

ejemplo$freq[1329:1336] <- c(0,0,0,0,0,0,0,0)
ejemplo$poblacion[1329:1336] <- c(0,0,0,0,0,0,0,0)

ejemplo$variante <- as.factor(ejemplo$variante)
ejemplo$dosis <- as.factor(ejemplo$dosis)

ejemplo <- ejemplo %>% 
  group_by(dosis, desenlace, variante, edad) %>% 
  summarise(freq = sum(freq), poblacion = sum(poblacion))

ejemplo_variante <- split(ejemplo, ejemplo$variante)
ejemplo_desenlace_delta <- split(ejemplo_variante$Delta, ejemplo_variante$Delta$desenlace)
ejemplo_desenlace_omicron <- split(ejemplo_variante$Omicron, ejemplo_variante$Omicron$desenlace)

# Modelo de binomial negativo para caso (delta)
RTI_caso_ajustada_nb_delta <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad, data = ejemplo_desenlace_delta[["caso"]])
summary(RTI_caso_ajustada_nb_delta)
VE_caso_nb_2_delta <- (1-exp(as.numeric(RTI_caso_ajustada_nb_delta[["coefficients"]][["dosis2"]])))
VE_caso_nb_3_delta <- (1-exp(as.numeric(RTI_caso_ajustada_nb_delta[["coefficients"]][["dosis3"]])))
VE_caso_ic_nb_delta <- 1-exp(confint(RTI_caso_ajustada_nb_delta))

# Modelo de binomial negativo para caso (omicron)
RTI_caso_ajustada_nb_omicron <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad, data = ejemplo_desenlace_omicron[["caso"]])
summary(RTI_caso_ajustada_nb_omicron)
VE_caso_nb_2_omicron <- (1-exp(as.numeric(RTI_caso_ajustada_nb_omicron[["coefficients"]][["dosis2"]])))
VE_caso_nb_3_omicron <- (1-exp(as.numeric(RTI_caso_ajustada_nb_omicron[["coefficients"]][["dosis3"]])))
VE_caso_ic_nb_omicron <- 1-exp(confint(RTI_caso_ajustada_nb_omicron))

# Modelo de binomial negativo para uci (delta)
RTI_uci_ajustada_nb_delta <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad, data = ejemplo_desenlace_delta[["uci"]])
summary(RTI_uci_ajustada_nb_delta)
VE_uci_nb_2_delta <- (1-exp(as.numeric(RTI_uci_ajustada_nb_delta[["coefficients"]][["dosis2"]])))
VE_uci_nb_3_delta <- (1-exp(as.numeric(RTI_uci_ajustada_nb_delta[["coefficients"]][["dosis3"]])))
VE_uci_ic_nb_delta <- 1-exp(confint(RTI_uci_ajustada_nb_delta))

# Modelo de binomial negativo para uci (omicron)
RTI_uci_ajustada_nb_omicron <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad, data = ejemplo_desenlace_omicron[["uci"]])
summary(RTI_uci_ajustada_nb_omicron)
VE_uci_nb_2_omicron <- (1-exp(as.numeric(RTI_uci_ajustada_nb_omicron[["coefficients"]][["dosis2"]])))
VE_uci_nb_3_omicron <- (1-exp(as.numeric(RTI_uci_ajustada_nb_omicron[["coefficients"]][["dosis3"]])))
VE_uci_ic_nb_omicron <- 1-exp(confint(RTI_uci_ajustada_nb_omicron))

# Modelo de binomial negativo para defunciones (delta)
RTI_def_ajustada_nb_delta <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad, data = ejemplo_desenlace_delta[["def"]])
summary(RTI_def_ajustada_nb_delta)
VE_def_nb_2_delta <- (1-exp(as.numeric(RTI_def_ajustada_nb_delta[["coefficients"]][["dosis2"]])))
VE_def_nb_3_delta <- (1-exp(as.numeric(RTI_def_ajustada_nb_delta[["coefficients"]][["dosis3"]])))
VE_def_ic_nb_delta <- 1-exp(confint(RTI_def_ajustada_nb_delta))

# Modelo de binomial negativo para uci (omicron)
RTI_def_ajustada_nb_omicron <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad, data = ejemplo_desenlace_omicron[["def"]])
summary(RTI_def_ajustada_nb_omicron)
VE_def_nb_2_omicron <- (1-exp(as.numeric(RTI_def_ajustada_nb_omicron[["coefficients"]][["dosis2"]])))
VE_def_nb_3_omicron <- (1-exp(as.numeric(RTI_def_ajustada_nb_omicron[["coefficients"]][["dosis3"]])))
VE_def_ic_nb_omicron <- 1-exp(confint(RTI_def_ajustada_nb_omicron))

# Tabla de efectividad (mensual)
efectividad_variante <- data.frame(Variante = rep(c("Delta (semana 35 a 52)", "Omicron (semana 53 a 56)"), each = 6),
                                   Desenlace = rep(c("Caso", "UCI", "Fallecer"), times = 4),
                                   Dosis = rep(rep(c("Esquema completo", "Refuerzo"), each = 3)),
                                   Efectividad = c(VE_caso_nb_2_delta, VE_uci_nb_2_delta, VE_def_nb_2_delta, VE_caso_nb_3_delta, VE_uci_nb_3_delta, VE_def_nb_3_delta, VE_caso_nb_2_omicron, VE_uci_nb_2_omicron, VE_def_nb_2_omicron, VE_caso_nb_3_omicron, VE_uci_nb_3_omicron, VE_def_nb_3_omicron),
                                   lb = c(VE_caso_ic_nb_delta[2,2], VE_uci_ic_nb_delta[2,2], VE_def_ic_nb_delta[2,2], VE_caso_ic_nb_delta[3,2], VE_uci_ic_nb_delta[3,2], VE_def_ic_nb_delta[3,2], 0, VE_uci_ic_nb_omicron[2,2], NA, VE_caso_ic_nb_omicron[3,2], VE_uci_ic_nb_omicron[3,2], NA),
                                   ub = c(VE_caso_ic_nb_delta[2,1], VE_uci_ic_nb_delta[2,1], VE_def_ic_nb_delta[2,1], VE_caso_ic_nb_delta[3,1], VE_uci_ic_nb_delta[3,1], VE_def_ic_nb_delta[3,1], VE_caso_ic_nb_omicron[2,1], VE_uci_ic_nb_omicron[2,1], NA, VE_caso_ic_nb_omicron[3,1], VE_uci_ic_nb_omicron[3,1], NA))
efectividad_variante$Desenlace <- factor(efectividad_variante$Desenlace, levels = c("Caso", "UCI", "Fallecer")) 

efectividad_variante_2 <- data.frame(Variante = rep(c("Delta (semana 35 a 52)", "Omicron (semana 53 a 56)"), each = 4),
                                   Desenlace = rep(c("Caso", "UCI"), times = 4),
                                   Dosis = rep(rep(c("Esquema completo", "Refuerzo"), each = 2)),
                                   Efectividad = c(VE_caso_nb_2_delta, VE_uci_nb_2_delta, VE_caso_nb_3_delta, VE_uci_nb_3_delta, VE_caso_nb_2_omicron, VE_uci_nb_2_omicron, VE_caso_nb_3_omicron, VE_uci_nb_3_omicron),
                                   lb = c(VE_caso_ic_nb_delta[2,2], VE_uci_ic_nb_delta[2,2], VE_caso_ic_nb_delta[3,2], VE_uci_ic_nb_delta[3,2], 0, VE_uci_ic_nb_omicron[2,2], 0, VE_uci_ic_nb_omicron[3,2]),
                                   ub = c(VE_caso_ic_nb_delta[2,1], VE_uci_ic_nb_delta[2,1], VE_caso_ic_nb_delta[3,1], VE_uci_ic_nb_delta[3,1], VE_caso_ic_nb_omicron[2,1], VE_uci_ic_nb_omicron[2,1], VE_caso_ic_nb_omicron[3,1], VE_uci_ic_nb_omicron[3,1]))
efectividad_variante_2$Desenlace <- factor(efectividad_variante_2$Desenlace, levels = c("Caso", "UCI")) 

g.variante <- ggplot(efectividad_variante, aes(x = Dosis, y=Efectividad)) + 
  geom_errorbar(aes(ymin=lb, ymax=ub, color= Dosis), width=.1) +
  ggtitle ("Efectividad de la vacunación para esquema de dos y tres dosis según variante predominante") +
  geom_point(aes(fill = Dosis),size=2, shape=23) +
  theme(text=element_text(size=20)) +
  theme(axis.text=element_text(size=20)) +
  theme(axis.title=element_text(size=20)) +
  theme(plot.title=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.spacing = unit(2, "lines")) +
  scale_y_continuous(limit = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, by = 0.2), labels = scales::percent) +
  facet_grid(vars(Desenlace), vars(Variante))
  

ggsave(plot = g.variante,
       filename = "/Users/Usuario/Desktop/efectividad variante 1.png",
       device = "png",
       dpi = "retina",
       width = 14, height = 12)

g.variante_2 <- ggplot(efectividad_variante_2, aes(x = Dosis, y=Efectividad)) + 
  geom_errorbar(aes(ymin=lb, ymax=ub, color= Dosis), width=.1) +
  ggtitle ("Efectividad de la vacunación para esquema de dos y tres dosis según variante predominante") +
  geom_point(aes(fill = Dosis),size=2, shape=23) +
  theme(text=element_text(size=20)) +
  theme(axis.text=element_text(size=20)) +
  theme(axis.title=element_text(size=20)) +
  theme(plot.title=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.spacing = unit(2, "lines")) +
  scale_y_continuous(limit = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, by = 0.2), labels = scales::percent) +
  facet_grid(vars(Desenlace), vars(Variante))


ggsave(plot = g.variante_2,
       filename = "/Users/Usuario/Desktop/efectividad variante 2.png",
       device = "png",
       dpi = "retina",
       width = 14, height = 12)