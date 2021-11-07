# Cargar base de datos
library(readxl)
ejemplo <- read_excel("~/GitHub/metodo_tamizaje/ejemplo_long.xlsx")
View(ejemplo)

#Definir variables categóricas
ejemplo$edad <- as.factor(ejemplo$edad)
ejemplo$semana <- as.factor(ejemplo$semana)
ejemplo$desenlace <- as.factor(ejemplo$desenlace)
ejemplo$dosis <- as.factor(ejemplo$dosis)

#Dividir base de datos
ejemplo_desenlace <- split(ejemplo, ejemplo$desenlace)

# Modelo de Poisson para caso
RTI_caso_ajustada <- glm(freq ~ offset(log(poblacion)) + dosis + edad + semana, family = "poisson", data = ejemplo_desenlace[["caso"]])
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
library(MASS)
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

library(ggplot2)

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

library(effects)
plot(allEffects(RTI_caso_ajustada_nb))
plot(allEffects(RTI_uci_ajustada_nb))
plot(allEffects(RTI_def_ajustada_nb))
plot(predictorEffects(RTI_caso_ajustada_nb, ~ dosis), main = "Casos", axes=list(grid=FALSE,  y=list(transform=exp, lab="N° de casos (escala aritmética)")))
plot(predictorEffects(RTI_uci_ajustada_nb, ~ dosis), main = "UCI", axes=list(grid=FALSE,  y=list(transform=exp, lab="N° de casos (escala aritmética)")))
plot(predictorEffects(RTI_def_ajustada_nb, ~ dosis), main = "Defunciones", axes=list(grid=FALSE,  y=list(transform=exp, lab="N° de casos (escala aritmética)")))

# Modelos bisemanales
library(dplyr)
ejemplo$bisemanal <- NA
ejemplo <- mutate(ejemplo, bisemanal = ifelse(semana %in% 31:32, "31-32",
                                  ifelse(semana %in% 33:34, "33-34",
                                         ifelse(semana %in% 35:36, "35-36",
                                                ifelse(semana %in% 37:38, "37-38",
                                                       ifelse(semana %in% 39:40, "39-40", 
                                                              ifelse(semana %in% 41:42, "41-42", NA)))))))
ejemplo$bisemanal <- as.factor(ejemplo$bisemanal)
ejemplo %>% 
  group_by(dosis, desenlace, bisemanal, edad) %>% 
  summarise(freq = sum(freq), poblacion = sum(poblacion))

ejemplo_desenlace <- split(ejemplo, ejemplo$desenlace)

# Modelo de binomial negativo para caso (bisemanal)
library(MASS)
RTI_caso_ajustada_nb_bisem <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad + bisemanal, data = ejemplo_desenlace[["caso"]])
summary(RTI_caso_ajustada_nb_bisem)
VE_caso_nb_2_bisem <- (1-exp(as.numeric(RTI_caso_ajustada_nb_bisem[["coefficients"]][["dosis2"]])))
VE_caso_nb_3_bisem <- (1-exp(as.numeric(RTI_caso_ajustada_nb_bisem[["coefficients"]][["dosis3"]])))
VE_caso_ic_nb_bisem <- 1-exp(confint(RTI_caso_ajustada_nb_bisem))

# Modelo binomial negativo para uci (bisemanal)
RTI_uci_ajustada_nb_bisem <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad + bisemanal, data = ejemplo_desenlace[["uci"]])
summary(RTI_uci_ajustada_nb_bisem)
VE_uci_nb_2_bisem <- (1-exp(as.numeric(RTI_uci_ajustada_nb_bisem[["coefficients"]][["dosis2"]])))
VE_uci_nb_3_bisem <- (1-exp(as.numeric(RTI_uci_ajustada_nb_bisem[["coefficients"]][["dosis3"]])))
VE_uci_ic_nb_bisem <- 1-exp(confint(RTI_uci_ajustada_nb_bisem))

# Modelo binomial negativo para defunciones (bisemanal)
RTI_def_ajustada_nb_bisem <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad + bisemanal, data = ejemplo_desenlace[["def"]])
summary(RTI_def_ajustada_nb_bisem)
VE_def_nb_2_bisem <- (1-exp(as.numeric(RTI_def_ajustada_nb_bisem[["coefficients"]][["dosis2"]])))
VE_def_nb_3_bisem <- (1-exp(as.numeric(RTI_def_ajustada_nb_bisem[["coefficients"]][["dosis3"]])))
VE_def_ic_nb_bisem <- 1-exp(confint(RTI_def_ajustada_nb_bisem))

# Tabla de efectividad (bisemanal)
efectividad_bisem <- data.frame(Esquema = rep(c("Dos dosis", "Tres dosis"), times = 3),
                          Desenlace = as.factor(c("Caso", "Caso", "UCI", "UCI", "Fallecer", "Fallecer")),
                          Efectividad = c(VE_caso_nb_2_bisem, VE_caso_nb_3_bisem, VE_uci_nb_2_bisem, VE_uci_nb_3_bisem, VE_def_nb_2_bisem, VE_def_nb_3_bisem),
                          lb = c(VE_caso_ic_nb_bisem[2,2], VE_caso_ic_nb_bisem[3,2], VE_uci_ic_nb_bisem[2,2], VE_uci_ic_nb_bisem[3,2], VE_def_ic_nb_bisem[2,2], VE_def_ic_nb_bisem[3,2]),
                          ub = c(VE_caso_ic_nb_bisem[2,1], VE_caso_ic_nb_bisem[3,1], VE_uci_ic_nb_bisem[2,1], VE_uci_ic_nb_bisem[3,1], VE_def_ic_nb_bisem[2,1], VE_def_ic_nb_bisem[3,1]))
efectividad_bisem$Desenlace <- factor(efectividad$Desenlace, levels = c("Caso", "UCI", "Fallecer")) 

g.efectividad_bisem <- ggplot(efectividad_bisem, aes(x = Esquema, y=Efectividad)) + 
  geom_errorbar(aes(ymin=lb, ymax=ub, color=Esquema), width=.1) +
  ggtitle ("Efectividad de la vacunación para esquema de dos y tres dosis (bisemanal)") +
  geom_point(aes(fill=Esquema),size=2, shape=23) +
  theme(text=element_text(size=20)) +
  theme(axis.text=element_text(size=20)) +
  theme(axis.title=element_text(size=20)) +
  theme(plot.title=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limit = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, by = 0.2), labels = scales::percent) +
  facet_wrap(~Desenlace)                                    

ggsave(plot = g.efectividad_bisem,
       filename = "/Users/Usuario/Desktop/efectividad ajustada (bisemanal).png",
       device = "png",
       dpi = "retina",
       width = 14, height = 12)

# Predichos para caso

ejemplo_desenlace_caso <- (ejemplo_desenlace$caso[complete.cases(ejemplo_desenlace$caso), ])
predichos_casos <- predict.glm(RTI_caso_ajustada, newdata = ejemplo_desenlace_caso, type = "response", se.fit = TRUE)
ejemplo_desenlace_caso <- cbind(ejemplo_desenlace_caso, predichos_casos)
ejemplo_desenlace_caso$tasa <- (ejemplo_desenlace_caso$freq / ejemplo_desenlace_caso$poblacion)*10^5
ejemplo_desenlace_caso$pred_lb <- ejemplo_desenlace_caso$fit - 1.96*ejemplo_desenlace_caso$se.fit
ejemplo_desenlace_caso$pred_ub <- ejemplo_desenlace_caso$fit + 1.96*ejemplo_desenlace_caso$se.fit
ejemplo_desenlace_caso$tasa_pred <- (ejemplo_desenlace_caso$fit / ejemplo_desenlace_caso$poblacion)*10^5
ejemplo_desenlace_caso$tasa_pred_lb <- (ejemplo_desenlace_caso$pred_lb / ejemplo_desenlace_caso$poblacion)*10^5
ejemplo_desenlace_caso$tasa_pred_ub <- (ejemplo_desenlace_caso$pred_ub / ejemplo_desenlace_caso$poblacion)*10^5

ejemplo_desenlace_caso <- ejemplo_desenlace_caso %>% 
                          group_by(dosis, semana) %>% 
                          summarise(freq = sum(freq), tasa = sum(tasa), tasa_pred = sum(tasa_pred), tasa_pred_lb = sum(tasa_pred_lb), tasa_pred_ub = sum(tasa_pred_ub), pred_lb = sum(pred_lb), pred_ub = sum(pred_ub))

#Ajuste modelo
ggplot(data = ejemplo_desenlace_caso, aes(x = semana, y = freq, group = dosis, color = dosis)) +
  geom_line() + 
  geom_point() +
  ggtitle ("Casos ajustados por edad para cada semana epidemiológica según esquema") +
  ylab ("Número de casos") +
  theme(text=element_text(size=40), axis.text=element_text(size = 40), axis.title=element_text(size = 40), plot.title = element_text(hjust = 0.5, size = 40)) +
  geom_ribbon( aes(ymin = pred_lb, ymax = pred_ub, fill = dosis, color = NULL), alpha = .15) +
  theme_minimal()

#Gráfico tasas ajustadas

g.caso <- ggplot(data = ejemplo_desenlace_caso, aes(x = semana, y = tasa_pred, group = dosis, color = dosis)) +
  geom_line() + 
  geom_point() +
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
predichos_uci <- predict.glm(RTI_uci_ajustada, newdata = ejemplo_desenlace_uci, type = "response", se.fit = TRUE)
ejemplo_desenlace_uci <- cbind(ejemplo_desenlace_uci, predichos_uci)
ejemplo_desenlace_uci$tasa <- (ejemplo_desenlace_uci$freq / ejemplo_desenlace_uci$poblacion)*10^5
ejemplo_desenlace_uci$pred_lb <- ejemplo_desenlace_uci$fit - 1.96*ejemplo_desenlace_uci$se.fit
ejemplo_desenlace_uci$pred_ub <- ejemplo_desenlace_uci$fit + 1.96*ejemplo_desenlace_uci$se.fit
ejemplo_desenlace_uci$tasa_pred <- (ejemplo_desenlace_uci$fit / ejemplo_desenlace_uci$poblacion)*10^5
ejemplo_desenlace_uci$tasa_pred_lb <- (ejemplo_desenlace_uci$pred_lb / ejemplo_desenlace_uci$poblacion)*10^5
ejemplo_desenlace_uci$tasa_pred_ub <- (ejemplo_desenlace_uci$pred_ub / ejemplo_desenlace_uci$poblacion)*10^5

ejemplo_desenlace_uci <- ejemplo_desenlace_uci %>% 
  group_by(dosis, semana) %>% 
  summarise(freq = sum(freq), tasa = sum(tasa), tasa_pred = sum(tasa_pred), tasa_pred_lb = sum(tasa_pred_lb), tasa_pred_ub = sum(tasa_pred_ub), pred_lb = sum(pred_lb), pred_ub = sum(pred_ub))

#Ajuste modelo
ggplot(data = ejemplo_desenlace_uci, aes(x = semana, y = freq, group = dosis, color = dosis)) +
  geom_line() + 
  geom_point() +
  ggtitle ("Casos uci ajustados por edad para cada semana epidemiológica según esquema") +
  ylab ("Número de hospitalizaciones UCI") +
  theme(text=element_text(size=40), axis.text=element_text(size = 40), axis.title=element_text(size = 40), plot.title = element_text(hjust = 0.5, size = 40)) +
  geom_ribbon( aes(ymin = pred_lb, ymax = pred_ub, fill = dosis, color = NULL), alpha = .15) +
  theme_minimal()

#Gráfico tasas ajustadas
g.uci <- ggplot(data = ejemplo_desenlace_uci, aes(x = semana, y = tasa_pred, group = dosis, color = dosis)) +
  geom_line() + 
  geom_point() +
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
predichos_def <- predict.glm(RTI_def_ajustada, newdata = ejemplo_desenlace_def, type = "response", se.fit = TRUE)
ejemplo_desenlace_def <- cbind(ejemplo_desenlace_def, predichos_def)
ejemplo_desenlace_def$tasa <- (ejemplo_desenlace_def$freq / ejemplo_desenlace_def$poblacion)*10^5
ejemplo_desenlace_def$pred_lb <- ejemplo_desenlace_def$fit - 1.96*ejemplo_desenlace_def$se.fit
ejemplo_desenlace_def$pred_ub <- ejemplo_desenlace_def$fit + 1.96*ejemplo_desenlace_def$se.fit
ejemplo_desenlace_def$tasa_pred <- (ejemplo_desenlace_def$fit / ejemplo_desenlace_def$poblacion)*10^5
ejemplo_desenlace_def$tasa_pred_lb <- (ejemplo_desenlace_def$pred_lb / ejemplo_desenlace_def$poblacion)*10^5
ejemplo_desenlace_def$tasa_pred_ub <- (ejemplo_desenlace_def$pred_ub / ejemplo_desenlace_def$poblacion)*10^5

ejemplo_desenlace_def <- ejemplo_desenlace_def %>% 
  group_by(dosis, semana) %>% 
  summarise(freq = sum(freq), tasa = sum(tasa), tasa_pred = sum(tasa_pred), tasa_pred_lb = sum(tasa_pred_lb), tasa_pred_ub = sum(tasa_pred_ub), pred_lb = sum(pred_lb), pred_ub = sum(pred_ub))

#Ajuste modelo
ggplot(data = ejemplo_desenlace_def, aes(x = semana, y = freq, group = dosis, color = dosis)) +
  geom_line() + 
  geom_point() +
  ggtitle ("Defunciones ajustadas por edad para cada semana epidemiológica según esquema") +
  ylab ("Número de defunciones") +
  theme(text=element_text(size=40), axis.text=element_text(size = 40), axis.title=element_text(size = 40), plot.title = element_text(hjust = 0.5, size = 40)) +
  geom_ribbon( aes(ymin = pred_lb, ymax = pred_ub, fill = dosis, color = NULL), alpha = .15) +
  theme_minimal()

#Gráfico tasas ajustadas
g.def <- ggplot(data = ejemplo_desenlace_def, aes(x = semana, y = tasa_pred, group = dosis, color = dosis)) +
  geom_line() + 
  geom_point() +
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

# Comparación de escenario con vacuna y sin vacuna para casos

library(tidyr)

ejemplo_desenlace_caso <- (ejemplo_desenlace$caso[complete.cases(ejemplo_desenlace$caso), ])
predichos_casos <- predict.glm(RTI_caso_ajustada, newdata = ejemplo_desenlace_caso, type = "response", se.fit = TRUE)
predichos_casos$residual.scale <- NULL
ejemplo_desenlace_caso <- ejemplo_desenlace_caso %>% mutate(dosis = "0")
predichos_casos_0 <- predict.glm(RTI_caso_ajustada, newdata = ejemplo_desenlace_caso, type = "response", se.fit = TRUE)
predichos_casos_0$residual.scale <- NULL
predichos_casos_0$fit0 <- predichos_casos_0$fit
predichos_casos_0$se.fit0 <- predichos_casos_0$se.fit
predichos_casos_0$fit <- NULL
predichos_casos_0$se.fit <- NULL
ejemplo_desenlace_caso <- cbind(ejemplo_desenlace_caso, predichos_casos)
ejemplo_desenlace_caso <- cbind(ejemplo_desenlace_caso, predichos_casos_0)
ejemplo_desenlace_caso$residual.scale <- NULL
ejemplo_desenlace_caso$pred_lb <- ejemplo_desenlace_caso$fit - 1.96*ejemplo_desenlace_caso$se.fit
ejemplo_desenlace_caso$pred_ub <- ejemplo_desenlace_caso$fit + 1.96*ejemplo_desenlace_caso$se.fit
ejemplo_desenlace_caso$pred_lb0 <- ejemplo_desenlace_caso$fit0 - 1.96*ejemplo_desenlace_caso$se.fit0
ejemplo_desenlace_caso$pred_ub0 <- ejemplo_desenlace_caso$fit0 + 1.96*ejemplo_desenlace_caso$se.fit0

ejemplo_desenlace_caso <- ejemplo_desenlace_caso %>% 
  group_by(semana) %>% 
  summarise(freq = sum(freq), fit = sum(fit), pred_lb = sum(pred_lb), pred_ub = sum(pred_ub), fit0 = sum(fit0), pred_lb0 = sum(pred_lb0), pred_ub0 = sum(pred_ub0))

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
predichos_uci <- predict.glm(RTI_uci_ajustada, newdata = ejemplo_desenlace_uci, type = "response", se.fit = TRUE)
predichos_uci$residual.scale <- NULL
ejemplo_desenlace_uci <- ejemplo_desenlace_uci %>% mutate(dosis = "0")
predichos_uci_0 <- predict.glm(RTI_uci_ajustada, newdata = ejemplo_desenlace_uci, type = "response", se.fit = TRUE)
predichos_uci_0$residual.scale <- NULL
predichos_uci_0$fit0 <- predichos_uci_0$fit
predichos_uci_0$se.fit0 <- predichos_uci_0$se.fit
predichos_uci_0$fit <- NULL
predichos_uci_0$se.fit <- NULL
ejemplo_desenlace_uci <- cbind(ejemplo_desenlace_uci, predichos_uci)
ejemplo_desenlace_uci <- cbind(ejemplo_desenlace_uci, predichos_uci_0)
ejemplo_desenlace_uci$residual.scale <- NULL
ejemplo_desenlace_uci$pred_lb <- ejemplo_desenlace_uci$fit - 1.96*ejemplo_desenlace_uci$se.fit
ejemplo_desenlace_uci$pred_ub <- ejemplo_desenlace_uci$fit + 1.96*ejemplo_desenlace_uci$se.fit
ejemplo_desenlace_uci$pred_lb0 <- ejemplo_desenlace_uci$fit0 - 1.96*ejemplo_desenlace_uci$se.fit0
ejemplo_desenlace_uci$pred_ub0 <- ejemplo_desenlace_uci$fit0 + 1.96*ejemplo_desenlace_uci$se.fit0

ejemplo_desenlace_uci <- ejemplo_desenlace_uci %>% 
  group_by(semana) %>% 
  summarise(freq = sum(freq), fit = sum(fit), pred_lb = sum(pred_lb), pred_ub = sum(pred_ub), fit0 = sum(fit0), pred_lb0 = sum(pred_lb0), pred_ub0 = sum(pred_ub0))

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
predichos_def <- predict.glm(RTI_def_ajustada, newdata = ejemplo_desenlace_def, type = "response", se.fit = TRUE)
predichos_def$residual.scale <- NULL
ejemplo_desenlace_def <- ejemplo_desenlace_def %>% mutate(dosis = "0")
predichos_def_0 <- predict.glm(RTI_def_ajustada, newdata = ejemplo_desenlace_def, type = "response", se.fit = TRUE)
predichos_def_0$residual.scale <- NULL
predichos_def_0$fit0 <- predichos_def_0$fit
predichos_def_0$se.fit0 <- predichos_def_0$se.fit
predichos_def_0$fit <- NULL
predichos_def_0$se.fit <- NULL
ejemplo_desenlace_def <- cbind(ejemplo_desenlace_def, predichos_def)
ejemplo_desenlace_def <- cbind(ejemplo_desenlace_def, predichos_def_0)
ejemplo_desenlace_def$residual.scale <- NULL
ejemplo_desenlace_def$pred_lb <- ejemplo_desenlace_def$fit - 1.96*ejemplo_desenlace_def$se.fit
ejemplo_desenlace_def$pred_ub <- ejemplo_desenlace_def$fit + 1.96*ejemplo_desenlace_def$se.fit
ejemplo_desenlace_def$pred_lb0 <- ejemplo_desenlace_def$fit0 - 1.96*ejemplo_desenlace_def$se.fit0
ejemplo_desenlace_def$pred_ub0 <- ejemplo_desenlace_def$fit0 + 1.96*ejemplo_desenlace_def$se.fit0

ejemplo_desenlace_def <- ejemplo_desenlace_def %>% 
  group_by(semana) %>% 
  summarise(freq = sum(freq), fit = sum(fit), pred_lb = sum(pred_lb), pred_ub = sum(pred_ub), fit0 = sum(fit0), pred_lb0 = sum(pred_lb0), pred_ub0 = sum(pred_ub0))

ggplot(data = ejemplo_desenlace_def) +
  geom_line(aes(x = semana, y = fit, group = 1, color = "red")) +
  geom_line(aes(x = semana, y = fit0, group = 1, color = "blue")) +
  geom_ribbon(aes(x = semana, ymin = pred_lb, ymax = pred_ub, group = 1, color = "red"), alpha = .15) +
  geom_ribbon(aes(x = semana, ymin = pred_lb0, ymax = pred_ub0, group = 1, color = "blue"), alpha = .15) +
  ggtitle("N° de defunciones semanales en escenario con vacunación (azul) y sin vacunación (rojo)") +
  ylab("N° de defunciones") +
  theme_minimal()
