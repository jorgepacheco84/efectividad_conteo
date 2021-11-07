# efectividad de la vacunación para modelo binomial negativo(conteo)

En el siguiente repositorio está la estimación de la efectividad de la vacunación en Chile ajustando por edad y semana epidemiológica utilizando un modelo lineal generalizado con distribución binomial negativa.

La información proviene del producto 89 del repositorio publicado por el Ministerio de Salud y Ministerio de Ciencias (https://github.com/MinCiencia/Datos-COVID19/tree/master/output/producto89). Esta base de datos tiene algunas limitaciones: no dispone información según tipo de vacuna ni incorpora otras covariables como el sexo, región u otras que son relevantes para estimar la efectividad de la vacunación.

En el desarrollo del código se: 
(1) estima la efectividad de la vacunación ajustando por edad y semana epidemiológica para los casos, las hospitalizaciones en UCI y defunciones
(2) estima tasas ajustadas por edad para cada desenlace según dosis recibida y semana epidemiológica
(3) elabora un escenario contrafactual (what if) donde ningún grupo habría recibido la vacunación.

Si tienen observaciones o formas de programar más eficiente no duden en hacerlo.

Cuídense y vacúnense.
