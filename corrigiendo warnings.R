setwd("F:/Juli/Documents/Bioestadistica")
datos <- read.delim("DatosPAP.txt", header = T, sep = "|")



#install.packages("pROC")
#install.packages("car")
#install.packages("DHARMa")
#install.packages("lme4")
#install.packages("TMB")
#install.packages("ggplot2")
#install.packages("emmeans")
#install.packages("ggeffects")

library(pastecs)
library(car)
library(DHARMa)
library(lme4)
library(glmmTMB)
library(pROC)
library(ggplot2)
library(emmeans)
library(ggeffects)


datos <- read.delim("Datos_crudos.txt", header = T, sep = "|")
head(datos)
# bhch03_j: Genero de jefx hogar: 1) varon, 2) mujer
# Hay una categoria para el respondente, que no necesariamente es jefx. Muy segura que le respondente es quien responde lo de las practicas preventivas.
# Que son los "factores de expansion"? wf1p. wf2p y wf3p, ultimas 3 columnas.
# Mencion de los pasos 2 y 3: Son si le respondente estuvo a favor de que le tomen medidas antropometricas y bioquimicas, ultimos apartados.

'''
Recomendaciones del ministerio de salud respecto al PAP y mamografias
PAP) Se recomienda que se realicen este estudio las mujeres a partir de los 25 años.
https://www.argentina.gob.ar/salud/cancer/tipos/cancer-de-cuello-de-utero-ccu

MAMO) Se recomienda que todas las mujeres de 50 a 69 años se realicen al menos una mamografía cada dos años
https://www.argentina.gob.ar/salud/cancer/tipos/cancer-de-mama
sda
Indagar mas profundo las recomendaciones, pero con esto creo que es mejor ir a PAP solo, por ahora
'''

###Acotamos datos####
# Hora de quitar individuos:
# bhch03 --> genero del respondente. 1) Varon, 2) Mujer  REMOVER LOS QUE TENGAN 1
# rango_edad --> rango de edad. 1) 18-24, 2) 25-34, etc  REMOVER LOS QUE TENGAN 1 (fuera del rango de recomendacion)
nueva <- subset(datos, bhch03!=1 & rango_edad!=1 & nivel_instruccion!=8) # sacamos educacion especial tmb
# tapply(nueva$rango_edad, nueva$bhch03, summary) 
# Solo quedaron los 2 en bhch03, y rango_edad va de 2 a 5! Todo ok
# table(nueva$bhch03)

# Elimino usuarios que hayan respondido 99 (son los NS/NC en bipp03 y bipp04 alguna vez se hizo un pap?/cuando fue el ultimo?)
nueva <- nueva[(nueva$control_pap < 3),]
# remplazo 2 por 0 para facilitar calculos
nueva$control_pap<-replace(nueva$control_pap,nueva$control_pap==2 ,0)


prueba1 <- subset(nueva, select = c(cod_provincia, rango_edad, quintil_uc, nivel_instruccion, cobertura_salud, region, control_pap))
# head(prueba1)
# table(prueba1$control_pap)
#####

"""
Definimos variables
# VR: control_pap, realizacion de pap en los ultimos 2 aÃ±os. Exito, si (1); fracaso, no (2)
# V: provincia - cobertura_salud - nivel_educativo - carencia - edad

NBI: Si cumple con: No tiene inodoro, hacinamiento (mas de 3 personas por cuarto), jefe con primario incompleto, vivienda inconveniente (pieza de inquilinato, vivienda precaria, material de la vivienda y propiedad de la misma), gas (kerosene, leÃ±a, carbon)

https://ri.conicet.gov.ar/bitstream/handle/11336/55046/CONICET_Digital_Nro.5c048296-4591-4280-ac5b-04f2fb59c898_H.pdf?sequence=5&isAllowed=y
"""

##### Agrupamiento nivel aducativo #####
agrupo_nivel <- function (x){
  nivel_nuevo <- c()
  for (i in x)
  {
    if (i == 1 || i == 2 || i == 3){
      nivel_nuevo <- c(nivel_nuevo, 1) # nivel eductativo hasta primario completo
    }
    else if (i == 4 || i == 5){
      nivel_nuevo <- c(nivel_nuevo, 2) # hasta secundario completo
    }
    else if (i == 6 || i == 7){
      nivel_nuevo <- c(nivel_nuevo, 3) # hasta 3rio/uni completo
    }
    else if (i == 8){
      nivel_nuevo <- c(nivel_nuevo, 99) # educacion especial
    }
  }
  return(nivel_nuevo) }

instr_agrupada <- agrupo_nivel(nueva$nivel_instruccion)
prueba1 <- cbind(prueba1, instr_agrupada = instr_agrupada)
prueba1 <- subset(prueba1, instr_agrupada!=99)
# table(prueba1$instr_agrupada)


##### Definicion CMT y definicion de la categoria #### 
# 1 es carencia
CMT_maker <- function(df, vivienda, b, c, d, e, f, g, h){
  
  NBI_1 <- df[[vivienda]] #bhcv01:Tipo de vivienda (1. Casa  2. Casilla 3. Depertamento  4. Pieza de inquilinato 5. Pieza en hotel o pensión 6. Local no construido para habitación+B33 7. Otros
  NBI_1[NBI_1 == 1 | NBI_1 == 3] <- 0 # casa o departamento
  NBI_1[!(NBI_1 == 0)] <- 1 # todo el resto se considera carente
  
  
  NBI_2 <- df[[b]] #bhcv10 (baño):
  NBI_2[is.na(NBI_2)] <- 99 # supongo que se refiere a q no tiene baño
  NBI_2[NBI_2 == 1] <- 0
  NBI_2[!(NBI_2 == 0)] <- 1 
  
  
  miembros <- df[[c]] #cant_componentes
  ambientes <- df[[d]] #bhcv02:cantidad de hambientes
  NBI_3 <- miembros/ambientes
  NBI_3 [NBI_3 < 3] <- 0
  NBI_3 [NBI_3 >= 3] <- 1
  
  
  NBI_4 <- df[[e]] # bhcv08: Agua 
  NBI_4[NBI_4 == 1] <- 0 #cañeria dentro de la vivienda
  NBI_4[!(NBI_4 == 0)] <- 1
  
  
  NBI_5 <- df[[f]] # bhcv03:Material del piso
  NBI_5[NBI_5 == 1] <- 0
  NBI_5[!(NBI_5 == 0)] <- 1 #Cemento o ladrillo fijo/ Tierra o ladrillo suelto/ Otros
  
  
  NBI_6 <- df[[g]] # bhcv05:Techo con cielorraso/revestimiento?
  NBI_6[NBI_6 == 1] <- 0 #si
  NBI_6[NBI_6 == 2] <- 1 #no
  NBI_6[NBI_6 == 9] <- NA #ns/nc
  
  #Agrego categoria GAS
  NBI_7 <- df[[h]] #bhcv06: para cocinar usa
  NBI_7[NBI_7 == 1 | NBI_7 == 2] <- 0 # gas de red o garrafa
  NBI_7[!(NBI_7 == 0)] <- 1 # todo el resto se considera carente
  
  CMT <- NBI_1 + NBI_2 + NBI_3 + NBI_4 + NBI_5 + NBI_6 + NBI_7
  CMT[!(CMT == 0)] <- 1
  CMT
}
# creo categoria llamada CMT en prueba1 
CMT <- CMT_maker(nueva, 'bhcv01', 'bhcv10', 'cant_componentes', 'bhcv02', 'bhcv08', 'bhcv03', 'bhcv05', 'bhcv06' )
prueba1 <- cbind(prueba1, Carencia = CMT)
# table(prueba1$Carencia)
# head(prueba1)
##### 


prueba1$cod_provincia <- as.factor(prueba1$cod_provincia)
prueba1$rango_edad <- as.factor(prueba1$rango_edad)
prueba1$quintil_uc <- as.factor(prueba1$quintil_uc)
prueba1$cobertura_salud <- as.factor(prueba1$cobertura_salud)
prueba1$control_pap <- as.factor(prueba1$control_pap)
prueba1$instr_agrupada <- as.factor(prueba1$instr_agrupada)
prueba1$Carencia <- as.factor(prueba1$Carencia)
# str(prueba1)


# freqpap <- table(prueba1$control_pap)/length(prueba1$control_pap)
# freqedad <- table(prueba1$rango_edad,prueba1$control_pap)/length(prueba1$rango_edad)
# freqquintil <- table(prueba1$quintil_uc, prueba1$control_pap)/length(prueba1$quintil_uc)
# freqinst <- table(prueba1$instr_agrupada, prueba1$control_pap)/length(prueba1$instr_agrupada)
# freqcob <- table(prueba1$cobertura_salud, prueba1$control_pap)/length(prueba1$cobertura_salud)
# freqCMT <- table(prueba1$Carencia, prueba1$control_pap)/length(prueba1$Carencia)

# prop.table(freqpap) # alternativa
# barplot(freqpap)
#barplot(freqedad)
# spineplot(freqedad, xlab = "Edades", ylab = "PAP", xaxlabels = c("25-34", "35-49", "50-64", "65+"), yaxlabels = c(">2 años", "<2 años"), col = c("orange", "gray"), main = "PAP según el rango de edad")

# spineplot(freqcob,xlab="Cobertura", ylab="PAP", xaxlabels = c("Con obra social", "Sólo cobertura pública"), yaxlabels = c(">2 años", "<2 años"), col = c("green", "gray"), main = "PAP según cobertura de salud") # 2 es cob pública

# spineplot(freqquintil,xlab="Quintil de ingresos", ylab="PAP", yaxlabels = c(">2 años", "<2 años"), col = c("purple", "gray"), main = "PAP según ingresos")

# spineplot(freqinst, xlab="Nivel de instruccion", ylab="PAP", xaxlabels = c("Primario", "Secundario","Universitario"), yaxlabels = c(">2 años", "<2 años"), col = c("violet", "gray"), main = "PAP según nivel educativo")

# spineplot(freqCMT, xlab="Carencia", xaxlabels = c("no carentes", "carentes"), yaxlabels = c(">2 años", "<2 años"), col = c("green", "gray"), main = "PAP segun carencia")


##### MODELOS ####
# Modelo solo con la edad, luego modelo edad + 1 variable (carencia, quintil, salud, instr), todas con provincia de aleatorio
"""
m_e <- glmmTMB(control_pap ~ rango_edad + (1|cod_provincia), data = prueba1, family = binomial)
sim <- simulateResiduals(fittedMod = m_e, plot = T)
Anova(m_e)
summary(m_e)


m_q <- glmmTMB(control_pap ~ rango_edad + quintil_uc + (1|cod_provincia), data = prueba1, family = binomial)
sim <- simulateResiduals(fittedMod = m_q, plot = T)
Anova(m_q)
summary(m_q)


m_cob <- glmmTMB(control_pap ~ rango_edad + cobertura_salud + (1|cod_provincia), data = prueba1, family = binomial)
sim <- simulateResiduals(fittedMod = m_cob, plot = T)
Anova(m_cob)
summary(m_cob)


m_ins <- glmmTMB(control_pap ~ rango_edad + instr_agrupada + (1|cod_provincia), data = prueba1, family = binomial)
sim <- simulateResiduals(fittedMod = m_ins, plot = T)
Anova(m_ins)
summary(m_ins)


m_car <- glmmTMB(control_pap ~ rango_edad + Carencia + (1|cod_provincia), data = prueba1, family = binomial)
sim <- simulateResiduals(fittedMod = m_car, plot = T)
Anova(m_car)
summary(m_car)


# Vamos agrupando edad + 2 variables:
m_qc <- glmmTMB(control_pap ~ rango_edad + quintil_uc + Carencia + (1|cod_provincia), data = prueba1, family = binomial)
sim <- simulateResiduals(fittedMod = m_qc, plot = T)
Anova(m_qc)
summary(m_qc)


m_si <- glmmTMB(control_pap ~ rango_edad + cobertura_salud + instr_agrupada + (1|cod_provincia), data = prueba1, family = binomial)
sim <- simulateResiduals(fittedMod = m_si, plot = T)
Anova(m_si)
summary(m_si)


m_comp_fijo <- glmmTMB(control_pap ~ rango_edad + cobertura_salud + quintil_uc + Carencia + instr_agrupada + cod_provincia, data = prueba1, family = binomial)
"""

m_comp <- glmmTMB(control_pap ~ rango_edad + cobertura_salud + quintil_uc + Carencia + instr_agrupada + (1|cod_provincia), data = prueba1, family = binomial)
sim <- simulateResiduals(fittedMod = m_comp, plot = T)
plotResiduals(sim, prueba1$rango_edad)
plotResiduals(sim, prueba1$cobertura_salud)
plotResiduals(sim, prueba1$quintil_uc)
plotResiduals(sim, prueba1$Carencia)
plotResiduals(sim, prueba1$instr_agrupada)

Anova(m_comp)
summary(m_comp)

alfai<-ranef(m_comp)$cond$cod_provincia # Todo ok! No hay evidencia para rechazar normalidad de aleatorio
qqnorm(alfai$`(Intercept)`, cex.main=.8)
qqline(alfai$`(Intercept)`)
shapiro.test(alfai$`(Intercept)`)


###### RESULTADOS  ####
# escala del PL (ln(odds))
# coef(m_comp)
confint(m_comp)

# escala de ODDS
# exp(coef(m_comp))
exp(confint(m_comp))

comp_e <- emmeans(m_comp, pairwise ~ rango_edad, type = "response") # en escala de la VR
summary(comp_e) # No habria diferencias significativas entre el rango de edad 2 y el 3
plot(comp_e, comparisons = TRUE)

comp_q <- emmeans(m_comp, pairwise ~ quintil_uc, type = "response")
summary(comp_q) # sin diferencias entre consecutivos
plot(comp_q, comparisons = TRUE)

comp_car <- emmeans(m_comp, pairwise ~ Carencia, type = "response")
summary(comp_car)
plot(comp_car, comparisons = TRUE)

comp_ins <- emmeans(m_comp, pairwise ~ instr_agrupada, type = "response")
summary(comp_ins)
plot(comp_ins, comparisons = TRUE)

comp_cob <- emmeans(m_comp, pairwise ~ cobertura_salud, type = "response")
summary(comp_cob)
plot(comp_cob, comparisons = TRUE)


# Curva ROC

# xlab = "Rango de edad", ylab = "Control de PAP bianual", main = "Porcentaje de realización de PAP bianualmente según rango de edad"


#?plot
plot(ggpredict(m_comp, terms = ~rango_edad)) + labs(x = "Rango de edad", y = "Control de PAP bianual") + 
  ggtitle("Realización de PAP bianualmente según el rango de edad")

plot(ggpredict(m_comp, terms = ~cobertura_salud)) + 
  labs(x = "Cobertura de salud", y = "Control de PAP bianual") + 
  ggtitle("Realización de PAP bianualmente según la cobertura de salud")

plot(ggpredict(m_comp, terms = ~instr_agrupada)) + 
  labs(x = "Nivel de instrucción", y = "Control de PAP bianual") + 
  ggtitle("Realización de PAP bianualmente según el nivel de instrucción")

plot(ggpredict(m_comp, terms = ~Carencia)) + 
  labs(x = "Carencia del hogar", y = "Control de PAP bianual") + 
  ggtitle("Realización de PAP bianualmente según la carencia del hogar")

plot(ggpredict(m_comp, terms = ~quintil_uc)) + 
  labs(x = "Quintil de ingresos", y = "Control de PAP bianual") + 
  ggtitle("Realización de PAP bianualmente según el quintil de ingresos")

library(sjPlot) # No quiere cargar
# tab_model(m_comp) # Supuestamente da una tabla linda :(


## intento de hacer graficos de odds ratio
install.packages("OddsPlotty")
install.packages("tidyverse")
install.packages("mlbench")



