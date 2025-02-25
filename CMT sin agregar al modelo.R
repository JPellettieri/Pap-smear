setwd("F:/Juli/Documents/Bioestadistica")

library(pastecs)

datos <- read.delim("Datos_crudos.txt", header = T, sep = "|")
View(datos)
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
nueva <- subset(datos, bhch03!=1 & rango_edad!=1)
tapply(nueva$rango_edad, nueva$bhch03, summary) # Solo quedaron los 2 en bhch03, y rango_edad va de 2 a 5! Todo ok
head(nueva)

nueva <- subset(nueva, select = -c (submuestra, bhch03_j, bhch04_j, rango_edad_j, bhch05_j, nivel_instruccion_j, nivel_instruccion_agrupado_j, bhch10_01_j, 
                                   bhch10_02_j, bhch10_03_j, bhch10_04_j, bhch10_05_j, bhch10_06_j, bhch10_99_j, cobertura_salud_j, bhsl01, bhsl02, bhsl03,
                                   bhsl04, bhsl05, bhsl06, condicion_actividad_j))
# Quitando columnas
nueva <- subset(nueva, select =-c(bisg01, bisg02, bisg03, bisg04, bisg05, bisg06, biaf01, biaf02_m, biaf02_99, biaf03, biaf04_m, biaf04_99, biaf05,
                                  biaf06_m, biaf06_99, biaf07_m, biaf07_99, biaf08, biaf09, biaf10_01, biaf10_02, biaf10_03, biaf10_04,
                                  nivel_actividad_fisica, barreras_actividad_fisica, bita01, bita02, 
                                  bita02_99, bita03, bita04, bita04_01, bita04_02, bita05, bita06_a, bita06_b, bita06_b_99, bita06_c, bita06_d, bita07,
                                  bita07_99, bita08, bita09_01, bita09_02, bita09_03, bita09_04, bita09_05, bita09_06, bita10_01, bita10_02, bita10_03,
                                  bita10_04, bita10_05, bita10_06,bita11, bita12, bita13, bita14, bita15, bita16, consumo_tabaco_100, ta_paquete_y_armado,
                                  ta_dejar_fumar, ta_otros_productos, hta_nofumadores, ta_perc_publicidad, ta_percepcion_riesgo, imagenes_tabaco, biha01,
                                  biha02, biha03, biha04, biha05_01, biha05_02, biha06, biha06_99, biha07, biha08, biha09, biha10, biha11, biha11_99, biha12,
                                  biha13, biha14, biha15, control_hipertension, prevalencia_hipertension, bipc01, bipc02, bipc03, bipc04, bipc04_99, bipc05,
                                  bipc05_99,   imc, imc_categorias, bial01, bial02, bial03, bial03_99, bial04, bial04_99, bial05, bial05_99, bial06,
                                  bial06_99, bial07, bial08, bial09, bial10, promedio_fv_diario, consumo_fv, barreras_fyv, tipo_dieta_razones, bico01,
                                  bico02, bico03, bico04, bico05_01, bico05_02, control_colesterol, prevalencia_colesterol, bica01, bica02, bica03_01,
                                  bica03_02, bica03_99, bica04_01_b, bica04_01_c, bica04_02_b, bica04_02_c, bica04_03_b, bica04_03_c, bica04_04, bica05_01_b,
                                  bica05_01_c, bica05_02_b, bica05_02_c, bica05_03_b, bica05_03_c, bica05_04, bica06, bica07, consumo_regular_riesgo,
                                  consumo_episodico_excesivo, bidi01, bidi02, bidi03, bidi04_01, bidi04_02, bidi04_03, bidi05, bidi06_01, bidi06_02, bidi07,
                                  bidi08, bidi09, bidi10, bidi11, bidi12, bidi13, bidi14, control_diabetes, prevalencia_diabetes, bile01, bile02, bile03,
                                  bicc01_01, bicc01_02, bicc01_03, bicc02, bicc03, control_colon, bima01, bima02, bima03, bima04_01_a, bima04_01_b,
                                  bima04_02_a, bima04_02_b, bima04_03_a,bima04_03_b, promedio_sistolica, promedio_diastolica, ta_elevada,
                                  prevalencia_hipertension_combinada, bima06, bima07, bima09, bima10, imc_bima, imc_categorias_bima, bima12, bima13, bima14,
                                  bimq01, bimq05, bimq05_01, glucemia_elevada, prevalencia_glucemia_elevada_combinada, findrisc, bimq06, bimq06_01,
                                  colesterol_elevado, prevalencia_colesterol_combinada))
head(nueva)
####Definimos variables####
# VR: control_pap, realizacion de pap en los ultimos 2 años. Exito, si (1); fracaso, no (2)
# V: region - cobertura_salud - nivel_educativo - NBI - edad
"""
NBI: Si cumple con: No tiene inodoro, hacinamiento (mas de 3 personas por cuarto), jefe con primario incompleto, vivienda inconveniente (pieza de inquilinato, vivienda precaria, material de la vivienda y propiedad de la misma), gas (kerosene, leña, carbon)

https://ri.conicet.gov.ar/bitstream/handle/11336/55046/CONICET_Digital_Nro.5c048296-4591-4280-ac5b-04f2fb59c898_H.pdf?sequence=5&isAllowed=y
"""

# Elimino usuarios que hayan respondido 99 (son los NS/NC en bipp03 y bipp04 alguna vez se hizo un pap?/cuando fue el ultimo?)
nueva <- nueva[(nueva$control_pap < 3),]
print (nueva$control_pap)

# remplazo 2 por 0 para facilitar calculos
nueva$control_pap<-replace(nueva$control_pap,nueva$control_pap==2 ,0)

prueba1 <- subset(nueva, select = c(rango_edad, quintil_uc, nivel_instruccion, cobertura_salud, region, control_pap))
prueba1$control_pap <- as.factor(prueba1$control_pap)

####Agrupamiento nivel aducativo 
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
#### 

##### Definicion CMT y definicion de la categoria #### 
# 1 es carencia
CMT_maker <- function(df,a,b,c,d,e,f,g,h){
  
  NBI_1 <- df[[a]] #bhcv01:Tipo de vivienda (1. Casa  2. Casilla 3. Depertamento  4. Pieza de inquilinato 5. Pieza en hotel o pensi�n 6. Local no construido para habitaci�n+B33 7. Otros
  NBI_1[NBI_1 == 1 | NBI_1 == 3] <- 0 # casa o departamento
  NBI_1[!(NBI_1 == 0)] <- 1 # todo el resto se considera carente
  
  
  NBI_2 <- df[[b]] #bhcv10:
  NBI_2[is.na(NBI_2)] <- 99 # supongo que se refiere a q no tiene ba�o
  NBI_2[NBI_2 == 1] <- 0
  NBI_2[!(NBI_2 == 0)] <- 1 
  
  
  miembros <- df[[c]] #cant_componentes
  ambientes <- df[[d]] #bhcv02:cantidad de hambientes
  NBI_3 <- miembros/ambientes
  NBI_3 [NBI_3 < 3] <- 0
  NBI_3 [NBI_3 >= 3] <- 1
  
  
  NBI_4 <- df[[e]] # bhcv08: Agua 
  NBI_4[NBI_4 == 1] <- 0 #ca�eria dentro de la vivienda
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
nueva$CMT <- CMT_maker(nueva, 'bhcv01', 'bhcv10', 'cant_componentes', 'bhcv02', 'bhcv08', 'bhcv03', 'bhcv05', 'bhcv06' )
table(nueva$CMT) #   0= 6921 / 1= 7727 no es demasiada carencia?-> cambie condiciones de gas y puse garrafa como no carente ahora da : 10267 no carentes y  4381 carentes que me gusta un poco mas.
prueba1$CMT <-nueva$CMT
table(prueba1$CMT)
##### 
print (prueba1)

freqpap <- table(prueba1$control_pap)/length(prueba1$control_pap)
freqedad <- table(prueba1$rango_edad,prueba1$control_pap)/length(prueba1$rango_edad)
freqquintil <- table(prueba1$quintil_uc, prueba1$control_pap)/length(prueba1$quintil_uc)
freqinst <- table(prueba1$instr_agrupada, prueba1$control_pap)/length(prueba1$instr_agrupada)
freqcob <- table(prueba1$cobertura_salud, prueba1$control_pap)/length(prueba1$cobertura_salud)
freqCMT <- table(prueba1$CMT, prueba1$control_pap)/length(prueba1$CMT)

# prop.table(freqpap) # alternativa
# barplot(freqpap)
#barplot(freqedad)
spineplot(freqedad, xlab = "Edades", ylab = "PAP", xaxlabels = c("25-34", "35-49", "50-64", "65+"), yaxlabels = c(">2 años", "<2 años"), col = c("orange", "gray"), main = "PAP según el rango de edad")
spineplot(freqcob,xlab="Cobertura", ylab="PAP", xaxlabels = c("Con obra social", "Sólo cobertura pública"), yaxlabels = c(">2 años", "<2 años"), col = c("green", "gray"), main = "PAP según cobertura de salud") # 2 es cob pública
spineplot(freqquintil,xlab="Quintil de ingresos", ylab="PAP", yaxlabels = c(">2 años", "<2 años"), col = c("purple", "gray"), main = "PAP según ingresos")
spineplot(freqinst, xlab="Nivel de instruccion", ylab="PAP", xaxlabels = c("Primario", "Secundario","Universitario"), yaxlabels = c(">2 años", "<2 años"), col = c("violet", "gray"), main = "PAP segun nivel educativo")
spineplot(freqCMT, xlab="Carencia", xaxlabels = c("no carentes", "carentes"), yaxlabels = c(">2 años", "<2 años"), col = c("yellow", "gray"), main = "PAP segun carencia")

prueba1$region <- as.factor(prueba1$region)
prueba1$rango_edad <- as.factor(prueba1$rango_edad)
prueba1$quintil_uc <- as.factor(prueba1$quintil_uc)
prueba1$cobertura_salud <- as.factor(prueba1$cobertura_salud)
prueba1$control_pap <- as.factor(prueba1$control_pap)
prueba1$nivel_instruccion_agrupado <- as.factor(prueba1$nivel_instruccion_agrupado)

m1 <- glm(control_pap ~ rango_edad + quintil_uc + cobertura_salud, data = prueba1, family = binomial)

library(car)
library(DHARMa)
Anova(m1) # En glm era Anova con mayuscula? Todo significativo

m2 <- glm(control_pap ~ rango_edad + quintil_uc*cobertura_salud, data = prueba1, family = binomial)
sim <- simulateResiduals(fittedModel = m2, plot = T)
Anova(m2)

m3 <- glm(control_pap ~ rango_edad*quintil_uc*cobertura_salud, data = prueba1, family = binomial)
Anova(m3)

