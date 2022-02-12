library(tidyr)
library(ggplot2)
library(rpart)
library(rattle)
library(ggridges)
library(dplyr)
library(forcats)
library(gplots)
library(car)
library(coin)
library(vcd)
library(rpart.plot)
library(caret)
library(e1071)
library(corrplot)
library(psych)
library(ROCR)
library(car)
library(fmsb)
library(randomForest)
link <- "https://raw.github.com/HendrixOB/MachineLearning/main/ConjutodeDatos/Ejemplo_analisis_grupo.csv"
datos <- read.csv(file = link, sep = ",")

##Guardamos como factores
datos$ORIGEN <- as.factor(datos$ORIGEN)
datos$SECTOR <- as.factor(datos$SECTOR)
datos$ENTIDAD <- as.factor(datos$ENTIDAD)
datos$SEXO <- as.factor(datos$SEXO)
datos$TIPACIEN <- as.factor(datos$TIPACIEN)
datos$EVOLUCI <- as.factor(datos$EVOLUCI)
datos$INTUBADO <- as.factor(datos$INTUBADO) ##
datos$DIGCLINE <- as.factor(datos$DIGCLINE)
datos$ESINDIGE <- as.factor(datos$ESINDIGE)
datos$HABLEIND <- as.factor(datos$HABLEIND)

datos$OCUPACIO <- as.factor(datos$OCUPACIO)
datos$SERINGRE <- as.factor(datos$SERINGRE)
datos$DIAGPROB <- as.factor(datos$DIAGPROB)
datos$FIEBRE <- as.factor(datos$FIEBRE)
datos$TOS <- as.factor(datos$TOS)
datos$ODINOGIA <- as.factor(datos$ODINOGIA)
datos$DISNEA <- as.factor(datos$DISNEA)
datos$IRRITABI <- as.factor(datos$IRRITABI)

datos$DIARREA <- as.factor(datos$DIARREA)
datos$DOTORACI <- as.factor(datos$DOTORACI)
datos$CALOFRIOS <- as.factor(datos$CALOFRIOS)
datos$CEFALEA <- as.factor(datos$CEFALEA)
datos$MIALGIAS <- as.factor(datos$MIALGIAS)
datos$ARTRAL <- as.factor(datos$ARTRAL)
datos$ATAEDOGE <- as.factor(datos$ATAEDOGE)
datos$RINORREA <- as.factor(datos$RINORREA)
datos$POLIPNEA <- as.factor(datos$POLIPNEA)
datos$VOMITO <- as.factor(datos$VOMITO)

datos$DOLABDO <- as.factor(datos$DOLABDO)
datos$CONJUN <- as.factor(datos$CONJUN)
datos$CIANOSIS <- as.factor(datos$CIANOSIS)
datos$INISUBIS <- as.factor(datos$INISUBIS)
datos$ANOSMIA <- as.factor(datos$ANOSMIA) ## MUCHOS SE IGNORA
datos$DISGEUSIA <- as.factor(datos$DISGEUSIA) ##MUCHOS SE IGNORA
datos$DIABETES <- as.factor(datos$DIABETES)
datos$EPOC <- as.factor(datos$EPOC)
datos$ASMA <- as.factor(datos$ASMA)
datos$INMUSUPR <- as.factor(datos$INMUSUPR)

datos$HIPERTEN <- as.factor(datos$HIPERTEN)
datos$VIH.SIDA <- as.factor(datos$VIH.SIDA)
datos$OTRACON<- as.factor(datos$OTRACON)
datos$ENFCARDI <- as.factor(datos$ENFCARDI)
datos$OBESIDAD <- as.factor(datos$OBESIDAD)
datos$INSRENCR <- as.factor(datos$INSRENCR)
datos$TABAQUIS <- as.factor(datos$TABAQUIS)
datos$RECTRATA <- as.factor(datos$RECTRATA)
datos$TXCROBIA <- as.factor(datos$TXCROBIA)
datos$TXANTIVI <- as.factor(datos$TXANTIVI)

datos$ANTIVIRA <- as.factor(datos$ANTIVIRA)
datos$TXANTIVI <- as.factor(datos$TXANTIVI)
datos$CONOCASO <- as.factor(datos$CONOCASO) ##Muchos " "
datos$CONTAVES <- as.factor(datos$CONTAVES)
datos$CONCERDO <- as.factor(datos$CONCERDO)
datos$CONANIMA <- as.factor(datos$CONANIMA)
datos$VACUNADO <- as.factor(datos$VACUNADO)
datos$RESDEFIN <- as.factor(datos$RESDEFIN)
datos$PUERPERIO <- as.factor(datos$PUERPERIO)
datos$ANTIVIRA <- as.factor(datos$ANTIVIRA)

datos$UCI <- as.factor(datos$UCI) #Muchos ""

## FechaS NULL
datos$FECHREG <- NULL
datos$FECDEF <- NULL
datos$FECINGRE <- NULL
datos$FECINISI <- NULL
datos$FECINITXANTIVI <- NULL
datos$FECVAEST <- NULL
datos$TOMMUE <- NULL
datos$NO <- NULL
datos$SI <- NULL
datos$DIASPUERP <- NULL

datos <- na.omit(datos)

## Reducir categorias
datos <- datos %>%
  mutate(SECTOR = fct_recode(SECTOR,   "OTROS"  = "DIF",
                                        "OTROS" = "ESTATAL",
                                        "OTROS" = "MUNICIPAL",
                                        "OTROS" = "UNIVERSITARIO",
                                        "IMSS" = "IMSS-OPORTUNIDADES",
                                        "ESPECIALES" = "PEMEX",
                                        "ESPECIALES" = "SEDENA",
                                        "ESPECIALES" = "SSA",
                                        "ESPECIALES" = "SEMAR"
                             ))

datos <- datos %>%
  mutate(FIEBRE = fct_recode(FIEBRE, "NO" = "NO",
                                      "NO" = "SE IGNORA",
                                      "SI" = "SI"))

datos <- datos %>%
  mutate(TOS = fct_recode(TOS, "NO" = "NO",
                             "NO" = "SE IGNORA",
                             "SI" = "SI"))

datos <- datos %>%
  mutate(ODINOGIA = fct_recode(ODINOGIA, "NO" = "NO",
                             "NO" = "SE IGNORA",
                             "SI" = "SI"))

datos <- datos %>%
  mutate(DISNEA = fct_recode(DISNEA, "NO" = "NO",
                             "NO" = "SE IGNORA",
                             "SI" = "SI"))

datos <- datos %>%
  mutate(IRRITABI = fct_recode(IRRITABI, "NO" = "NO",
                             "NO" = "SE IGNORA",
                             "SI" = "SI"))
datos <- datos %>%
  mutate(DIARREA = fct_recode(DIARREA, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))

datos <- datos %>%
  mutate(DOTORACI = fct_recode(DOTORACI, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))
datos <- datos %>%
  mutate(CALOFRIOS = fct_recode(CALOFRIOS, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))
datos <- datos %>%
  mutate(CEFALEA = fct_recode(CEFALEA, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))

datos <- datos %>%
  mutate(MIALGIAS = fct_recode(MIALGIAS, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))
datos <- datos %>%
  mutate(ARTRAL = fct_recode(ARTRAL, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))
datos <- datos %>%
  mutate(ATAEDOGE = fct_recode(ATAEDOGE, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))
datos <- datos %>%
  mutate(RINORREA = fct_recode(RINORREA, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))
datos <- datos %>%
  mutate(POLIPNEA= fct_recode(POLIPNEA, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))
datos <- datos %>%
  mutate(VOMITO = fct_recode(VOMITO, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))
##
datos <- datos %>%
  mutate(DOLABDO = fct_recode(DOLABDO, "NO" = "NO",
                             "NO" = "SE IGNORA",
                             "SI" = "SI"))
datos <- datos %>%
  mutate(CONJUN = fct_recode(CONJUN, "NO" = "NO",
                             "NO" = "SE IGNORA",
                             "SI" = "SI"))

datos <- datos %>%
  mutate(CIANOSIS = fct_recode(CIANOSIS, "NO" = "NO",
                             "NO" = "SE IGNORA",
                             "SI" = "SI"))

datos <- datos %>%
  mutate(INISUBIS = fct_recode(INISUBIS, "NO" = "NO",
                             "NO" = "SE IGNORA",
                             "SI" = "SI"))

datos <- datos %>%
  mutate(DIABETES = fct_recode(DIABETES, "NO" = "NO",
                             "NO" = "SE IGNORA",
                             "SI" = "SI"))

datos <- datos %>%
  mutate(EPOC = fct_recode(EPOC, "NO" = "NO",
                             "NO" = "SE IGNORA",
                             "SI" = "SI"))

datos <- datos %>%
  mutate(ASMA = fct_recode(ASMA, "NO" = "NO",
                             "NO" = "SE IGNORA",
                             "SI" = "SI"))
datos <- datos %>%
  mutate(INMUSUPR = fct_recode(INMUSUPR, "NO" = "NO",
                             "NO" = "SE IGNORA",
                             "SI" = "SI"))
##
datos <- datos %>%
  mutate(HIPERTEN = fct_recode(HIPERTEN, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))
datos <- datos %>%
  mutate(VIH.SIDA = fct_recode(VIH.SIDA, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))
datos <- datos %>%
  mutate(OTRACON = fct_recode(OTRACON, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))
datos <- datos %>%
  mutate(ENFCARDI = fct_recode(ENFCARDI, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))
datos <- datos %>%
  mutate(OBESIDAD = fct_recode(OBESIDAD, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))
datos <- datos %>%
  mutate(INSRENCR = fct_recode(INSRENCR, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))
datos <- datos %>%
  mutate(TABAQUIS = fct_recode(TABAQUIS, "NO" = "NO",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))

##
datos <- datos %>%
  mutate(ANTIVIRA = ifelse(ANTIVIRA == "OSELTAMIVIR",1,0))

datos <- datos %>%
  mutate(CONTAVES = fct_recode(CONTAVES, "NO" = "NO",
                          "NO" = "",
                          "SI" = "SI"))
datos <- datos %>%
  mutate(CONANIMA = fct_recode(CONANIMA, "NO" = "", "SI" = "04 PERROS", "SI" = "1 CANINO",
                               "SI" = "1 GATO", "SI" = "1 PERRO", "SI" = "2 CANINOS",
                               "SI" = "2 GATOS", "SI" = "2 PERICOS + 4 GATOS",
                               "SI" = "2 PERROS", "SI" = "BORREGO", "SI" = "BORREGOS",
                               "SI" = "CABALLOS", "SI" = "CABALLOS Y BURROS",
                               "SI" = "CANINO", "SI" = "CANINO Y GATO", "SI" = "CHIVAS",
                               "SI" = "CONEJO", "SI" = "CUYO", "SI" = "CUYO Y PERRO",
                               "SI" = "GALLINAS Y CONEJOS", "SI" = "GALLOS, PERROS",
                               "SI" = "GATO", "SI" = "GATOS", "SI" = "GATOS Y PERROS",
                               "NO" = "N", "NO" = "N0", "NO" = "NEGADO", "NO" = "NEGADOS",
                               "NO" = "NINGUNO", "NO" = "NINGUNA", "NO" = "NINGUNO",
                               "NO" = "NINUGNO", "NO" = "NNINGUNO", "NO" = "NNO",
                               "NO" = "NIGUNO", "NO" = "NON", "SI" = "PEEROS", "SI" = "PERO",
                               "SI" = "PERO Y GATO", "SI" = "PERR", "SI" = "PERRO",
                               "SI" = "PERRO DOMESTICO", "SI" = "PERRO GATO", 
                               "SI" = "PERRO TORTUGA", "SI" = "PERRO VACA", "SI" = "PERRO Y GATO",
                               "SI" = "PERRO Y GATOS", "SI" = "PERRO Y TORTUGAS",
                               "SI" = "PERRO, GATO", "SI" = "PERRO,GATO", "SI" = "PERROGATO",
                               "SI" = "PERROS", "SI" = "PERROS-GATOS", "SI" = "PERROS GATOS",
                               "SI" = "PERROS Y GATO", "SI" = "PERROS Y GATOS",
                               "SI" = "PERROS, GATO", "SI" = "PERRS Y GATOS", "SI" = "POLLO",
                               "NO" = "SE IGNORA", "SI" = "SI (PERRO)", 
                               "SI" = "SI, PERRO DOMESTICO", "SI" = "SI,PERRO",
                               "SI" = "TORTUGA", "SI" = "VACA", "SI" = "VACAS",
                               "SI" = "VACAS Y PERROS", "SI" = "VACAS, CABALLOS, PERROS",
                               "SI" = "VACAS, PERROS"))

datos <- datos %>%
  mutate(VACUNADO= fct_recode(VACUNADO, "NO" = "NO", "NO" = "",
                               "NO" = "SE IGNORA",
                               "SI" = "SI"))

datos <- datos %>%
  mutate(ANTIPIRETICOS = fct_recode(ANTIPIRETICOS, "NO" = "NO",
                               "NO" = "",
                               "SI" = "SI"))

datos <- datos %>%
  mutate(DIGCLINE = fct_recode(DIGCLINE, "NO" = "NO",
                                    "NO" = "",
                                    "SI" = "SI"))

datos <- datos %>%
  mutate(ESINDIGE = fct_recode(ESINDIGE, "NO" = "NO",
                               "NO" = "",
                               "SI" = "SI"))

datos <- datos %>%
  mutate(HABLEIND = fct_recode(HABLEIND, "NO" = "NO",
                               "NO" = "",
                               "SI" = "SI"))

datos <- datos %>%
  mutate(RECTRATA = fct_recode(RECTRATA, "NO" = "NO",
                               "NO" = "",
                               "SI" = "SI"))

datos <- datos %>%
  mutate(OCUPACIO = fct_recode(OCUPACIO, "Otros" = "OTROS", 
                               "Otros" = "OTROS PROFESIONISTAS",
                               "No activos" = "DESEMPLEADOS",
                               "No activos" = "ESTUDIANTES",
                               "No activos" = "HOGAR",
                               "No activos" = "JUBILADO / PENSIONADO",
                               "No activos" = "MAESTROS",
                               "Salud" = "DENTISTAS",
                               "Salud" = "ENFERMERAS",
                               "Salud" = "LABORATORISTAS",
                               "Salud" = "MEDICOS",
                               "Salud" = "OTROS TRABAJADORES DE LA SALUD",
                               "Generales" = "CAMPESINOS",
                               "Generales" = "CHOFERES",
                               "Generales" = "COMERCIANTES DE MERCADOS FIJOS O AMBULANTES",
                               "Generales" = "EMPLEADOS",
                               "Generales" = "GERENTES O PROPIETARIOS DE EMPRESAS O NEGOCIOS",
                               "Generales" = "OBREROS"))

datos <- datos %>%
  mutate(CONCERDO = fct_recode(CONCERDO, "NO" = "NO",
                               "NO" = "",
                               "SI" = "SI"))

summary(datos)


aux <- (datos$RESDEFIN=="NEGATIVO" |datos$RESDEFIN=="SARS-CoV-2")
data <- datos[aux==1,] ##N 2352 & S 1894

summary(data)

table(is.na(data)) #No hay NAs

data <- data %>%
  mutate(RESDEFIN = fct_recode(RESDEFIN, "NEGATIVO" = "NEGATIVO",
                               "SARS-CoV-2" = "SARS_CoV_2",
                               "NEGATIVO" = "",
                               "NEGATIVO" = "B",
                               "NEGATIVO" = "CORONA 229E",
                               "NEGATIVO" = "CORONA NL63",
                               "NEGATIVO" = "ENTEROV//RHINOVIRUS",
                               "NEGATIVO" = "INF A",
                               "NEGATIVO" = "INF AH1N1 PMD",
                               "NEGATIVO" = "NO ADECUADO",
                               "NEGATIVO" = "NO RECIBIDA",
                               "NEGATIVO" = "NO SUBTIPIFICADO",
                               "NEGATIVO" = "RECHAZADA"))



### Clasificador de bayes

# Partimos la muestra
set.seed(1989)  ##Taylor's version

particion <- createDataPartition(y=data$RESDEFIN, p = .66, list= FALSE)
entrenamiento <- data[particion,]
prueba <- data[-particion,]


### Arbol cart retencion
cart01 <- train(RESDEFIN ~ ., data = entrenamiento, method = "rpart",
                  trControl = trainControl(method = "cv", number = 10),
                  tuneLength = 20)
plot(cart01)

cart01$finalModel$tuneValue #CP = 0.002398082

rpart.plot(cart01$finalModel, extra = 2, under = TRUE,  varlen = 0, faclen = 0,
           cex = .6)

varImp(cart01)

dotPlot(varImp(cart01, compete=FALSE))


cart02 <- train(RESDEFIN ~ ., data = entrenamiento, method = "rpart",
                trControl = trainControl(method = "cv", number = 20),
                tuneLength = 10)
plot(cart02)

cart02$finalModel$tuneValue #CP = 0.004796163

rpart.plot(cart02$finalModel, extra = 2, under = TRUE,  varlen = 0, faclen = 0,
           cex = .6)

varImp(cart02)

dotPlot(varImp(cart02, compete=FALSE))

#Matrices de confusion
prediccion01 <- predict(cart01,newdata = prueba)
matconf_tree01 <- confusionMatrix(data = prediccion01,prueba$RESDEFIN)
matconf_tree01

prediccion02 <- predict(cart02,newdata = prueba)
matconf_tree02 <- confusionMatrix(data = prediccion02,prueba$RESDEFIN)
matconf_tree02

##Random forest
entrena <- createFolds(data$RESDEFIN,k=10)

##Bosque aleatorio
arbolFit <- train(RESDEFIN ~ .,  data = data, method = "rpart",
                  tuneLength = 10,
                  trControl = trainControl(
                    method = "cv", indexOut = entrena))

plot(arbolFit)
arbolFit$finalModel$tuneValue #0.003167899

predarbol <- predict(arbolFit, newdata = prueba)
confusionMatrix(predarbol,prueba$RESDEFIN)

dotPlot(varImp(arbolFit, compete=FALSE))

rpart.plot(arbolFit$finalModel, extra = 2, under = TRUE,  varlen = 0, faclen = 0,
           cex = .6)


arbolFit2 <- train(RESDEFIN ~ .,  data = data, method = "rpart",
                  tuneLength = 15,
                  trControl = trainControl(
                    method = "cv", indexOut = entrena))

plot(arbolFit2)
arbolFit2$finalModel$tuneValue #0.002111932
dotPlot(varImp(arbolFit2, compete=FALSE))

predarbol2 <- predict(arbolFit2, newdata = prueba)
confusionMatrix(predarbol2,prueba$RESDEFIN)


rpart.plot(arbolFit2$finalModel, extra = 2, under = TRUE,  varlen = 0, faclen = 0,
           cex = .6)

arbolFit3 <- train(RESDEFIN ~ .,  data = data, method = "rpart",
                   tuneLength = 7,
                   trControl = trainControl(
                     method = "cv", indexOut = entrena))

plot(arbolFit3)
arbolFit3$finalModel$tuneValue #0.005807814
dotPlot(varImp(arbolFit3, compete=FALSE))

predarbol3 <- predict(arbolFit3, newdata = prueba)
confusionMatrix(predarbol3,prueba$RESDEFIN)


rpart.plot(arbolFit2$finalModel, extra = 2, under = TRUE,  varlen = 0, faclen = 0,
           cex = .6)


randomFit <- train(RESDEFIN ~ .,  data = data, method = "rf",
                   tuneLength = 10,
                   trControl = trainControl(
                     method = "cv", indexOut = entrena))

##Modelo final del bosque
randomFit$finalModel$tuneValue # 6  68
randomFit$results
mean(randomFit$results$Accuracy)
randomFit$finalModel

##Importancia de los predictores
importrf <- as.data.frame(varImp(randomFit$finalModel))

importrf %>%
  filter(Overall > 32)

##DIbujamos la importancia
dotplot(randomFit$finalModel$importance)

predrf <- predict(randomFit, newdata = prueba)
confusionMatrix(data = predrf, prueba$RESDEFIN)


#### Bayes
bayes01 <- naiveBayes(RESDEFIN ~., data = entrenamiento)
summary(bayes01)

predbayes <- predict(bayes01, newdata = prueba)

confmat_bayes <- confusionMatrix(data = predbayes,prueba$RESDEFIN)
confmat_bayes

bayes02 <- naiveBayes(RESDEFIN ~., data = entrenamiento, laplace = 10)
summary(bayes02)

predbayes2 <- predict(bayes02, newdata = prueba)

confmat_bayes <- confusionMatrix(data = predbayes2,prueba$RESDEFIN)
confmat_bayes


#### Logistica
reglog01 <- glm(RESDEFIN ~., family = binomial, data = entrenamiento)
summary(reglog01)

step(reglog01, k = 10, direction = "both")

## Call:  glm(formula = RESDEFIN ~ SEXO + DIGCLINE + EDAD + FIEBRE + DISNEA + 
#MIALGIAS + DISGEUSIA + OBESIDAD + TXCROBIA + ANTIPIRETICOS, 
#family = binomial, data = entrenamiento)

reglog02 <- glm(RESDEFIN ~ SEXO + DIGCLINE + EDAD + FIEBRE + DISNEA + 
                         MIALGIAS + DISGEUSIA + OBESIDAD + TXCROBIA + ANTIPIRETICOS, 
                       family = binomial, data = entrenamiento)

confint(reglog02)
vif(reglog02)

predlogist <- predict.glm(reglog02, newdata = prueba, type = "response")
aux <- floor(predlogist+0.5)
conflogist <- table(prueba$RESDEFIN, aux)

##Precision
accurate3 <- (conflogist[1,1] + conflogist[2,2]) / (sum(conflogist))

sum(conflogist) #1442
Qalpha <- qnorm(0.025, mean = 0, sd = 1)

##Sensibilidad
(conflogist[2,2]) / (conflogist[2,2] + conflogist[2,1])

#Especificidad
conflogist[1,1] / (conflogist[1,1] + conflogist[1,2])

inf3 <- accurate3 + Qalpha*(sqrt((accurate3*(1 - accurate3))/1442))
sup3 <- accurate3 - Qalpha*(sqrt((accurate3*(1 - accurate3))/1442))

c(inf3,sup3) #intervalo de confianza

#### Support vector machines
svm1_cv <- tune("svm", RESDEFIN ~., data = entrenamiento,
               kernel = 'linear',
               ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 20, 50, 100,
                                      150, 200)))

summary(svm1_cv)

svm1_cv$best.model #costo = 0.1

sv1 <- svm(RESDEFIN ~ ., data = entrenamiento, kernel = "linear", cost = 0.1)
summary(sv1)

predsv1 <- predict(sv1, newdata = prueba)

confmat_sv1 <- confusionMatrix(data = predsv1,prueba$RESDEFIN)
confmat_sv1

svm2_cv <- tune("svm", RESDEFIN ~., data = entrenamiento,
                kernel = 'radial',
                ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 20),
                              gamma = c(0.0005, 0.005, 0.05, 1, 2, 5)))

summary(svm2_cv)

svm2_cv$best.parameters #costo 1 gamma 0.05


sv2 <- svm(RESDEFIN ~ ., data = entrenamiento, kernel = "radial", cost = 1,
           gamma = 0.05)
summary(sv2)

predsv2 <- predict(sv2, newdata = prueba)

confmat_sv2 <- confusionMatrix(data = predsv2,prueba$RESDEFIN)
confmat_sv2
