setwd("C:/Users/DTI/Downloads")
datos = read.csv(file.choose())
summary(datos)
head(datos)
#--------

#ESTADISTICA DESCRIPTIVA

pdf("grafico de DISPERSION.pdf")
pairs(~duration+age+sex+temp1+wbc1+antib+serv,datos)
dev.off()


### (a) COMPARANDO MODELOS
#MODELO NORMAL
mln=glm(duration~age+sex+temp1+wbc1+antib+serv,family=gaussian(link="identity"),datos)
summary(mln)
coef(mln)

#MODELO GAMMA
mlg=glm(duration~age+sex+temp1+wbc1+antib+serv,family=Gamma(link="log"),datos)
summary(mlg)
coef(mlg)

##PRUEBAS DE VEROSIMILITUD

#Modelo Normal
mlnNULL =glm(duration~ 1,family=gaussian(link="identity"),datos)
anova(mlnNULL,mln, test="Chisq")

#INTERPRETACION:p-valor>0.05 entonces no se rechaza el modelo con ninguna
variable explicativa

#Modelo Gamma
mlgNULL =glm(duration~ 1,family=Gamma(link="log"),datos)
anova(mlgNULL,mlg, test="Chisq")

#INTERPRETACION:p-valor>0.05 entonces no se rechaza el modelo con ninguna
variable explicativa

##INTERVALOS DE CONFIANZA DE LOS COEFICIENTES

#MODELO NORMAL
confint.default(mln)

#MODELO GAMMA
confint.default(mlg)

AIC(mln)
AIC(mlg)
##INTERPRETACION: EN PRIMERA INSTANCIA SELECCIONO EL MODELO GAMA
AL TENER PRESENTE UN VALOR AIC MENOR QUE EL MODELO NORMAL

#-------------
#####(b) SELECCION DE VARIABLES DEL MODELO GAMMA

#METODO 1
library(bestglm)
design<-data.frame(datos$age,datos$sex,datos$temp1,datos$wbc1,datos$antib,
datos$serv,datos$duration)
mlg2=bestglm(design,family = Gamma(link="log"),IC="AIC")

#METODO 2

library(MASS)
mlg2=stepAIC(mlg) 
summary(mlg2)

##CONCLUSIÓN: EL MODELO QUE MEJOR AJUSTA EL RESULTADO ES EL QUE VIENE USANDO
LAS VARIABLES EDAD Y TEMPERATURA AL PRESENTAR UN MENOR AIC QUE LAS DEMÁS

# Algunos métodos disponibles
coef(mlg2)
confint.default(mlg2)

#exp(coef(mlg2)[2])
#exp(coef(mlg2)[3])
# INTERPRETACION DE LOS PARAMETROS: ESTADISTICAMENTE, SI LA EDAD AUMENTA EN UNO,
SE ESPERA QUE LA DURACION AUMENTE EN UN 1% APROXIMADAMENTE.
# SI LA TEMPERATURA AUMENTA EN UNO, SE ESPERA QUE LA DURACION AUMENTE EN UN
36% APROXIMADAMENTE.


#-------------
#####(c) ANÁLISIS DE DIAGNÓSTICO

## Leverage
X = model.matrix(mlg2)
# Matriz con la funciones de varianza
V = fitted(mlg2)
V = diag(V)
# Matriz de pesos
w = mlg2$weights
W = diag(w)

H = solve(t(X)%*%W%*%X)
H = sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h = diag(H)

hatvalues(mlg2)

##RESIDUOS
phi<-mlg2$deviance/mlg2$df.residual
phi

#PEARSON
rp<-resid(mlg2, type="pearson")
rp

#Deviance
rd = resid(mlg2, type= "deviance")
rd

#Residuos Deviance estandarizados
td = rd/sqrt(phi*(1-h))
td

#Residuos studentizados
rstudent(mlg2)

#Influencia
#Distancia de Cook
LD = h*(ts^2)/(1 - h)
cooks.distance(mlg2)

#Gráficos de Diagnóstico
#source.with.encoding('envel.pois.R', encoding='ISO-8859-1')
source('envel.gama.R')
#source.with.encoding('diag.pois.R', encoding='ISO-8859-1')
source('diag.gama.R')

pdf("grafico de diagnostico 1.pdf")
envel.gama(mlg2,conf=0.95)
dev.off()

pdf("grafico de diagnostico 2.pdf")
diag.gama(mlg2)
dev.off()
diag.gama(mlg2,iden=c(4,4,4,4,4,4,4,4))

#SE IDENTIFICAN TRES POSIBLES VALORES INFLUYENTES
out=c(21,6,7)
res1=round(summary(mlg2)$coef,3)

fit1=glm(duration~age+temp1,family=Gamma(link=log),data=datos,subset=-21)
cbind(
  coef(mlg2),
  coef(fit1),
  100*(coef(mlg2)-coef(fit1))/coef(mlg2)
)
fit2=glm(duration~age+temp1,family=Gamma(link=log),data=datos,subset=-6)
cbind(
  coef(mlg2),
  coef(fit2),
  100*(coef(mlg2)-coef(fit2))/coef(mlg2)
)
fit3=glm(duration~age+temp1,family=Gamma(link=log),data=datos,subset=-7)
cbind(
  coef(mlg2),
  coef(fit3),
  100*(coef(mlg2)-coef(fit3))/coef(mlg2)
)

#INTERPRETACIÓN: EL DATO 7 ES UN DATO DE MUCHA INFLUENCIA PUES GENERAN
UN GRAN CAMBIO EN LA ESTIMACION DE LOS PARAMETROS. SE PROSEGUIRÁ A CALCULAR
EL MODELO SIN TALES DATOS.
# EL MODELO QUE OPTO ES EL SIGUIENTE:
# fit3=glm(duration~age+temp1,family=Gamma(link=log),data=datos,subset=-7)
summary(fit3)
coef(fit3)
AIC(fit3)

pdf("grafico de diagnostico 1 MODELO SIN VALOR INFLUYENTE.pdf")
envel.gama(fit3,conf=0.95)
dev.off()