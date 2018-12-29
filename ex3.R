require(MASS)

## Lectura de Datos
dengue <-read.table("dengue.dat")
dengue
## Preprocesamiento
# Verificar si las variables categóricas son factores
sapply (dengue, class)
colnames(dengue)<-c("edad","nivel","sector","caso")
# Convertir a factor
dengue$nivel <- factor(dengue$nivel)
dengue$sector <- factor(dengue$sector)
dengue$caso <- factor(dengue$caso)



##  Estimacion (modelo completo)
modcompleto <- glm(caso~edad + nivel + sector, family=binomial,data=dengue)


## Selección de Variables
require(MASS)
stepAIC(modcompleto)

mod1 <- glm(caso~edad + sector, family=binomial,data=dengue)
summary(mod1)

# Mostrar coeficientes:
coef(mod1)

# Intervalos de Confianza para los coeficientes
confint(mod1)

# Impacto en la  tasa de ventajas
exp(coef(mod1))

# Tasa de ventajas e IC 95% 
exp(cbind(OR = coef(modcompleto), confint(completo)))



# Calidad de Ajuste (Prueba de Hosmer y Lemeshow)

yprob <- predict(admision.fit, type="response")
# Para el Grupo 1
g1<-head(as.data.frame(cbind(admision,yprob))[order(yprob),] ,n = 40)
g1
O1 <- sum(as.numeric(g1$admit)-1)
O1
Pbar <- mean(g1$yprob)
E1<- dim(g1)[1]*Pbar
E1
X2g1 <- (O1-E1)^2/(E1*(1-Pbar))
X2g1

library(ResourceSelection)
hl <- hoslem.test(admision.fit$y, fitted(admision.fit), g=10)
hl
cbind(hl$observed,hl$expected)

# Diagnóstico
#source.with.encoding('envel.bino.R', encoding='ISO-8859-1')
source('http://www.poleto.com/funcoes/envel.bino.txt')
envel.bino(mod2)
# Con la funcion modificada
source("envel.bino2.R")
envel.bino(mod2)

#source.with.encoding('diag.bino.R', encoding='ISO-8859-1')
source('http://www.poleto.com/funcoes/diag.bino.txt')
diag.bino(admision.fit)
source("diag.bino2.R")
diag.bino(admision.fit)


mod2 <- glm(caso~nivel*edad+sector, family=binomial,data=dengue)
summary(mod2)
plot(density(),ylab="Densidade")
boxplot(split(dengue$edad, dengue$nivel))
boxplot(split(cpue, ano))

par(mfrow=c(1,2))
boxplot(split(latitude, frota))
boxplot(split(longitude, frota))

par(mfrow=c(1,1))
boxplot(split(cpue, trimestre))
