# Paquetes
library(ISLR)

# Datos
attach(College)

Private = factor(Private)
head(College)


#Semilla
set.seed(281117)

n = sample(nrow(College), 600)

# Muestra de entrenamiento (600 observaciones)
College.training = College[n,]

# Muestra de evaluacion
College.testing = College[-n,]



#### Item a

library(MASS)

mod1 = glm(Outstate ~ ., data = College.training, family=gaussian(link=identity))

stepAIC(mod1)


mod1.GLM = glm(Outstate ~ Private + Apps + Accept + Enroll + Top10perc + 
                 Room.Board + Personal + Terminal + S.F.Ratio + perc.alumni + 
                 Expend + Grad.Rate, data = College.training, family=gaussian(link=identity))


library(gam)

mod2.df1 = gam(Outstate ~ Private + s(Apps,2) + s(Accept,2) + s(Enroll,2) + s(Top10perc,2) + s(Room.Board,2) + s(Personal,2) + s(Terminal,2) + s(S.F.Ratio,2) + s(perc.alumni,2) + s(Expend,2) + s(Grad.Rate,2), data=College.training)

summary(mod2.df1)

mod3.df4 = gam(Outstate ~ Private + s(Apps,5) + s(Accept,5) + s(Enroll,5) + s(Top10perc,5) + s(Room.Board,5) + s(Personal,5) + s(Terminal,5) + s(S.F.Ratio,5) + s(perc.alumni,5) + s(Expend,5) + s(Grad.Rate,5), data=College.training)

summary(mod3.df4)


mod4.span025 = gam(Outstate~ Private+lo(Apps,span = 0.25)+ lo(Accept,span = 0.25)+lo(Enroll,span = 0.25) + lo(Top10perc,span = 0.25) + lo(Room.Board,span = 0.25) + lo(Personal,span = 0.25) + lo(Terminal,span = 0.25) +lo(S.F.Ratio,span = 0.25)+ lo(perc.alumni,span = 0.25)+ lo(Expend, span = 0.25) + lo(Grad.Rate,span = 0.25), data= College.training)
summary(mod4.span025)

mod5.span075 = gam(Outstate~ Private+lo(Apps,span=0.75)+ lo(Accept,span=0.75)
                   + lo(Enroll,span=0.75) + lo(Top10perc,span=0.75) + lo(Room.Board,span=0.75) + 
                     lo(Personal,span=0.75) + lo(Terminal,span=0.75) +lo(S.F.Ratio,span=0.75)+ 
                     lo(perc.alumni,span=0.75)+ lo(Expend,span=0.75) + lo(Grad.Rate,span=0.75), 
                   data= College.training)

summary(mod5.span075)

VAL.AIC=AIC(mod1.GLM,mod2.df1,mod3.df4,mod4.span025,mod5.span075)
delta.AIC=VAL.AIC[,2]-min(VAL.AIC[,2])
VAL.AIC=cbind(VAL.AIC,delta.AIC)

VAL.AIC


# Prediccion
pred1 = predict(mod1.GLM,newdata=College.testing)
pred2 = predict(mod2.df1,newdata=College.testing)
pred3 = predict(mod3.df4,newdata=College.testing)
pred4 = predict(mod4.span025,newdata=College.testing)
pred5 = predict(mod5.span075,newdata=College.testing)

Tabla.pred = cbind(data.frame(College.testing$Outstate),pred1,pred2,pred3,pred4)

Tabla.dif=cbind((Tabla.pred[,1]-Tabla.pred[,2])^2,
                (Tabla.pred[,1]-Tabla.pred[,3])^2,
                (Tabla.pred[,1]-Tabla.pred[,4])^2,(Tabla.pred[,1]-Tabla.pred[,5])^2,
(Tabla.pred[,1]-Tabla.pred[,5])^2)

ECM=apply(Tabla.dif,2,mean)

anova(mod3.df4,mod3.df4.3,test="F")

mod3.df4.2 = gam(Outstate ~ Private + s(Apps,5) + s(Accept,5) + s(Enroll,5) + s(Top10perc,5) + s(Room.Board,5) + s(Personal,5) + s(Terminal,5) + s(S.F.Ratio,5) + s(perc.alumni,5) + Expend + s(Grad.Rate,5), data=College.training)
mod3.df4.3 = gam(Outstate ~ ., data=College.training)

par(mfrow=c(3,4))
plot.gam(mod1.GLM, se=TRUE, col="red")
plot.gam(mod2.df1, se=TRUE, col="red")
plot.gam(mod3.df4, se=TRUE, col="red")
plot.gam(mod4.span025, se=TRUE, col="red")
plot.gam(mod5.span075, se=TRUE, col="red")



Grad.Ratelims=range(Grad.Rate)
Grad.Rate.grid=seq(from=Grad.Ratelims[1],to=Grad.Ratelims[2])
plot(College.testing$Grad.Rate,College.testing$Outstate,xlim=Grad.Ratelims,cex=.5,col="darkgrey")

m2=smooth.spline(College.training$Grad.Rate,College.training$Outstate,df=2)
m3=smooth.spline(College.training$Grad.Rate,College.training$Outstate,df=5)
lines(m2,col="red",lwd=2)
lines(m3,col="blue",lwd=2)
title("Smoothing Spline")
legend("topright",legend=c("1 DF","4 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

anova()

Expendlims=range(Expend)
Expend.grid=seq(from=Expendlims[1],to=Expendlims[2])
plot(College.testing$Expend,College.testing$Outstate,xlim=Expendlims,cex=.5,col="darkgrey")

m2=smooth.spline(College.training$Expend,College.training$Outstate,df=2)
m3=smooth.spline(College.training$Expend,College.training$Outstate,df=5)
lines(m2,col="red",lwd=2)
lines(m3,col="blue",lwd=2)
title("Smoothing Spline")
legend("topright",legend=c("1 DF","4 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)
