library("MASS")
library("ISLR")

plot(medv~age,Boston)

temp <- Boston
plotY <- function (x,y) {
  plot(temp[,y]~temp[,x], xlab=paste(names(temp)[x]," X",x,sep=""),
       ylab=names(temp)[y])
}

# Representando 
par(mfrow=c(3,4))
x <- sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2])
par(mfrow=c(1,1))

# Afinando un poco más
par(mfrow=c(3,3))
x <- sapply(c(1, 5, 6, 7, 8, 10, 11, 12, 13), plotY, dim(temp)[2])
par(mfrow=c(1,1))

# Modelo lineal entre las mejores (6,13)
fit1=lm(Boston$medv~Boston$lstat)
fit1

fit2=lm(medv~rm,data=Boston)
fit2

# Modelo lstat
summary(fit1)
par(mfrow=c(2,1))
plot(medv~lstat,Boston)
abline(fit1,col="red")
confint(fit1)

# Modelo rm
summary(fit2)
plot(medv~rm,Boston)
abline(fit2,col="blue")
par(mfrow=c(1,1))
confint(fit2)

# MODELO LSTAT
names(fit1)
# Error cuadrático medio
sqrt(sum(fit1$residuals^2)/length(fit1$residuals))
sqrt(sum(fit1$residuals^2)/(length(fit1$residuals)-2))

predict(fit1,data.frame(lstat=c(5,10,15)))
#Otra forma de calcular el error cuadrático
yprime=predict(fit1,data.frame(lstat=Boston$lstat))
sqrt(sum(abs(Boston$medv-yprime)^2)/length(yprime))


# MODELO LINEAL MÚLTIPLE
# Forma descendente
fit3=lm(medv~lstat+age,data=Boston)
summary(fit3)

# Visualización de pares de variables por escala de grises
temp <- Boston
plot(temp[,-dim(temp)[2]],pch=16,col=gray(1-(temp[,dim(temp)[2]]/max(temp[,dim(temp)[2]]))))

fit4=lm(medv~lstat+rm,data=Boston)
summary(fit4)

# Todas las variables
fit5=lm(medv~.,data=Boston)
summary(fit5)
# Eliminamos las no relevantes
fit6=lm(medv~.-age-indus,data=Boston)
summary(fit6)

# Mayor precisión
fit7=lm(medv~.-age-indus-chas-crim,data=Boston)
summary(fit7)

# Interacciones
# Interpretabilidad
attach(Boston)
fit8=lm(medv~lstat*rm,Boston)
summary(fit8)
plot(medv~lstat)
points(lstat,fitted(fit8),col="green",pch=20)
# Precisión -> Añado la interacción al fit7 --> Mejor resultado hasta el momento con diferencia
fit72 = lm(medv~.-age-indus-chas-crim+lstat*rm,data=Boston)
summary(fit72)
plot(medv~lstat)
points(lstat,fitted(fit72),col="green",pch=20)

fit9=lm(medv~lstat +I(lstat^2),Boston)
summary(fit9)
plot(medv~lstat)
points(lstat,fitted(fit9),col="red",pch=20)

# Prueba
fitprueba=lm(medv~lstat +rm +I(lstat * rm) +I(lstat^2) +I(lstat^2 * rm),Boston)
summary(fitprueba)
plot(medv~lstat)
points(lstat,fitted(fitprueba),col="red",pch=20)

# Calculo manual del ECM a fit8
yprime=predict(fit8,Boston)
sqrt(sum(abs(Boston$medv-yprime)^2)/length(yprime))

# Comportamiento con log--> lm(medv~log(lstat)+lstat,Boston)
fit10 =  lm(medv~log(lstat)+lstat,Boston)
summary(fit10)
plot(medv~lstat)
points(lstat,fitted(fit10),col="red",pch=20)


## LABORATORIO 2
library("kknn")
fitknn1 <- kknn(medv ~ ., Boston, Boston)
names(fitknn1)

# Visualización
plot(medv~lstat)
points(lstat,fitknn1$fitted.values,col="blue",pch=20)

# ECM
yprime = fitknn1$fitted.values
sqrt(sum((Boston$medv-yprime)^2)/length(yprime)) #RMSE

# Utilizando los modelos de regresión lineal
fitknn2 <- kknn(medv ~ lstat*rm+I(lstat^2)+age+crim+dis, Boston, Boston)
yprime = fitknn2$fitted.values; sqrt(sum((Boston$medv-yprime)^2)/length(yprime))

fitknn3 <- kknn(medv ~ lstat*rm+I(lstat^2)+age+crim+dis+black+nox, Boston, Boston)
yprime = fitknn3$fitted.values; sqrt(sum((Boston$medv-yprime)^2)/length(yprime))

fitknn4 <- kknn(medv ~ . + lstat*rm+I(lstat^2) - chas, Boston, Boston)
yprime = fitknn4$fitted.values; sqrt(sum((Boston$medv-yprime)^2)/length(yprime))

# Sin utilizarlos
fitknn5 <- kknn(medv ~ . - chas, Boston, Boston)
yprime = fitknn5$fitted.values; sqrt(sum((Boston$medv-yprime)^2)/length(yprime))

fitknn6 <- kknn(medv ~ . - chas - ptratio -zn, Boston, Boston)
yprime = fitknn6$fitted.values; sqrt(sum((Boston$medv-yprime)^2)/length(yprime))

plot(medv~lstat)
points(lstat,fitknn1$fitted.values,col="blue",pch=20)
points(lstat,fitknn5$fitted.values,col="red",pch=20)
points(lstat,fitknn6$fitted.values,col="green",pch=20)
