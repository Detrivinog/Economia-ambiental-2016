#Estimación de modelos de valoración contingente

setwd("C:/Users/David Esteban/Google Drive/Materias cursadas/Economía ambiental/Regresion")
datos<-read.csv2("conting.csv")
str(datos)
head(datos,10)
summary(datos)
table(datos$CANTIDAD)
sum(is.na(datos$CANTIDAD))

require(lattice)
xyplot(INGMEN~CANTIDAD,group=DAP,
       data=datos,type="p",auto.key=TRUE)

xyplot(jitter(INGMEN)~jitter(CANTIDAD),group=DAP,
       data=datos,type="p",auto.key=TRUE)


head(datos,3)
#Regresión logística de DAP versus CANTIDAD ofrecida
conting.dap.ingreso= glm(DAP ~ log(INGMEN), data=datos, family=binomial(link="logit"))
summary(conting.dap.ingreso) #Null deviance, residual 
logLik(conting.dap.ingreso) 

conting.dap.cantidad=glm(DAP ~ log(CANTIDAD), data=datos, family=binomial(link="logit"))
summary(conting.dap.cantidad)

#POr que negativo 
#Intervalos de confianza
confint(conting.dap.cantidad)
#Prueba de significancia global con test de razón de verosimilitud
with(conting.dap.cantidad, null.deviance - deviance)
with(conting.dap.cantidad, df.null - df.residual)
with(conting.dap.cantidad, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#Prueba anova
anova(conting.dap.cantidad,test="Chisq")
#Calculando los valores predichos
problogit = predict(conting.dap.cantidad, type="response")

#Comparación de valores observados y predichos para las primeras 20 observaciones
comparacion <- cbind(datos$DAP,problogit)
head(comparacion,20)

#Verificando por que las dos primeras observaciones tienen el mismo valor predicho
#Deben tener la misma cantidad ofrecida
datos[1:2,]

#Regresión logística de DAP versus CANTIDAD ofrecida y EDAD
conting.dap.cantidad.edad = glm(DAP ~ CANTIDAD + EDAD, data=datos, family=binomial(link="logit"))
summary(conting.dap.cantidad.edad)
logLik(conting.dap.cantidad.edad)
#Intervalos de confianza
confint(conting.dap.cantidad)
#Prueba de significancia global con test de razón de verosimilitud
with(conting.dap.cantidad.edad, null.deviance - deviance)
with(conting.dap.cantidad.edad, df.null - df.residual)
with(conting.dap.cantidad.edad, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

anova(conting.dap.cantidad.edad,test="Chisq")
#Regresión probit de DAP versus CANTIDAD ofrecida
conting.dap.cantidad = glm(DAP ~ CANTIDAD, data=datos, family=binomial(link="probit"))
summary(conting.dap.cantidad)
coef(conting.dap.cantidad)[1]/coef(conting.dap.cantidad)[2]
logLik(conting.dap.cantidad)
#Intervalos de confianza
confint(conting.dap.cantidad)
#Prueba de significancia global con test de razón de verosimilitud
with(conting.dap.cantidad, null.deviance - deviance)
with(conting.dap.cantidad, df.null - df.residual)
with(conting.dap.cantidad, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
anova(conting.dap.cantidad, test="Chisq")
#Calculando los valores predichos
probprobit = predict(conting.dap.cantidad.edad, type="response")

#Regresión probit de DAP versus CANTIDAD ofrecida y EDAD
conting.dap.cantidad.edad = glm(DAP ~ CANTIDAD + EDAD, data=datos, family=binomial(link="probit"))
summary(conting.dap.cantidad.edad)
logLik(conting.dap.cantidad.edad)
#Intervalos de confianza
confint(conting.dap.cantidad)
#Prueba de significancia global con test de razón de verosimilitud
with(conting.dap.cantidad.edad, null.deviance - deviance)
with(conting.dap.cantidad.edad, df.null - df.residual)
with(conting.dap.cantidad.edad, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#Comparación de valores predichos de logit y probit 
comparacion<-data.frame(problogit,probprobit)
head(comparacion,10)
comparacion$prueba=with(comparacion, problogit<probprobit)
sum(comparacion$prueba)*100/length(comparacion$prueba)
min(comparacion)






