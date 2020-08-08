# CURSOS ECONOMÍA Y MEDIO AMBIENTE. PERÍODO ACADÉMICO 01 DE 2013
#Introducción a R con la base de datos Beefdemand
#Beefdemand consiste de datos de consumo per-cápita de carne de vaca, el precio de carne de vaca, el precio 
#de carne de cordero, el precio de carne de cerdo e ingreso disponible per-cápita en Australia, para el período
#1949-1965. Todos los precios e ingreso se deflactaron con 1953 como año base.
#Source: Commonwealth Bureau of Census and Statistics, QUARTERLY SUMMARY OF AUSTRALIAN STATISTICS. Datos reportados en
#Griffiths, W. E., R. C. Hill and G. G. Judge. 1993. Learning and Practicing Econometrics. John Wiley & Sons, Inc.
#(Table 9.8, page 318)


#Lectura de datos
#Opción 1. file choose y apertura de ventana para encontrar archivo con extensión txt 
beefdemand <- read.table(file.choose(),header=TRUE)
beefdemand
str(beefdemand)

#Opción 2. Abrir directamente un archivo con extensión csv, con decimal denotado por punto
#Definiendo el directorio de trabajo (set working directory from tools in RStudio)
dir()
beefdemand <- read.csv2("Beefdemand.csv")
beefdemand
#Figura de cantidad consumidad de carne de vaca versus su precio
with(beefdemand,plot(qb,pb, type="p", col="blue",cex=1.5, xlab="Cantidad de carne de vaca", 
                     ylab="Precio de la carne de vaca",las=1))

#Estimación empírica de la demanda por carne de vaca
#Modelo de cantidad demandada de carne en función del propio precio
attach(beefdemand)
demand.qb.pb <- lm(log(qb)~log(pb))
summary(demand.qb.pb)
plot(log(qb)~log(pb),las=1)
#Adicionando la línea de regresión
abline(demand.qb.pb, col="red")

#Valores predichos
predict.demand <- predict(demand.qb.pb, int="predict")
predict.demand
head(predict.demand)
#verificando el tipo de objeto de predict.demand
str(predict.demand)
#Extracción de la segunda columna de predict.demand, correspondiente a "lwr"
predict.demand[,2]

#Modelo de cantidad demandada de carne en función del propio precio y de ingreso
demand.qb.pb.y <- lm(log(qb)~log(pb)+log(y))
summary(demand.qb.pb.y)

#Modelo de cantidad demandada de carne en función del propio precio, precio de bienes relacionados e ingreso
demand.qb.pb.pl.pp.y <- lm(log(qb)~log(pb)+log(pl)+log(pp)+log(y))
summary(demand.qb.pb.pl.pp.y)

beefdemand2 <- data.frame(rbind(beefdemand,beefdemand,beefdemand))
beefdemand2
attach(beefdemand2)
demand.qb.pb.pl.pp.y <- lm(log(qb)~log(pb)+log(pl)+log(pp)+log(y), beefdemand2)
summary(demand.qb.pb.pl.pp.y)

#Uso de la librería CAR sobre análisis de regresión (CAR: Companion to Appied Regression, Fox & Weisberg (2011))
require(car)

#Estimación de un modelo no lineal
#Ejemplo de tasa de captura de atun. Fuente: Verzani, J. (2005). Using R for introductory statistics, pag. 353
#Cargando la librería UsingR
require(UsingR)
install.packages("UsingR")
#En la librería UsingR se encuentra la base de datos yellowfin

with(yellowfin, plot(count~year, las=1))

#Definición de una función en R que sea razonable para la figura de count versus year

model <- function (t,N,r,d)  N*(exp(-r*(t-1952))*(1-d)+d)

#Graficando el modelo con valores de partida arbitrarios
curve(model(x, N=6, r=1/10, d=0.1), add=TRUE, col="blue", lwd=1.5) 

#Regresión no lineal
reg.yellow <- nls(count~model(year,N,r,d), start=c(N=6, r=1/10, d=0.1), data=yellowfin)
reg.yellow

curve(model(x, N=6.02, r=0.0939, d=0.0539), add=TRUE, lty=2, lwd=2, col="red")
legend(1970, 6, legend=c("modelo exploratorio","modelo exponencial"), lty=1:2, col=c("blue","red"))


#Estimación de un modelo no lineal
#Ejemplo de Sea urchin growth. Fuente: Verzani, J. (2005). Using R for introductory statistics, pag. 354-356
#Sea urchins are herbivores of temperate marine ecosystems worldwide. 
#Unlike other sessile benthic invertebrates, sea urchins can persist at high population density when 
#resources are limited owing to their plastic allocation of resources.

#Cargando la librería UsingR
require(UsingR)

#En la librería UsingR se encuentra la base de datos urchin.growth
with(urchin.growth, plot(jitter(size)~jitter(age), las=1, 
                         xlab="age", ylab="size",main="Urchin growth by age"))

#Some models for growth: logistic and Richards
#Logistic function
logistic <- function(t,Y,k,t0)  Y*(1+exp(-k*(t-t0)))^(-1)

#Exploratory graph
curve(logistic(x, Y=60, k=1, t0=2), add=TRUE, col="blue", lwd=1.5)

#Fitting the logistic model using NLS
model.logistic <- nls(size ~ logistic(age,Y,k,t0), start=c(Y=60, k=1, t0=2),
                      data = urchin.growth)
model.logistic

curve(logistic(x, Y=53.903, k=1.393, t0=1.958), add=TRUE, col="red", lwd=1.5)

#AIC
AIC(model.logistic)

#Richards function
Richards <- function(t,Y,k,t0,m)  Y*(1-exp(-k*(t-t0)))^(m) 

curve(Richards(x, Y=53.903, k=1.393, t0=1.958, m=1), add=TRUE, col="green", lwd=1.5)

#Fitting the Richards model using NLS USING THE THE VALUES OF THE ESTIMATED
#LOGISTIC MODEL
model.Richards <- nls(size ~ Richards(age,Y,k,t0,m), start=c(Y=53, k=1.393, 
                            t0=1.958, m=1), data = urchin.growth)


#Changing t0 AND k values
model.Richards <- nls(size ~ Richards(age,Y,k,t0,m), start=c(Y=53, k=0.5, 
                            t0=0, m=1), data = urchin.growth)

model.Richards

curve(Richards(x, Y=57.26, k=0.78, t0=-0.8587, m=6.0636), add=TRUE, 
      col="darkgreen", lwd=1.5, lty=2)

legend(4,20, legend=c("Logistic", "Richards"), lty=1:2, col=c("red", "dark green"))
#AIC
AIC(model.Richards)
#Modelos logit
#Source: Cramer, J.S. (2003). Logit Models: from Economics and other Fields. Cambridge University Press
#Los datos corresponden a posesión privada de carro en Holanda
#Variables: private car ownership status numbered from 0 to 3 (none, used, new, more), income per equivalente adult 
#(calculated by counting the first adult as 1, other adults as 0.7,and children as 0.5.), household size in number of
#equivalente adult, age of the head of household in five-year group (measured by five-year classes,
#starting with the class 'below 20'), the degree of urbanization (measured on a six-point scale from
#countryside (1) to city (6), and a dummy variable for the presence of a business car in
#the household. A business car is a car that is primarily used for business or professional purposes, 
#regardless of whether it is paid for wholly or in part by the employer or whether its costs are tax-deductible. 


#Lectura de los datos, especificando los nombres de las variables 
car <- read.table(file.choose(),sep=",",header=F,dec=".",col.names=c("PCAR","INC","SIZE","AGE","URBA","BUSCAR"))
head(car,30)
attach(car)


#Creación de un dataframe para almacenar la variable dummy para posesión de carro, sea usado o nuevo
carownership <-ifelse((PCAR==1|PCAR==2),1,0)
str(carownership)
head(carownership,10)

plot(INC,carownership,xlab="Income",ylab="Car",las=1)
which(INC>120000)
car[1633,]

modellogit <- glm(carownership~log(INC),family=binomial)
summary(modellogit)






