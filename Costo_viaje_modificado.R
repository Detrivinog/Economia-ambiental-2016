#Estimación de modelos de costo del viaje
#Abriendo base de datos 
setwd("C:/Users/David Esteban/Google Drive/Materias cursadas/Economía ambiental/Regresion")
aguas <- read.csv2("aguas.csv")
str(aguas)
summary(aguas)
attach(aguas)

#Estimación de un modelo de demanda recreacional asumiendo distribución normal y sin considerar truncación
modelo.normal <- lm(TRIPS ~ TCOST2 + TCOSTSUB2 + WAGE + DUMMYWATER, data = aguas)
summary(modelo.normal)
str(modelo.normal)
#Estimación de un modelo de demanda recreacional asumiendo distribución normal y considerando truncación
require(truncreg)
modelo.normal.truncado <- truncreg(TRIPS ~ TCOST2 + TCOSTSUB2 + WAGE + DUMMYWATER, data=aguas, point = 0, direction = "left")
summary(modelo.normal.truncado)

#Estimación de un modelo Poisson no truncado usando la función glm 
modelo.poisson <- glm(TRIPS ~ TCOST2 + TCOSTSUB2 + WAGE + DUMMYWATER, data = aguas, family=poisson)
summary(modelo.poisson)
str(modelo.poisson)

#Test de sobredispersión en modelo Poisson Var(y/x)=E(y/x)+a*{E(y/x)^2}
#El test es realizado usando una regresión auxiliar de {(obs-predicted)^2-obs}/predicted en función de
#predicted, sin intercepto y verificando con una prueba "t" si el coeficiente de predicted es cerode

dispersion<-function(modelo){
  pred=fitted.values(modelo)
  z=((modelo$data[,1]-pred)^2-modelo$data[,1])/pred
  mod=lm(z~0+pred)
  return(summary(mod))
}
dispersion(modelo.poisson)
#Prueba de dispersión
require(AER)
dispersiontest(modelo.poisson)


#Estimación de un modelo Poisson truncado en cero, usando la función glm
TRIPS1=TRIPS-1
modP<- glm(TRIPS ~ TCOST1 + TCOSTSUB1 + DUMMYWATER+AGE+GROUP, data = aguas, family=poisson)
summary(modP)

require(AER)
dispersiontest(modP)

EC=1/-coef(modP)[2]
EC*mean(aguas$TRIPS)

#Programación de la función de verosimilitud
f1=TRIPS1 ~ TCOST2 + TCOSTSUB2 + WAGE + DUMMYWATER
f2=terms.formula(f1)
str(f2)
class(f1)
f2[[3]] #trips
f2
attr(f2,"term.labels")[1]
terms2=rownames(attr(f2,"factors"))
aguas[terms2[1]]
aguas["TCOST2"]
length()

verosimilitud<-function(formula,datos,metodo="BNeg"){
  f2=terms.formula(formula)
  term=rownames(attr(f2,"factors"))
  Y=as.matrix(datos[term[1]][[1]])
  X=matrix(ncol=length(term),nrow=dim(Y)[1])
  X[,1]=rep(1,dim(Y)[1])
  for(i in 2:length(term)){
    X[,i]=datos[term[i]][[1]]
  }
  beta=as.matrix(coef(lm(formula,data = datos)))
  if(metodo=="Poisson"){
      poissonreg.lik <- function(beta,y,X){
      XB <- (X %*% beta)
      logl <- sum(-1*exp(XB)) + sum(y*(XB))
      return(-logl)
      }
      lik=poissonreg.lik
  }else{
    NegbinEnglin_Shonkwiler.lik <- function(beta,y,X){
      alfa <- 0.01
      XB <- (X %*% beta)
      logl <- sum(lgamma(y + (1/alfa))) + sum(y*log(alfa)) + sum((y - 1)*(XB)) - 
        sum((y + (1/alfa))*log(1 + alfa*exp(XB)))- sum(lgamma(1/alfa))
      return(-logl)
    }
    alfa=0.01
    lik= NegbinEnglin_Shonkwiler.lik
    beta<-as.matrix(beta,alfa)
  }
  modelo=optim(beta, lik,method="Nelder-Mead",hessian=T,y=Y,X=X,control=list(maxit=10000))
  se=sqrt(diag(solve(modelo[6][[1]])))
  t=modelo[1][[1]]/se
  Pvalue=2*(1-pt(abs(t),nrow(X)-ncol(X)))
  results<-data.frame(modelo[1][[1]],se,t,Pvalue)
  colnames(results)<-c("beta","se","t","p-value")
  rownames(results)=c("constante",term[-1])
  return(print(results, digits = 3))
}
aguas$TCOST2a<-with(aguas, TCOST2/10)
aguas$TCOSTSUB2a<-with(aguas, TCOSTSUB2/10)
aguas$WAGEa<-with(aguas, WAGE/1000)
verosimilitud(TRIPS ~ TCOST2a + TCOSTSUB2a + WAGEa + DUMMYWATER, aguas, "BNeg")
verosimilitud(TRIPS ~ TCOST2a + TCOSTSUB2a + WAGEa + DUMMYWATER, aguas, "Poisson")

X <- as.matrix(cbind(1,aguas[,3]/10, aguas[,6]/10, aguas[,8]/1000, aguas[,9]))
y <- as.matrix((aguas[,1]))
beta <- as.matrix(c(0,0,0,0,0))
alfa <- 0.01


#Especificación del vector beta correspondiente a los coeficientes estimados de un modelo asumiendo una 
#distribución normal, el cual se especificó arriba
beta<-as.matrix(coef(modelo.normal))


#Definición de la función loglik para Poisson
#Observe que en la función se omite el término ln(yi!). Ello por que no contiene el vector beta de
#parámetros que se desea estimar. Por tanto, es posible eliminar ese término de la optimización
poissonreg.lik <- function(beta,y,X){
  XB <- (X %*% beta)
  logl <- sum(-1*exp(XB)) + sum(y*(XB))
  return(-logl)
}
#Estimación del modelo Poisson usando optim. Observe que el método de estimación es Nelder-Mead.
#Las estimaciones son sensibles al escalamiento de las variables y al método de estimación, el cual
#puede contar con una forma específica para el cálculo de los errores estandar de los parámetros estimados
#Observe también que el número máximo de iteraciones se define igual a 10000. Por defecto es 500
#pero la optimización las puede exceder fácilmente 
mod_poisson1 <- optim(beta,poissonreg.lik,method="Nelder-Mead",hessian=T,y=y,X=X,control=list(maxit=10000))
mod_poisson1[6]

#La matriz de información de Fisher es igual al negativo del valor esperado
#del Hessiano (-E(H)). Bajo condiciones generales la matriz de varianzas y covarianzas
#del estimador de máxima verosimilutud es la inversa de la matriz de información de Fisher.
#Es decir, (-E(H)^-1).Observe que no se coloca el signo menos, por que por default optim
#hace una minimización, que equivale a menos la maximización. Con solve se obtiene el inverso del hessiano
Hessianoinv<-solve(mod_poisson1$hessian)
#Los errores estandar corresponden a la raiz cuadrada de la diagonal principal
#de la matriz de información de Fisher
se<-sqrt(diag(Hessianoinv))

#Calculos de valores de t
t<-mod_poisson1$par/se

#Calculo de p-values usando una función de distribución student con n-k grados
#de libertad. Observe que se multiplica por dos, por ser prueba de dos colas 
pval<-2*(1-pt(abs(t),nrow(X)-ncol(X)))

#Presentación de resultados
results<-cbind(mod_poisson1$par,se,t,pval)
colnames(results)<-c("beta","se","t","p-value")
rownames(results)<-c("Constant","TCOST2","TCOSTSUB2", "WAGE", "DUMMYWATER")
print(results,digits=3)
class(results)

#Definición de la función loglik para Binomial Negativa que corrige por truncación y estratificación 
#endógena. Modelo propuesto por Englin&Shonkwiler (1995)
#Observe que en la función se omiten los términos ln(yi) y logGamma(yi + 1). Ello por que no contiene el vector de
#parámetros que se desea estimar. Por tanto, es posible eliminar ese término de la optimización
#Observe también que alfa(i) se parametrizó como alfa, siendo un escalar. Otra alternativa es parametrizar
#alfa(i)=alfa/lambda(i).
#Es posible evaluar que tan sensible es la optimización al valor inicial de alfa, lo cual es posible al
#considerar valores distintos, pero NUNCA cero!!!
NegbinEnglin_Shonkwiler.lik <- function(beta,y,X){
  alfa <- 0.01
  XB <- (X %*% beta)
  logl <- sum(lgamma(y + (1/alfa))) + sum(y*log(alfa)) + sum((y - 1)*(XB)) - sum((y + (1/alfa))*log(1 + alfa*exp(XB)))
  - sum(lgamma(1/alfa))
  return(-logl)
}

beta1 <- as.matrix(beta,alfa)

mod_negbinEnglin_Shonkwiler <- optim(beta1,NegbinEnglin_Shonkwiler.lik,
                                     method="Nelder-Mead",hessian=T,y=y,X=X,control=list(maxit=10000))
mod_negbinEnglin_Shonkwiler

Hessianoinv<-solve(mod_negbinEnglin_Shonkwiler$hessian)

se<-sqrt(diag(Hessianoinv))

t<-mod_negbinEnglin_Shonkwiler$par/se

#Calculo de p-values usando una función de distribución student con n-k grados
#de libertad. Observe que se multiplica por dos, por ser prueba de dos colas 
pval<-2*(1-pt(abs(t),nrow(X)-ncol(X)))

#Presentación de resultados
results<-cbind(mod_negbinEnglin_Shonkwiler$par,se,t,pval)
colnames(results)<-c("beta","se","t","p-value")
rownames(results)<-c("Constant","TCOST2","TCOSTSUB2", "WAGE", "DUMMYWATER")
print(results,digits=3)

summary(modelo.normal)

significance<-function(x){
  y=c(0, 0.001, 0.01, 0.05, 0.1, 1)
  v=c("***","**","*","."," ")
  c=vector()
  for (i in length(x)) {
    for (j in 1:5) {
      if(x[i]>=y[j] & x[i]<y[j+1]){
        c[i]=v[j]
        break
      }else {next}
    }
  }
  return(c)
}
significance(veroP[4][[1]])
significance(z)
z=c(0.1, 0.0012, 0.011, 0.051, 0.11,0.91)
summary(modelo.normal)

