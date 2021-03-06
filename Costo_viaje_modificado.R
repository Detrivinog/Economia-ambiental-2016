#Estimaci�n de modelos de costo del viaje
#Abriendo base de datos 
setwd("C:/Users/David Esteban/Google Drive/Materias cursadas/Econom�a ambiental/Regresion")
aguas <- read.csv2("aguas.csv")
str(aguas)
summary(aguas)
attach(aguas)

#Estimaci�n de un modelo de demanda recreacional asumiendo distribuci�n normal y sin considerar truncaci�n
modelo.normal <- lm(TRIPS ~ TCOST2 + TCOSTSUB2 + WAGE + DUMMYWATER, data = aguas)
summary(modelo.normal)
str(modelo.normal)
#Estimaci�n de un modelo de demanda recreacional asumiendo distribuci�n normal y considerando truncaci�n
require(truncreg)
modelo.normal.truncado <- truncreg(TRIPS ~ TCOST2 + TCOSTSUB2 + WAGE + DUMMYWATER, data=aguas, point = 0, direction = "left")
summary(modelo.normal.truncado)

#Estimaci�n de un modelo Poisson no truncado usando la funci�n glm 
modelo.poisson <- glm(TRIPS ~ TCOST2 + TCOSTSUB2 + WAGE + DUMMYWATER, data = aguas, family=poisson)
summary(modelo.poisson)
str(modelo.poisson)

#Test de sobredispersi�n en modelo Poisson Var(y/x)=E(y/x)+a*{E(y/x)^2}
#El test es realizado usando una regresi�n auxiliar de {(obs-predicted)^2-obs}/predicted en funci�n de
#predicted, sin intercepto y verificando con una prueba "t" si el coeficiente de predicted es cerode

dispersion<-function(modelo){
  pred=fitted.values(modelo)
  z=((modelo$data[,1]-pred)^2-modelo$data[,1])/pred
  mod=lm(z~0+pred)
  return(summary(mod))
}
dispersion(modelo.poisson)
#Prueba de dispersi�n
require(AER)
dispersiontest(modelo.poisson)


#Estimaci�n de un modelo Poisson truncado en cero, usando la funci�n glm
TRIPS1=TRIPS-1
modP<- glm(TRIPS ~ TCOST1 + TCOSTSUB1 + DUMMYWATER+AGE+GROUP, data = aguas, family=poisson)
summary(modP)

require(AER)
dispersiontest(modP)

EC=1/-coef(modP)[2]
EC*mean(aguas$TRIPS)

#Programaci�n de la funci�n de verosimilitud
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


#Especificaci�n del vector beta correspondiente a los coeficientes estimados de un modelo asumiendo una 
#distribuci�n normal, el cual se especific� arriba
beta<-as.matrix(coef(modelo.normal))


#Definici�n de la funci�n loglik para Poisson
#Observe que en la funci�n se omite el t�rmino ln(yi!). Ello por que no contiene el vector beta de
#par�metros que se desea estimar. Por tanto, es posible eliminar ese t�rmino de la optimizaci�n
poissonreg.lik <- function(beta,y,X){
  XB <- (X %*% beta)
  logl <- sum(-1*exp(XB)) + sum(y*(XB))
  return(-logl)
}
#Estimaci�n del modelo Poisson usando optim. Observe que el m�todo de estimaci�n es Nelder-Mead.
#Las estimaciones son sensibles al escalamiento de las variables y al m�todo de estimaci�n, el cual
#puede contar con una forma espec�fica para el c�lculo de los errores estandar de los par�metros estimados
#Observe tambi�n que el n�mero m�ximo de iteraciones se define igual a 10000. Por defecto es 500
#pero la optimizaci�n las puede exceder f�cilmente 
mod_poisson1 <- optim(beta,poissonreg.lik,method="Nelder-Mead",hessian=T,y=y,X=X,control=list(maxit=10000))
mod_poisson1[6]

#La matriz de informaci�n de Fisher es igual al negativo del valor esperado
#del Hessiano (-E(H)). Bajo condiciones generales la matriz de varianzas y covarianzas
#del estimador de m�xima verosimilutud es la inversa de la matriz de informaci�n de Fisher.
#Es decir, (-E(H)^-1).Observe que no se coloca el signo menos, por que por default optim
#hace una minimizaci�n, que equivale a menos la maximizaci�n. Con solve se obtiene el inverso del hessiano
Hessianoinv<-solve(mod_poisson1$hessian)
#Los errores estandar corresponden a la raiz cuadrada de la diagonal principal
#de la matriz de informaci�n de Fisher
se<-sqrt(diag(Hessianoinv))

#Calculos de valores de t
t<-mod_poisson1$par/se

#Calculo de p-values usando una funci�n de distribuci�n student con n-k grados
#de libertad. Observe que se multiplica por dos, por ser prueba de dos colas 
pval<-2*(1-pt(abs(t),nrow(X)-ncol(X)))

#Presentaci�n de resultados
results<-cbind(mod_poisson1$par,se,t,pval)
colnames(results)<-c("beta","se","t","p-value")
rownames(results)<-c("Constant","TCOST2","TCOSTSUB2", "WAGE", "DUMMYWATER")
print(results,digits=3)
class(results)

#Definici�n de la funci�n loglik para Binomial Negativa que corrige por truncaci�n y estratificaci�n 
#end�gena. Modelo propuesto por Englin&Shonkwiler (1995)
#Observe que en la funci�n se omiten los t�rminos ln(yi) y logGamma(yi + 1). Ello por que no contiene el vector de
#par�metros que se desea estimar. Por tanto, es posible eliminar ese t�rmino de la optimizaci�n
#Observe tambi�n que alfa(i) se parametriz� como alfa, siendo un escalar. Otra alternativa es parametrizar
#alfa(i)=alfa/lambda(i).
#Es posible evaluar que tan sensible es la optimizaci�n al valor inicial de alfa, lo cual es posible al
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

#Calculo de p-values usando una funci�n de distribuci�n student con n-k grados
#de libertad. Observe que se multiplica por dos, por ser prueba de dos colas 
pval<-2*(1-pt(abs(t),nrow(X)-ncol(X)))

#Presentaci�n de resultados
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

