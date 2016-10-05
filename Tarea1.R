#Inversa de la fda de una beta(a,1)
Finv <- function(w,a){
  return(w^(1/a))
}
#fda de una beta(a,1)
F <- function(w,a){
  return(w^a)
}
#fdd de una beta(a,1)
f<-function(w,a){
  return(a*w^(a-1))
}

#Funci??n que realiza los gr??ficos de la primera parte 
plot_results<-function(a,n){
u<-runif(n)
x<-Finv(u,a)
z<- seq(0.0,1.0,length=1000)

#fda empirica.
ecdf1<-ecdf(x)

#Gr??fico de la fda emp??rica y fda de beta(a,1)
plot(z,F(z,a),type="l",col ="red",main='CDF')
plot(ecdf1,add=TRUE,col="blue")

#Gr??fico de densidad contra histograma
h<-hist(x)
plot(h,freq=FALSE,col="blue")
lines(z,f(z,a),col='red')

#k es el n??mero de intervalos para los cuantiles
k=4
prob=seq(0.0,1.0,length=k+1)[2:4]

#qq-plot
plot(z,z,type='l')
points(quantile(ecdf1,prob),qbeta(prob,a,1))
}

#Corre el programa
A=c(0.3,1,3,10)
for (a in A){
  plot_results(a,1000)
}

#Estimadores
theta<-function(x){
  return(c(1/(length(x)/sum(x)-1),-length(x)/sum(log(x,base=exp(1)))))
}

#Funci??n que realiza la segunda parte del ejercicio:
plot_results2<-function(a,n){
  m=500
  u<-runif(n*m)
  x<-matrix(Finv(u,a),nrow=n,ncol=m)
  est <-t(apply(x,2,theta))
  #Boxplot
  boxplot(est)
  sesgo1 <-mean(est[,1])-a
  sesgo2 <-mean(est[,2])-a
  ECM1 <- mean((est[,1]-a)^2)
  ECM1 <- mean((est[,2]-a)^2)
}

N=c(20,50,100,200,500)
for (a in A){
  for (n in N){
    plot_results2(a,n)
  }
}

a=0.3
n=20
m=500
u<-runif(n*m)
x<-matrix(Finv(u,a),nrow=n,ncol=m)
est <-t(apply(x,2,theta))
est[,1]






