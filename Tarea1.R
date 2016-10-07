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
title <- bquote('CDF; ' ~ alpha~ '='~ .(a))
plot(z,F(z,a),type="l",col ="red",xlab='z',ylab='CDF',main=title)
plot(ecdf1,add=TRUE,col="blue")

#Gr??fico de densidad contra histograma
h<-hist(x,plot=FALSE)
title <- bquote('Histograma; ' ~ alpha~ '='~ .(a))
plot(h,freq=FALSE,col="blue",main=title)
lines(z,f(z,a),col='red')

#k es el n??mero de intervalos para los cuantiles
k=4
prob=seq(0.0,1.0,length=k+1)[2:4]

#qq-plot
title <- bquote('q-q plot; ' ~ alpha~ '='~ .(a))
plot(z,z,type='l',xlab='',ylab='',main=title)
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
plot_results2<-function(a){
  N=c(20,50,100,200,500)
  m=500
  #Arreglo para los valores del sesgo
  s1=c()
  s2=c()
  E1=c()
  E2=c()
  for (n in N){
    u<-runif(n*m)
    x<-matrix(Finv(u,a),nrow=n,ncol=m)
    est <-t(apply(x,2,theta))
    #Boxplot
    title <- bquote(n==.(n) ~ alpha==.(a))
    boxplot(est,main=title,names=c(expression(hat(theta)[1]),expression(hat(theta)[2])),col=c('darkgreen','darkorange'))
    
    sesgo1 <-mean(est[,1])-a
    s1=append(s1,sesgo1)
    sesgo2 <-mean(est[,2])-a
    s2=append(s2,sesgo2)
    ECM1 <- mean((est[,1]-a)^2)
    E1=append(E1,ECM1)
    ECM2 <- mean((est[,2]-a)^2)
    E2=append(E2,ECM2)
  }
  
  #Gr??fico log-log del ECM
  title <- bquote(alpha ~ '=' ~ .(a))
  plot(log(N,base=exp(1)),log(E1,base=exp(1)),col='darkgreen',main=title,xlab='ln(N)',ylab='ECM')
  points(log(N,base=exp(1)),log(E2,base=exp(1)),col='darkorange')
  
  Ef_rel=E1/E2
  
  print(paste('sesgo1= ',toString(s1),' ,a=',a ,nsep=''))
  print(paste('sesgo2= ',toString(s2),' ,a=',a ,nsep=''))
  print(paste('E1= ',toString(E1),' ,a=',a ,nsep=''))
  print(paste('E2= ',toString(E2),' ,a=',a ,nsep=''))
}

for (a in A){
  plot_results2(a)
}



