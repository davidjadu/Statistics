library(nortest)

Finv <- function(w,a){
  return(return((1-w)^(-1/a)))
}

Estimator1 <- function(x1){
  t1=mean(x1)
  return(t1/(1-t1))
}
Estimator2 <- function(x2){
  t2=mean(x2)
  return(t2/(t2-1))
}
Estimator3 <- function(x3){
  return(sqrt(sum(x3^2)/(12*length(x3))))
}

EMV1 <-function(x1){
  return(-length(x1)/sum(log(x1)))
}

EMV2 <-function(x2){
  return(length(x2)/sum(log(x2)))
}

EMV3 <-function(x3){
  return(sum(x3)/(3*length(x3)))
}

Z1<-function(emv1,n){
  return(sqrt(n)/3*(emv1-3))
}

Z2<-function(emv2,n){
  return(sqrt(n)/3*(emv2-3))
}

Z3<-function(emv3,n){
  return(sqrt(n/3)*(emv3-3))
}

Z1a<-function(e1,n){
  return(sqrt(n*80/3)/16*(e1-3))
}

Z2a<-function(e2,n){
  return(sqrt(2*n/3)/4*(3-e2))
}

Z3a<-function(e3,n){
  return(12/9*sqrt(n/6)*(e3-3))
}

run<-function(){
  E1=c()
  E2=c()
  E3=c()
  E1a=c()
  E2a=c()
  E3a=c()
  p1=c()
  p2=c()
  p3=c()
  p1a=c()
  p2a=c()
  p3a=c()
  m=500
  N=c(20, 30, 50, 100, 200, 500)
  for (n in N){
    x1<-matrix(rbeta(m*n,3,1),nrow = n,ncol=m)
    x2<-matrix(Finv(runif(n*m),3),nrow = n,ncol=m)
    x3<-matrix(rgamma(m*n,3,1/3),nrow = n,ncol=m)
    emv1<-t(apply(x1,2,EMV1))
    emv2<-t(apply(x2,2,EMV2))
    emv3<-t(apply(x3,2,EMV3))
    e1<-t(apply(x1,2,Estimator1))
    e2<-t(apply(x2,2,Estimator2))
    e3<-t(apply(x3,2,Estimator3))
    z1<-Z1(emv1,n)
    title <- bquote('q-q plot EMV; modelo 1, n=' ~ .(n))
    qqnorm(z1,main=title)
    z2<-Z2(emv2,n)
    title <- bquote('q-q plot EMV; modelo 2, n=' ~ .(n))
    qqnorm(z2,main=title)
    z3<-Z3(emv3,n)
    title <- bquote('q-q plot EMV; modelo 3, n=' ~ .(n))
    qqnorm(z3,main=title)

    test1<-ad.test(z1)
    a=as.numeric(test1[2])
    p1=append(p1,as.numeric(test1[2]))
    test2<-ad.test(z2)
    p2=append(p2,as.numeric(test2[2]))
    test3<-ad.test(z3)
    p3=append(p3,as.numeric(test3[2]))
    
    z1a<-Z1a(e1,n)
    title <- bquote('q-q plot Alternativo; modelo 1, n=' ~ .(n))
    qqnorm(z1a,main=title)
    z2a<-Z2a(e2,n)
    title <- bquote('q-q plot Alternativo; modelo 2, n=' ~ .(n))
    qqnorm(z2a,main=title)
    z3a<-Z3a(e3,n)
    title <- bquote('q-q plot Alternativo; modelo 3, n=' ~ .(n))
    qqnorm(z3a,main=title)
    
    test1a<-ad.test(z1a)
    p1a=append(p1a,as.numeric(test1a[2]))
    test2a<-ad.test(z2a)
    p2a=append(p2a,as.numeric(test2a[2]))
    test3a<-ad.test(z3a)
    p3a=append(p3a,as.numeric(test3a[2]))
    
    E1=append(E1,mean(emv1-3)^2)
    E1a=append(E1a,mean(e1-3)^2)
    E2=append(E2,mean(emv1-3)^2)
    E2a=append(E2a,mean(e2-3)^2)
    E3=append(E3,mean(emv1-3)^2)
    E3a=append(E3a,mean(e3-3)^2)
  }
  Efrel1=E1a/E1
  Efrel2=E2a/E2
  Efrel3=E3a/E3
  title <- bquote('p value; Modelo 1')
  plot(N,p1,main=title)
  title <- bquote('p value; Modelo 2')
  plot(N,p2,main=title)
  title <- bquote('p value; Modelo 3')
  plot(N,p3,main=title)
  title <- bquote('p value; Modelo 1 Alternativo')
  plot(N,p1a,main=title)
  title <- bquote('p value; Modelo 2 Alternativo')
  plot(N,p2a,main=title)
  title <- bquote('p value; Modelo 3 Alternativo')
  plot(N,p3a,main=title)
  title <- bquote('Eficiencia relativa; Modelo 1')
  plot(N,Efrel1,main=title)
  title <- bquote('Eficiencia relativa; Modelo 2')
  plot(N,Efrel2,main=title)
  title <- bquote('Eficiencia relativa; Modelo 3')
  plot(N,Efrel3,main=title)
  title <- bquote('Error; Modelo 1')
  plot(N,E1,main=title)
  title <- bquote('Error; Modelo 2')
  plot(N,E2,main=title)
  title <- bquote('Error; Modelo 3')
  plot(N,E3,main=title)
  title <- bquote('Error; Modelo 1 Alternativo')
  plot(N,E1a,main=title)
  title <- bquote('Error; Modelo 2 Alternativo')
  plot(N,E2a,main=title)
  title <- bquote('Error; Modelo 3 Alternativo')
  plot(N,E3a,main=title)
}

run()

