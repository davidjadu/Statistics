a<-10
n<-1000
Finv <- function(w){
  return(w^(1/a))
}
F <- function(w){
  return(w^a)
}

f<-function(w){
  return(a*w^(a-1))
}

u<-runif(n)
x<-Finv(u)
z<- seq(0.0,1.0,length=n+2)

ecdf1<-ecdf(x)
plot(z,F(z),type="l",col ="red",main='CDF')
plot(ecdf1,add=TRUE,col="blue")

h<-hist(x)
plot(h,freq=FALSE,col="blue")
lines(z,f(z),col='red')

plot(sort(x),Finv(z[2:1001]),col='blue',main='q-q plot',xlab='sorted x',ylab='teorical q')
lines(z,z,col='red')


