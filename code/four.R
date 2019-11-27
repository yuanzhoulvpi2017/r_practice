library(MASS)
library(psych)
num=5
sigma=matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),4,4)#diag(4)
B=mvrnorm(n=500,rep(0,4),sigma)
A=B[,1:3]
epsilon=0.02*B[,4]
Ik<-diag(3)
W=sqrt(500*Ik)
#Hq<-solve(W)%*%
C=t(A)%*%A
q=eigen(C,symmetric = T)
q1=q$vectors
H=q1%*%W
ft=mvrnorm(n=100,rep(0,3),diag(3))
theta=ft%*%solve(H)
cov=cor(theta)
f=mvrnorm(n=100,rep(0,3),cov)
dim(A)
dim(f)
X=f%*%t(A)+epsilon
beta11<-matrix(runif(10, min = 2, max = 5),ncol=10)
beta12<-matrix(0,nrow=1,ncol=490)
beta1<-cbind(beta11,beta12)
beta21<-matrix(runif(100, min = 2, max = 5),ncol=100)
beta22<-matrix(0,nrow=1,ncol=400)
beta2<-cbind(beta21,beta22)
beta3<-matrix(0,nrow=1,ncol=500)
e1<-matrix(rnorm(100,0,0.3),nrow=100)
e2<-matrix(rnorm(100,0,0.3),nrow=100)
e3<-matrix(rnorm(100,0,0.3),nrow=100)
y1<-X%*%t(beta1)+e1
y2<-X%*%t(beta2)+e2
y3<-X%*%t(beta3)+e3
covx<-cor(X)
fa_model2 <- fa(covx, nfactors=3,rotate = "varimax")
F=X%*%fa_model2$weights
lm1<-lm(F~y1)
scoef1<-lm1$coefficients[2,]
#mse1<-lm1$residuals
#smse1<-rbind(mse1)
lm2<-lm(F~y2)
scoef2<-lm2$coefficients[2,]
#mse2<-lm1$residuals
#smse2<-mse2
lm3<-lm(F~y3)
scoef3<-lm3$coefficients[2,]
#mse3<-lm1$residuals
#smse3<-rbind(mse3)
for(i in 1:100){
  sigma=matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),4,4)#diag(4)
  B=mvrnorm(n=500,rep(0,4),sigma)
  A=B[,1:3]
  epsilon=0.02*B[,4]
  I1<-diag(3)
  W=sqrt(500*I1)
  #Hq<-solve(W)%*%
  C=t(A)%*%A
  q=eigen(C,symmetric = T)
  q1=q$vectors
  H=q1%*%W
  ft=mvrnorm(n=100,rep(0,3),diag(3))
  theta=ft%*%solve(H)
  cov=cor(theta)
  f=mvrnorm(n=100,rep(0,3),cov)
  dim(A)
  dim(f)
  X=f%*%t(A)+epsilon
  beta11<-matrix(runif(10, min = 2, max = 5),ncol=10)
  beta12<-matrix(0,nrow=1,ncol=490)
  beta1<-cbind(beta11,beta12)
  beta21<-matrix(runif(100, min = 2, max = 5),ncol=100)
  beta22<-matrix(0,nrow=1,ncol=400)
  beta2<-cbind(beta21,beta22)
  beta3<-matrix(0,nrow=1,ncol=500)
  e1<-matrix(rnorm(100,0,0.3),nrow=100)
  e2<-matrix(rnorm(100,0,0.3),nrow=100)
  e3<-matrix(rnorm(100,0,0.3),nrow=100)
  y1<-X%*%t(beta1)+e1
  y2<-X%*%t(beta2)+e2
  y3<-X%*%t(beta3)+e3
  covx<-cor(X)
  fa_model2 <- fa(covx, nfactors=3,rotate = "varimax")
  F=X%*%fa_model2$weights
  lm1<-lm(F~y1)
  coef1=lm1$coefficients[2,]
  scoef1<-rbind(scoef1,coef1)
#  mse1<-lm1$residuals
#  smse1<-rbind(mse1)
  lm2<-lm(F~y2)
  coef2<-lm2$coefficients[2,]
  scoef2<-rbind(sscoef2,coef2)
#  mse2<-lm1$residuals
#  smse2<-rbind(mse2)
  lm3<-lm(F~y3)
  coef3<-lm3$coefficients[2,]
  scoef3<-rbind(scoef3,coef3)
#  mse3<-lm1$residuals
#  smse3<-rbind(mse3)
}
  

print(scoef1)

ii <- list()

lapply(c(1, 2, 3, 4), FUN = function(i){ii[[i]] <<- c(i*2, i*3)})
