library(MASS)
library(psych)
scoef<-matrix(0,3,3)
sigma=matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),4,4)#diag(4)
B=mvrnorm(n=500,rep(0,4),sigma)
dim(B)
A=B[,1:3]
dim(A)
epsilon=0.02*B[,4]
dim(epsilon)
I1<-diag(3)
W=sqrt(500*I1)
print(W)
#Hq<-solve(W)%*%
C=t(A)%*%A
dim(t(A))
print(C)
q=eigen(C,symmetric = T)
q1=q$vectors
H=q1%*%W
H
ft=mvrnorm(n=100,rep(0,3),diag(3))
dim(ft)
theta=ft%*%solve(H)
dim(theta)
cov=cor(theta)
dim(cov)
cov
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
beta<-rbind(beta1,beta2,beta3)
e<-matrix(rnorm(300,0,0.3),nrow=100)
y<-X%*%t(beta)+e

dim(X)
dim(e)
covx<-cor(X)
dim(covx)
fa_model2 <- fa(covx, nfactors=3,rotate = "varimax")
F=X%*%fa_model2$weights
for(j in 1:3){
  lm<-lm(F~y[,j])
  coef<-lm$coefficients[2,]
  scoef[j,]<-coef
}
print(scoef)
sscoef<-scoef
for(i in 0:5){
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
  beta<-rbind(beta1,beta2,beta3)
  e<-matrix(rnorm(300,0,0.3),nrow=100)
  y<-X%*%t(beta)+e
  covx<-cor(X)
  fa_model2 <- fa(covx, nfactors=3,rotate = "varimax")
  F=X%*%fa_model2$weights
  for(j in 1:3){
    lm<-lm(F~y[,1])
    coef<-lm$coefficients[2,]
    scoef[j,]<-coef
  }
  print(scoef)
  sscoef<-rbind(sscoef,scoef)
}

print(sscoef)
library(broom)
T=tidy(lm)
T[2,6]
