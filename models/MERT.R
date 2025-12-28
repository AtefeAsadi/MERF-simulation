

library(MASS)
library(tree)


set.seed(123)
n = 100
J = 55
PMSE = 0

############ Generalized Log-Likelihood (GLL) function

library(MASS)
GLL <- function(e,b,D){
gll = 0
for (i in 1:n){
gll[i] = t(e[,i])%*%ginv(diag(5))%*%e[,i]+
         t(b[1,i])%*%ginv(D)%*%b[1,i]+log(sqrt(D))+log(det(diag(5)))
}
sum(gll)
}

for (k in 1:10){
############ Data Generating Process
sigma2b = 0.9; ro = 0; sig2 = 1; m = 0.8

  corrX = matrix(ro,J,J)
  diag(corrX) = 1
  X1 = mvrnorm(n = n, mu = rep(0, J), Sigma = corrX)
  X2 = mvrnorm(n = n, mu = rep(0, J), Sigma = corrX)
  X3 = mvrnorm(n = n, mu = rep(0, J), Sigma = corrX)

  b = rnorm(n,0,sqrt(sigma2b)) 
  e = matrix(rnorm(n*J,0,sig2),n,J)
  y = g_values = matrix(NA,n,J)

  for (i in 1:n){
   for (j in 1:J){
	 g_values[i,j] = 2 * X1[i,j] + X2[i,j]^2 + 4 * (X3[i,j] > 0) +
              2 * log(abs(X1[i,j]))* X3[i,j]
      y[i,j] = m*g_values[i,j]+b[i]+e[i,j]
    }
 }

#######################################################
####################################################### Model Fitting
#MERT

ytrain = y[,1:5]
X1train = X1[,1:5]; X2train = X2[,1:5]; X3train = X3[,1:5]; 

r = 0; q = 1
b0 = b1 = matrix(NA,q,n)
D0 = diag(q); ys = matrix(NA,5,n)
#sig2 = 1;
for (i in 1:n){
b0[,i] = rep(0,q,1)
ys[,i] = ytrain[i,]-matrix(1,5,q)*b0[,i]
}

data.train = data.frame(y = c(ys), x1 = c(t(X1train)), x2 = c(t(X2train)), x3 = c(t(X3train)))
f = tree(y~x1+x2+x3,data = data.train)
fhat = matrix(predict(f),5,n)
V0 = matrix(1,5,q)%*%D0%*%t(matrix(1,5,q))+diag(5)
e = matrix(NA,5,n); d = 0
for (i in 1:n){
b1[,i] = D0%*%t(matrix(1,5,q))%*%ginv(V0)%*%(ytrain[i,]-fhat[,i])
e[,i] = ytrain[i,]-fhat[,i]-matrix(1,5,q)%*%b1[,i]
d[i] = b1[,i]%*%t(b1[,i])+D0 -D0 %*%t(matrix(1,5,q))%*%ginv(V0)%*%matrix(1,5,q)%*%D0
}
D1 = mean(d)
gll = 0
r = r+1
gll[r] = GLL(e,b1,D1)

r = r+1
b0 = b1; D0 = D1
for (i in 1:n){
ys[,i] = ytrain[i,]-matrix(1,5,q)*b0[,i]
}
data.train = data.frame(y = c(ys), x1 = c(t(X1train)), x2 = c(t(X2train)), x3 = c(t(X3train)))
f = tree(y~x1+x2+x3,data.train )
fhat = matrix(predict(f),5,n)
V0 = matrix(1,5,q)%*%D0%*%t(matrix(1,5,q))+diag(5)
for (i in 1:n){
b1[,i] = D0%*%t(matrix(1,5,q))%*%ginv(V0)%*%(ytrain[i,]-fhat[,i])
e[,i] = ytrain[i,]-fhat[,i]-matrix(1,5,q)%*%b1[,i]
d[i] = b1[,i]%*%t(b1[,i])+D0 -D0 %*%t(matrix(1,5,q))%*%ginv(V0)%*%matrix(1,5,q)%*%D0
}
D1 = mean(d)
gll[r] = GLL(e,b1,D1)

while(abs(gll[r]-gll[r-1])>10^-2){ #| r<50
r = r+1
b0 = b1; D0 = D1
for (i in 1:n){
ys[,i] = ytrain[i,]-matrix(1,5,q)*b0[,i]
}
data.train = data.frame(y = c(ys), x1 = c(t(X1train)), x2 = c(t(X2train)), x3 = c(t(X3train)))
f = tree(y~x1+x2+x3,data.train )
fhat = matrix(predict(f),5,n)
V0 = matrix(1,5,q)%*%D0%*%t(matrix(1,5,q))+diag(5)
for (i in 1:n){
b1[,i] = D0%*%t(matrix(1,5,q))%*%ginv(V0)%*%(ytrain[i,]-fhat[,i])
e[,i] = ytrain[i,]-fhat[,i]-matrix(1,5,q)%*%b1[,i]
d[i] = b1[,i]%*%t(b1[,i])+D0 -D0 %*%t(matrix(1,5,q))%*%ginv(V0)%*%matrix(1,5,q)%*%D0
}
D1 = mean(d)
gll[r] = GLL(e,b1,D1)
}

ytest = y[,6:55]
X1test = X1[,6:55]; X2test = X2[,6:55]; X3test = X3[,6:55]; 
data.test = data.frame(y = c(t(ytest)), x1 = c(t(X1test)), x2 = c(t(X2test)), x3 = c(t(X3test)))
tree.pred = predict(f,data.test)
PMSE[k] = mean((data.test$y  - tree.pred)^2)
}

summary(PMSE)
sd(PMSE)
PMSE






