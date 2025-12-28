

library(MASS)
library(tree)


set.seed(123)
n = 100
J = 55
PMSE = 0

for (k in 1:100){
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
#RT

ytrain = y[,1:5]
X1train = X1[,1:5]; X2train = X2[,1:5]; X3train = X3[,1:5]; 

data.train = data.frame(y = c(ytrain), x1 = c(t(X1train)), x2 = c(t(X2train)), x3 = c(t(X3train)))
f = tree(y~x1+x2+x3,data = data.train)
fhat = matrix(predict(f),5,n)

ytest = y[,6:55]
X1test = X1[,6:55]; X2test = X2[,6:55]; X3test = X3[,6:55]; 
data.test = data.frame(y = c(t(ytest)), x1 = c(t(X1test)), x2 = c(t(X2test)), x3 = c(t(X3test)))
tree.pred = predict(f,data.test)
PMSE[k] = mean((data.test$y  - tree.pred)^2)
}

summary(PMSE)
sd(PMSE)
PMSE


