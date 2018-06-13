# Aula 7 - Estatística Computacional - Geração de v. a.s multivariadas
rm(list = ls())


# Fatorações --------------------------------------------------------------

# Cholesky
LT = chol(Sigma)
LT
t(LT) %*% LT 

# Decomposição espectral 
E = eigen(Sigma)
E 
P = E$vectors # autovetores
P 
Lambda = diag(E$values)
Lambda # autovalores
P %*% Lambda %*% t(P) # produto que é a fatoração o resultado é a propria matriz Sigma 

# Teorema do valor singular
SVD = svd(Sigma)
SVD
d = diag(SVD$d) # d retorna as raízes dos autovalores de AA 
U = SVD$u
V = SVD$v
U %*% d %*% t(V)


# verificando os autovalores de AAT
SST = Sigma %*% t(Sigma)
ESST = eigen(SST)$values # autovalores de AAT
diag(sqrt(ESST))


# Funções -----------------------------------------------------------------

# Função para gerar n v.a.s da normal p-variada
# com vetor de médias mu e matriz de covariâncias
# Sigma
rnormmv1 = function(n, mu, Sigma){
  p = nrow(Sigma)
  ev = eigen(Sigma, symmetric = TRUE)
  lambda = ev$values
  P = ev$vectors
  Sigmaroot = P %*% diag(sqrt(lambda)) %*% t(P)
  Z = rnorm(p,0,1)
  X = t(Sigmaroot %*% Z + mu)
  if(n > 1){
    for(ii in 2:n){
      Z = rnorm(p,0,1)
      Y = Sigmaroot %*% Z + mu
      X = rbind(X, t(Y))
    }   # for
  }  # if
  X  # matriz nxp dos dados
}  #rnormmv1


# Exemplo de utilização 
Sigma = (Sigma = matrix(c(4, 1 ,1, 1 ),2,2))
mu  = c(10, 50)
n = 2500
X = rnormmv1(n, mu, Sigma)
X

Xb = apply(X, 2, mean) # aplica a função mean às colunas de X
Xb
var(X)
plot(X, xlab = 'X1', ylab = 'X2', pch = 20)

# sem correlação 
p = 2
Sigma = diag(p)

# sem correlação e médias iguais a 0
mu = numeric(p)
mu
X = rnormmv1(n, mu, Sigma)
mu  = c(10, 50)
n = 2500
X = rnormmv1(n, mu, Sigma)
X

Xb = apply(X, 2, mean) # aplica a função mean às colunas de X
Xb
var(X)
plot(X, xlab = 'X1', ylab = 'X2', pch = 20)

# versão otimizada - sem loops
rnormmv2 = function(n, mu, Sigma){
  p = nrow(Sigma)
  ev = eigen(Sigma, symmetric = TRUE)
  lambda = ev$values
  P = ev$vectors
  Sigmaroot = P %*% diag(sqrt(lambda)) %*% t(P)
  X = (matrix(rnorm(n * p), n, p) %*% Sigmaroot) + 
    matrix(rep(mu, each = n), n, p)
  X # matriz n x p dos dados
} # rnormmv2

# exemplo de uso
(Sigma = matrix(c(4, 1.9 ,1.9, 1 ),2,2))
(mu = c(10,50))
n = 2500
(X = rnormmv2(n, mu, Sigma))
(Xb = apply(X, 2, mean))
(S = var(X))
plot(X, xlab = 'X1', ylab = 'X2', pch = 20)


# Comparação dos tempos ---------------------------------------------------

n = 15000
# função com loops
tg1 = system.time(rnormmv1(n, mu, Sigma))
tg1
# funcção sem loops
tg2 = system.time(rnormmv2(n, mu, Sigma))
tg2
# razão entre os tempos gastos pelas duas funções
tg1/tg2

# Funções prontas do R ----------------------------------------------------

Sigma = (Sigma = matrix(c(4, 1.9 , 1.9, 1 ),2,2))
mu  = c(10, 50); n = 30
library(MASS)
X = mvrnorm(n, mu, Sigma)
Xb = apply(X, 2, mean)
Xb # média de X
var(X) # covariância de X

library(mvtnorm)
Y = rmvnorm(n, mu, Sigma)
Yb = apply(Y, 2, mean)
Yb # média de Y
var(Y) # covariância de X

# Comparação de tempos ----------------------------------------------------

n = 30000

# função rnormmv1 com loops
tg1 = system.time(rnormmv1(n, mu, Sigma))
tg1

# funcção rnormmv2 sem loops
tg2 = system.time(rnormmv2(n, mu, Sigma))
tg2

# razão entre os tempos gastos pelas duas funções
tg1/tg2

# função mvrnorm
tg3 = system.time(mvrnorm(n, mu, Sigma))
tg3

# função rmvnorm
tg4 = system.time(rmvnorm(n, mu, Sigma))
tg4

