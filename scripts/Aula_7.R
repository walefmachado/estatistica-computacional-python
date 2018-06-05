# Aula 7 - Estatística Computacional
rm(list = ls())

set.seed(10)
x = runif(100)
y = x / 1000

h = hist(x, probability = T, breaks = 4)
h

area = sum((h$breaks[2] - h$breaks[1]) * h$density) # comferindo a somo da densidade do histograma

# Uma uma função que recebe uma amostra e retorna o histograma e a área do histograma

verifica = function(a){
  h = hist(a, probability = T)
  area = sum((h$breaks[2] - h$breaks[1]) * h$density)
  h
  list(hist=h, area=area)
}

verifica(rnorm(100))

# Outras transformações Beta
n = 1000; a = 3; b = 2
lambda = 1
u = rgamma(n,shape = a, rate = lambda)
v = rgamma(n, shape = b, rate = lambda)
x = u/(u+v)
x
hist(x, probability = T)
hist(rbeta(n, a, b), probability = T)

# Densidade obtida e a amostra gerada pelo R - rbeta
lines(density(x), col = "blue")
lines(density(rbeta(n, a, b)), col = "red")

qqplot()
# QQ plot da beta(3,2)
qqplot((qbeta(ppoints(n), a, b)), x,
        cex = 0.25, main = 'QQ plot da beta(3,2)', xlab = 'quantis teóricos', ylab = 'quantis amostrais')

qqline(x, distribution = function(p) qbeta(p,a,b)) #  A função só define os quantis 

# ppoints(n): funçãodo R que calcula o vetor de n posiões
# p[i] = (i - 3/8)/(n + 1/4)

n = 10
i = 1:n
ppoints(n)
# o mesmo que 
(i - 3/8)/(n+1/4)


# Exemplo 7

n <- 1000
theta <- 0.5
u <- runif(n)
v <- runif(n)
x <- floor(1 + log(v)/log(1 - (1- theta) ** u))
hist(x, probability = T, breaks = seq(0.5, max(x) + 0.5))
hist(x, probability = T)

# Exemplo 7 
rlogaritmica - function(n, theta){
  stopifnot(theta < 0 &  theta < 1)
  u = runif(n)
  v = floor(1 + log(v)/log(1 - (1 - theta)**u))
  return(x)
}

x = rlogaritmica(1000, theta)

# Exemplo 8

n = 1000
nu = 2 
Z = matrix(rnorm(n*nu), n, nu) ** 2 # matriz de N(0,1)
v = rowSums(Z) # soma das linhas - método 1
v = apply(Z, 1, sum) # soma das linhas - método 

# Conferindo
mean(v) # E(v)=nu
mean(v ** 2) # E(V**2)=2nu + nu **2
var(x)

# Misturas 

# Exemplo 10

n = 1000
r = 2; s = 4; lambda = 2
x1 = rgamma(n, r, lambda) # gama(2,2)
x2 = rgamma(n, s, lambda) # gama(4,2)
s = x1 + x2
u = runif(n)
k = sample(0:1, n, replace = T)
x = k * x1 + (1-k) * x2 # A mistura

par(mfrow=c(1,2))
hist(s, probability = T)
hist(x, probability = T)

# gama(r, lambda) + gama(s, lambda) = gama(r+s, lamda)
# densidade obtida de s e a amostragerada pelo R
par(mfrow=c(1,1)) # voltar a ser 1 gráfico
plot(density(s), col = 'blue')
lines(density(rgamma(n, r + s, lambda)), col = 'red')

# Exercício

# a)
n = 1000
r = 2; s = 4; lambda = 2
k = sample(0:1, n, replace = T)
x1 = rnorm(n, 0, 1)
x2 = rnorm(n, 3, 1)
x = k * x1 + (1-k) * x2 # A mistura
s = x1 + x2

# b)
par(mfrow=c(2,2))
hist(x, probability = T)
lines(density(x), col = "blue")
qqnorm(x)
qqline(x, col = "blue")
hist(s, probability = T)
lines(density(s), col = "blue")
qqnorm(s)
qqline(s, col = "blue")

# c)
mean(s)
var(s)

# d)
par(mfrow=c(1,2))
hist(s, probability = T)
lines(density(s), col = "blue")
hist((rnorm(n)), probability = T)
lines(density(rnorm(n)), col = "blue")














