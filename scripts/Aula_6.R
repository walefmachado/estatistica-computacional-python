# Aula 6 - Estatística Computacional
rm(list = ls())

# Método da transformação inversa -----------------------------------------

n = 1000
u = runif(n) # geração do vetor u 
x = u ** (1/3) # transformação
# histograma da amostra
hist(x, pro=TRUE, main=expression((f(x) == lambda* exp(-lambda*x))))
lines(density((x)))

# Para gerar o grafico da finção teórica
y = seq(0,1 , .01)
# curva teórica 
lines(y,3 * y ** 2, col = "red")

# Para a distribuição exponencial

n = 1000
lambda = 2 
u = runif(n) # geração do vetor u 
x = (-1/lambda)*log(u) # transformação
# histograma da amostra
hist(x, pro=TRUE, main=expression((f(x) == lambda* exp(-lambda*x))))
lines(density((x)))

# Para gerar o grafico da finção teórica
y = seq(0,4,0.01) # curva teórica 
lines(y,lambda* exp(-lambda*y), col = "red")

rexpon = function(n, lamb){
  u = runif(n) # geração do vetor u 
  x = (-1/lamb)*log(u) # transformação
  return(x)
}

exp1 = rexp(100,2)
hist(exp1, prob = T)
lines(density(exp1))

lamb = 5
exp2 = rexp(n, lamb)
hist(exp2, prob = T)
lines(density(exp1))


# Exemplo 3

n = 10
p = 0.4
u = runif(n)
u > 0.6 # visualizar o vetor lógico
x = as.integer(u < 0.6)
x
mean(x) # teeórico: mu = p = 0,4
var(x) # teórico: sigma**2 = p(1-p)=0,4*0,6 = 0,24

hist(x, breaks = c(-0.5, 0.5, 1.5), probability = TRUE)
# para gerar o gráfico da função do R
y = rbinom(n, size = 1, prob = p)
hist(y, breaks = c(-0.5, 0.5, 1.5), probability = TRUE)
# amostra maior
n = 1000

# Exemplo 3 
n = 1000
p = c(0.2, 0.3, 0.1, 0.05, 0.35)
Fx = cumsum(p)
u = runif(n)
x = 1:n
for (i in 1:n) {
  indice = 1
  while(u[i] > Fx[indice]){
    indice = indice +1
  }
  x[i] = indice
}
hist(x, breaks = seq(0.5, 5.5))
hist(x, breaks = seq(0.5, 5.5), probability = TRUE)

# Método da aceitação/rejeição 

# Ex 5

y = seq(0,1, length=100)
plot(y, 6*y*(1-y), type = 'l')

# ou usando a própria densidade beta do R
curve(dbeta(x, 2, 2), col='red')
# ponto maximo é considerado como a constante c

# gerar n observações da B(2,2) usando o metódo da rejeição
# função g é a U(0,1) e c = 1,5
n = 1000
k = 0 # contador para aceitação
j = 0 # número de interações
x = numeric(n)
while(k < n){
  u = runif(1)
  j = j + 1
  y = runif(1) # v. a. de g
  if(u <= 4*y*(1-y)) { # aceitamos y
    k = k +1
    x[k] = y
  }
}

j # interações necessárias para n
hist(x, probability = TRUE)
lines(density(x), col= "blue")
lines(density(rbeta(n,2,2)), col= "red")

# comparar quantis empíricos e teóricoas 
p = seq(.1, .9, .1)
Qhat = quantile(x, p)
Q = qbeta(p, 2, 2)
round(rbind(Qhat, Q), 3)

# incluir o erro padrão da estimativa 
ep = sqrt(p * (1-p)) / (sqrt(n) * dbeta(Q, 2, 2))
round(rbind(Qhat, Q, ep), 3)


# Exercício 

n = 10000
a = 2
b = 2
u = rexp(n,b)
x = c()
gama = function(n, a, b){
  for (i in 1:n) {
    x[i] = -(1/b)* log(prod(runif(a)))
  }
  return(x)
}

gama_e = function(n, b){
  for (i in 1:n) {
    x[i] = sum(rexp(b))
  }
  return(x)
}


x = gama(n, a, b)
ks.test(x, "pgamma", 2, 2)
mean(x)
var(x)


x = gama_e(n, b)
ks.test(x, "pgamma", 2, 2)

hist(x, probability = TRUE)
lines(density(x), col= "blue")

lines(density(rbeta(n,2,2)), col= "red")




