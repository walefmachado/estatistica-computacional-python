# Aula 11 - Estatística Computacional
rm(list = ls())

install.packages('bootstrap')
install.packages('ISLR')

library(bootstrap)
library(boot)
library(ISLR)



# Exemplo 1 
x = c(8.26, 6.33, 10.4, 5.27, 5.35, 5.61, 6.12, 6.19, 5.2,
      7.01, 8.74, 7.78, 7.02, 6, 6.5, 5.8, 5.12, 7.41, 6.52,
      6.21, 12.28, 5.6, 5.38, 6.6, 8.74)

# Estatística escolhida: CV (theta) - função pra calculá-la
CV = function(x) sqrt(var(x))/mean(x) * 100

# Estimativa (valor de theta chapéu)
CV(x)

# Gerar apenas uma amostra bootstrap
bb = sample(x, replace = T)

# Obter o CV usando a amostra bootstrap 
CV(bb)

# Gerar B = 1000 amostras bootstrap
B = 10000 
(boo = numeric(B))

# Gerar B amostras e calcular o CV para cada amostra
for (b in 1:B) boo[b] = CV(sample(x, replace = T))
# boot é theta chapéu* 

# obter média e variância da coleção de amostras
mean(boo); var(boo)    # estimativas bootstrap

# Histograma
hist(boo, prob = T)

# Valor dos quantis 0,975 e 0,025
quantile(boo, 0.025); quantile(boo, 0.975)

# Viés:
viesb = mean(boo) - CV(x)
(viesb = sum(boo - CV(x))/B)

# Exemplo -----------------------------------------------------------------
# Exemplo com uma população uniforme e sem a amostra (gerar amostra)

U = runif(1000)
u = sample(u, 25)
mean(U) # Estimativa populacional

# Estatística escolhida: média
# Estimativa (valor de theta chapéu)
mean(u)

# Gerar apenas uma amostra bootstrap
bb = sample(u, replace = T)

# Obter o CV usando a amostra bootstrap 
mean(bb)

# Gerar B = 1000 amostras bootstrap
B = 10000
(boo = numeric(B))

# Gerar B amostras e calcular o CV para cada amostra
for (b in 1:B) boo[b] = mean(sample(u, replace = T))
# boot é theta chapéu* 

# obter média e variância da coleção de amostras
mean(boo); var(boo)    # estimativas bootstrap

# Histograma
hist(boo, prob = T)

# Valor dos quantis 0,975 e 0,025
quantile(boo, 0.025); quantile(boo, 0.975)

# Viés:
viesb = mean(boo) - mean(u)

# Exemplo 2  --------------------------------------------------------------

data(law)
law

# Estatística escolhida: Coeficiente de correlação amostral
# estimativa (theta chapéu) para a amostra law de dimensão n = 15

(corr = cor(law$LSAT, law$GPA))

# gerar B amostras bootstrap
B = 2000; n = nrow(law); x = law
(boo = numeric(B))
# Gerar B amostras e calcular a correlação para cada amostra
for (b in 1:B){
  i = sample(1:n, size = n, replace = T) # i é o vetor dos índices
  LSAT = x$LSAT[i]
  GPA = x$GPA[i]
  boo[b] = cor(LSAT, GPA)
}
# boot

# obter média e variância da coleção de amostras
mean(boo); var(boo)    # estimativas bootstrap

# Histograma
hist(boo, prob = T)

# Valor dos quantis 0,975 e 0,025
quantile(boo, 0.025); quantile(boo, 0.975)

# Viés:
(viesb = mean(boo) - corr)

# Desvio padrão bootstrap
(SB = sd(boo))
# ou 
(SB = sqrt(sum((boo - mean(boo)) ** 2)/ (B-1)))


# Usando a função boot 
# escrever a função da correlação usando
# índice como argumento
r = function(x, j) cor(x[i, 1], x[i,2])

library(boot)
# função boot
obj = boot(data = law, statistic = r, R = 2000)
obj


# Jackkinfe ---------------------------------------------------------------

# exemplo retirar uma determinada observação
x = 1:5
for (i in 1:5){
  print(x[-i])
}
# fim exemplo 

data(patch, package = "bootstrap")
patch

n = nrow(patch)
y = patch$y
z = patch$z
# estimativa theta chapéu
theta_ch = mean(y)/mean(z)
theta_ch

# Obter as reamostragens jackknife, deixando uma observação de fora 
theta_jack = numeric(n)
for (i in 1:n){
  theta_jack[i] = mean(y[-i]) / mean(z[-i])
}
theta_jack
mean(theta_jack) # estimativa jakknife
vies = (n-1) * (mean(theta_jack) - theta_ch)
vies # estimativa jackknife do viés


# Exercícios --------------------------------------------------------------

# 1)
CV = function(x, ind){
  X = x[ind]
  return (sqrt(var(X))/mean(X)*100)
}
# uso 
CV(x, 1:length(x)) # estimativa (theta chapéu)
# obter média e variância da coleção de amostras
mean(boo); var(boo)    # estimativas bootstrap

# 2)

# Exercício em aula 
y = c(33.23, 11.81, 23.67, 19.28, 3.92, 4.01, 43.84, 0.19, 1.31, 2.20,
      1.48, 7.82, 3.04, 81.09, 11.18, 3.83, 7.46, 23.23, 22.77, 33.57,
      16.75, 2.69, 39.31, 15.72, 10.99, 33.99, 2.72, 26.31, 14.66, 63.40,
      52.51, 18.22, 11.54, 29.45, 6.27, 28.32, 12.14, 54.62, 10.24, 17.52)

lambda = 1/(sum(y)/length(y))
lambda = 1/mean(y)

# a)
# i) 
ex = rexp(40, lambda)
# ii)
(xb = mean(ex))
1/lambda

for (i in 1:1000){
  xb = numeric(1000)
  ex = rexp(40, lambda)
  xb[i] = mean(ex)  
}
xb


mdm = function(n){
  xb = matrix(0,n,3)
  for (i in 1:n) {
    X4 = rnormmv2(n, mu, Sigma)
    for (j in 1:3) {
      xb[i,j] = (apply(X4, 2, mean))[j]
    }
  }
  return(apply(xb, 2, mean))
}
