# Avaliação 1 - Walef Machado de Mendonça
rm(list = ls())

# 1 -----------------------------------------------------------------------

# a) 

gera_pareto = function(n, a, b){      # Função para gerar valores de uma distribuição Pareto
  gna0 <- function(n, sem=0){
    gnu0 <- function(sem){  # função local
      k <- sem %/% iq # divisão de inteiros
      # calculando (ia * sem mod im) sem provocar overflow- Schrage
      sem = ia * (sem %% iq) - ir * k
      if (sem < 0) sem <- sem+im
      ran0 <- am * sem # converte sem para ponto flutuante
      return(list(ran0 = ran0, sem = sem))
    }
    ia <- 16807; im <- 2147483647; am <- 1.0/im
    iq <- 127773; ir <- 2836
    if (sem <= 0){
      t <- as.numeric(substring(Sys.time(),
                                c(1,6,9,12,15,18),c(4,7,10,13,16,19))) #relógio/sist.
      sem <- t[6]+t[5]*60+t[4]*3600
      sem <- ia * (sem %% iq) - ir * (sem %/% iq)# retirar o efeito inicial
      if (sem <= 0) sem <- sem + im
    }
    u <- matrix(0, n, 1) # inicia o vetor de resultados
    amostra <- gnu0(sem) #chama gnu0
    u[1] <- amostra$ran0 # inicia o primeiro elemento
    for (i in 2:n){
      amostra <- gnu0(amostra$sem)
      u[i] <- amostra$ran0
    }
    return(u)
  }
  f_inv <- a / ((gna0(n)) ** (1 / b))
  return(f_inv)
}

# b)

n = 1000; a = 1; b = 3    # definindo o tamanho da amostra e os parâmetros alpha (a) e beta (b)
x = gera_pareto(n, a, b)  # gerando os valores da Pareto

hist(x, probability = T)  # Histograma
hist(x, pro=TRUE, main=expression(f(x, alpha, beta) == (beta * alpha ** beta)/x ** (beta+1)))
lines(density(x), col = 'blue')         # Estimação kernel

# Para gerar o grafico da função teórica
y = seq(0,10 , .01)
# curva teórica 
lines(y, (b * (a ** b))/y ** (b + 1)  , col = "red")

# c)

# média:
media_gerada = mean(x)          # média dos valores da amostra gerada pela função gera_pareto
media_pareto = (a * b)/(b - 1)  # média teórica da distribuição 

# variância:
var_gerada = var(x)                                   # variância dos valores da amostra gerada pela função gera_pareto
var_pareto = (b * (a ** 2))/((b -1) ** 2) * (b - 2)   # Variância esperada para a distribuição

# 2 -----------------------------------------------------------------------

# a) 

n = 100; m1 = 3; m2 = 4   # definindo o tamanho da amostra (n) e os graus de liberdade (m1 e m2) 

v = rchisq(n, m1)         # Gera uma amostra de tamanho n de uma chi-quadrado com m1 graus de liberdade
w = rchisq(n, m2)         # Gera uma amostra de tamanho n de uma chi-quadrado com m2 graus de liberdad

x = (v/m1)/(w/m2)         # Transformação para criar valoes de uma distribuição F com m1 e m2 graus de liberdade

# b)

hist(x, probability = T)  # Histograma
lines(density(x),  col = "blue")         # Estimação kernel

# Para gerar o grafico da função teórica

# curva teórica 
lines(density(df(n, m1, m2)), col = "red")

# c)

# média: 
media_gerada = mean(x)
media_f = m2/(m2-2)

# d) 

qqplot((qf(ppoints(n), m1, m2)), x,    # qqplot (ficou estranho :( ))
       cex = 0.25, main = 'QQ plot da f(3,4)', xlab = 'quantis teóricos', ylab = 'quantis amostrais')

# qqplot feito manualmente 
q = seq(0,1,length.out = n)
xsort = sort(x)
plot(q, xsort, col ="cadetblue3")
abline(0,1,col = "red")

# e)

gera_chi = function(n, nu){  # função para gerar n valores de uma chi-quadrado
  Z = matrix(rnorm(n*nu), n, nu) ** 2 # matriz de N(0,1)
  v = apply(Z, 1, sum) # soma das linhas - método 
  return(v)
}

v = gera_chi(n, m1)      # Gera uma amostra de tamanho n de uma chi-quadrado com m1 graus de liberdade
w = gera_chi(n, m2)      # Gera uma amostra de tamanho n de uma chi-quadrado com m2 graus de liberdade

x = (v/m1)/(w/m2)

hist(x, probability = T)  # Histograma
lines(density(x),  col = "blue")         # Estimação kernel
lines(density(rf(n = n, m1, m2)), col = "red")


ks.test(x, "pf", n = n, m1, m2)

# qqplot
q = seq(0,1,length.out = n)
xsort = sort(x)
plot(q, xsort, col ="cadetblue3")
abline(0,1,col = "red")

# 3 -----------------------------------------------------------------------

# a) 
f_inf = function(n){
  u = runif(n)  
  x = u ** (1/3)
  return(x)
}

n = 1000

x = f_inf(n)
# histograma da amostra
hist(x, pro=TRUE, main=expression(f(x) == 3*x**2 ))
lines(density((x)))

# Para gerar o grafico da finção teórica
y = seq(0,1 , .01)
# curva teórica 
lines(y,3 * y ** 2, col = "red")

# b)  
p = c(.01, 0.05, 0.1, 0.5, 0.9, 0.95, 0.99)
Quants = round(quantile(x, p),3)

Q = round(p^(1/3), 3)
# comparar quantis empíricos e teóricoas
round(rbind(Quants, Q), 3)




