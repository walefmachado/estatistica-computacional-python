# Aula 8 - Estatística Computacional

# função BoxMüller retorna uma amostra de tamanho n de uma distribuição 
# normal com média mu e desvio padrão sigma utilizando o método de 
# Box-Müller modificado (polar)
BoxMuller = function(n, mu=0, sigma=1){
  # Polar: função que retorna dois números normais
  Polar = function(){  # função sem argumento
    repeat{
      u = runif(2, -1, 1) # 2 v. uniformes U(-1,1)
      r2 = as.numeric(u %*% u) # toma o quadrado
      if((r2 > 0) & (r2 < 1)) break
    } # fim do repeat
    ff = sqrt(-2 * log(r2) / r2)
    y = ff * u # vetor de dim 2 com var. normais padrao ind.
    y
  } # fim de polar
  if(n %% 2 == 0){ # n par
    k = n %/% 2 # pega a parte inteira da div.
    for(ki in 1:k){
      if(ki == 1) x = c(Polar()) else x = c(x, Polar())
    }  # for
  } else{ # n impar
    k = n %/% 2
    if(k == 0){ 
      x = Polar()[1]
    } else{
      for(ki in 1:k){
        if(ki == 1) x = c(Polar()) else x = c(x, Polar())
      } # for
      x = c(x, Polar()[1])
    } # else interno
  } # else n par
  x = x * sigma + mu  # transformar de N(0, 1) para N(mu, sigma)
  return(x)
} # fim de BoxMuller

# Exemplos de uso:

n = 12
(x = BoxMuller(n))
plot(density(x))
mean(x)
sd(x)
(y = BoxMuller(n, 100, 10))
plot(density(y))
mean(y)
sd(y)
par(mfrow= c(1,1))
n = 1000
w = BoxMuller(n, 100, 10)
plot(density(w))
hist(w, probability = T)


Polar = function(){  # função sem argumento
  repeat{
    u = runif(2, -1, 1) # 2 v. uniformes U(-1,1)
    r2 = as.numeric(u %*% u) # toma o quadrado
    if((r2 > 0) & (r2 < 1)) break
  } # fim do repeat
  ff = sqrt(-2 * log(r2) / r2)
  y = ff * u # vetor de dim 2 com var. normais padrao ind.
  y
}

Polar() # gera duas variáveis normais padrão. 

# Algoritmo BU - Binomial uniforme


BU = function(n,p){
  x = 0; k = 0
  repeat{
    u = runif(1)
    k = k + 1
    if(u <= p) x = x + 1
    if(k == n) break
  } # repeat
  return(x)
} # função BU

# Exemplo:
n = 1000
x = BU(n, 0.5) # n experimentos binomial prob de sucesso = 0.5
x # numero de sucessos 

for (i in 2:n) x = c(x, BU(n, 0.5))
x # numero de resultado binomial de ensaios bernoulli
par(mfrow = c(1,1))
hist(x)
mean(x) # np
var(x)  

# Algoritmo BG - Binomial Geométrica

BG = function(n, p){
  if (p > 0.5) pp = 1 - p else pp = p
  y = 0; x = 0; c = log(1-p)
  if (c < 0){
    repeat{
      u = runif(1)
      y = y + trunc(log(u)/c)+1
      if (y <= n){
        x = x + 1
      }else break
    }
  }
  return(x)
} # função BG

# Exemplo 
n = 1000
x = BU(10, 0.5)
for (i in 2:n) x = c(x, BG(10, 0.5))
x # numero de resultado binomial de ensaios bernoulli
par(mfrow = c(1,1))
hist(x)
mean(x) # np
var(x)  

# verificar o grafico da funcção ln
curve(log(x), col = 'red')


# Exercício 

# 1. Obter o QQplot com as funções qqnorm(x);qqline(x) de amostras geradas com o algoritmo
# Box-Müller usando diferentes valores de n (100, 1.000 e 10.000).

n = 100
x = BoxMuller(n)
qqnorm(x)
qqline(x)

n = 1000
x = BoxMuller(n)
qqnorm(x)
qqline(x)

n = 10000
x = BoxMuller(n)
qqnorm(x)
qqline(x)

# 2. Faça o mesmo usando a função qqplot() utilizando os argumentos corretos.

n = 100
qqplot((qnorm(ppoints(n),mean = 0, sd = 1)), BoxMuller(n),
       cex = 0.25, main = 'QQ plot da normal(0,1)', xlab = 'quantis teóricos', ylab = 'quantis amostrais')

n = 1000
qqplot((qnorm(ppoints(n),mean = 0, sd = 1)), BoxMuller(n),
       cex = 0.25, main = 'QQ plot da normal(0,1)', xlab = 'quantis teóricos', ylab = 'quantis amostrais')

n = 10000
qqplot((qnorm(ppoints(n),mean = 0, sd = 1)), BoxMuller(n),
       cex = 0.25, main = 'QQ plot da normal(0,1)', xlab = 'quantis teóricos', ylab = 'quantis amostrais')

# 3. Criar a função BoxMuller1 que retorna uma amostra de tamanho n de uma distribuição
# normal com média μ e desvio padrão σ utilizando o método de Box-Müller original (com as funções trigonométricas).

# a) Gere n = 1000 realizações de v.a.s normais usando as funções BoxMuller e BoxMuller1.
 
BoxMuller1 = function(n, mu=0, sigma=1){
  # Polar: função que retorna dois números normais
  Polar = function(){  # função sem argumento
    repeat{
      u = runif(2, -1, 1) # 2 v. uniformes U(-1,1)
      if((u[1] > 0) & (u[1] < 1) & (u[2] > 0) & (u[2] < 1)) break
    }
    y1 = sqrt(-2 * log(u[1])) * cos(2 * pi * u[2])
    y2 = sqrt(-2 * log(u[1])) * sin(2 * pi * u[2])
    y = c(y1, y2)
  } # fim de polar
  if(n %% 2 == 0){ # n par
    k = n %/% 2 # pega a parte inteira da div.
    for(ki in 1:k){
      if(ki == 1) x = c(Polar()) else x = c(x, Polar())
    }  # for
  } else{ # n impar
    k = n %/% 2
    if(k == 0){ 
      x = Polar()[1]
    } else{
      for(ki in 1:k){
        if(ki == 1) x = c(Polar()) else x = c(x, Polar())
      } # for
      x = c(x, Polar()[1])
    } # else interno
  } # else n par
  x = x * sigma + mu  # transformar de N(0, 1) para N(mu, sigma)
  return(x)
} # fim de BoxMuller


BoxMuller(100)
BoxMuller1(100)

# b) Aplique o teste de Shapiro-Wilk nas duas amostras e interprete o resultado. A função é shapiro.test(amostra).
shapiro.test(BoxMuller(100))  # valor-p maior que 0,5 então a amostra é normal
shapiro.test(BoxMuller1(100)) # valor-p maior que 0,5 então a amostra é normal

# c) Compare os tempos de execução das duas funções ao gerarem n = 10.000 realizações com:
n = 10000
system.time(BoxMuller(n))
system.time(BoxMuller1(n))
