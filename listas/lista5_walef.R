# Lista 5 - Walef Machado de Mendonça
rm(list = ls())
library(grid)
library(ggplot2)

# Funções -----------------------------------------------------------------

congruencial = function(n, m, a, c, U0) {
  U = c()
  Ui = U0
  for (i in 1:n) {
    Ui = (a * Ui + c) %% m
    U[i] = Ui / m
    # para resultados entre 0 e 1
  }
  return(U)
}

gna0 = function(n, sem=0){
  gnu0 = function(sem){ # função local
    k = sem %/% iq # divisao de inteiros
    # calculando (ia * sem mod im) sem provocar overflow - Schrage
    sem = ia * (sem %% iq) - ir * k
    if (sem < 0) sem = sem + im
    ran0 = am * sem # converte sem para ponto flutuante
    return(list(ran0 = ran0, sem = sem))
  }
  ia = 16807; im = 2147483647; am = 1.0 / im
  iq = 127773; ir = 2836
  if(sem <= 0){
    t = as.numeric(substring(Sys.time(),
                             c(1,6,9,12,15,18),c(4,7,10,13,16,19))) # relógio/sist.
    sem = t[6] + t[5] * 60 + t[4] * 3600
    # retirar o efeito inicial
    sem = ia * (sem %% iq) - ir * (sem %/% iq)
    if(sem <= 0) sem = sem + im
  }
  u = matrix(0, n, 1) # inicia o vetor de resultados
  amostra = gnu0(sem) # chama gnu0
  u[1] = amostra$ran0 # inicia o primeiro elemento
  for (i in 2:n){
    amostra = gnu0(amostra$sem)
    u[i] = amostra$ran0
  }
  return(u)
} 

# 1 -----------------------------------------------------------------------

n = 100; m = 2147483647; a = 16807; c = 0

t = as.numeric(substring(Sys.time(), c(1,6,9,12,15,18),c(4,7,10,13,16,19))) # relógio/sist.
U0 = t[6] + t[5] * 60 + t[4] * 3600

congruencial(n,m,a,c,U0) # Dessa forma a função congruencial sempre irá usar 
                         # o relógio do sistema criando em U0 gerando, portanto, 
                         # sempre o mesmo conjunto de numeros aleatórios.

# ou 

congruencial = function(n, m, a, c) {
  U = c()
  t = as.numeric(substring(Sys.time(), c(1,6,9,12,15,18),c(4,7,10,13,16,19))) # relógio/sist.
  U0 = t[6] + t[5] * 60 + t[4] * 3600
  Ui = U0
  for (i in 1:n) {
    Ui = (a * Ui + c) %% m
    U[i] = Ui / m
    # para resultados entre 0 e 1
  }
  return(U)
}

(x = congruencial(n,m,a,c)) # A cada vez que executada a função ira obter 
                               # uma "hora do sistema" diferente e sempre irá 
                               # gerar um conjunto de numeros aleatorios diferente.


# 2 -----------------------------------------------------------------------

n = 100 
(u = gna0(n,0))

# 3 -----------------------------------------------------------------------

par(mfrow = c(1,2))
plot(sort(x), 1:length(x)/length(x), 
     xlab = "x", ylab = expression(F[n]),
     main = "Função distribuição empírica (congruencial) \n X uniforme")
abline(0, 1)

plot(sort(u), 1:length(u)/length(u),
     xlab = "x", ylab = expression(F[n]),
     main = "Função distribuição empírica (gna0) \n X uniforme")
abline(0, 1)

# Ambas as amostras, tando a gerada pela função congruencial quanto a gerada 
# pela função gna0, seguem uma distribuição uniforme U(0,1).

# 4 -----------------------------------------------------------------------

par(mfrow = c(1,3))
plot(ecdf(runif(100)))
plot(ecdf(x))
plot(ecdf(u))

# 5 -----------------------------------------------------------------------

par(mfrow = c(1,2))
hist(x, probability = T)
rug(x) # adiciona dados originais ao histograma
lines(density(x)) # estimação kernel

hist(u, probability = T)
rug(u) # adiciona dados originais ao histograma
lines(density(u)) # estimação kernel

# Com o ggplot :D

pushViewport(viewport(layout=grid.layout(1, 2)))
a = ggplot(,aes(x)) + geom_histogram(aes(y=..density..), 
                                 binwidth = 0.1, colour="black", fill='#66CDAA') + 
  geom_density(alpha=.2)

b = ggplot(,aes(u)) + geom_histogram(aes(y=..density..), 
                                 binwidth = 0.1, colour="black", fill='#66CDAA') + 
  geom_density(alpha=.2)
print(a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(b, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# 6 -----------------------------------------------------------------------

ks.test(x, "punif", 0, 1) # O teste Kolmogorov-Smirnov apresentou um valor-p = 0.3439 
                          # o que indica que não rejeitamos a hipótese de que os dados da amostra
                          # gerada pela função congruencial tem distribuição U(0,1)

ks.test(u, "punif", 0, 1) # O teste Kolmogorov-Smirnov apresentou um valor-p = 0.1935 
                          # o que indica que não rejeitamos a hipótese de que os dados da amostra
                          # gerada pela função gna0 tem distribuição U(0,1)

# 7 -----------------------------------------------------------------------

h = hist(x)
h$counts
chisq.test(h$counts) # O teste Chi-quadrado permite concluir que x gerada pela função congruencial 
                     # segue uma distribuição U(0,1), visto que o valor-p = 0.7399 > 0.05.

h = hist(u)
h$counts
chisq.test(h$counts) # O teste Chi-quadrado permite concluir que a amostra u gerada pela função gna0
                    # segue uma distribuição U(0,1), visto que o valor-p = 0.7399 > 0.05.

# Fim :P 