# Aula 2 - Estatística Computacional
rm(list = ls())

options(scipen = 999)

congruencial = function(n, m, a, c, U0){
  U = c()
  Ui = U0
  for (i in 1:n) {
    Ui = (a * Ui + c) %% m
    U[i] = Ui / m                     # Para resultados entre 0 e 1
    
  }
  return(U)
}

m = 8; a = 5; c = 1; seed = 0; n = 10
X = numeric()
X[1] = seed
Y = congruencial(n, m, a, c, seed)
X = c(X,Y)
X

m = 10; a = c = seed = 7; n = 10
X = numeric()
X[1] = seed / m
Y = congruencial(n, m, a, c, seed)
X = c(X, Y)
X





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
n = 5000
x = gna0(n,0)
x
dim(x)

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
sem = 0
n = 1
if (sem <= 0){
  t <- as.numeric(substring(Sys.time(),
                            c(1,6,9,12,15,18),c(4,7,10,13,16,19))) #relógio/sist.
  sem <- t[6]+t[5]*60+t[4]*3600
  sem <- ia * (sem %% iq) - ir * (sem %/% iq)# retirar o efeito inicial
  if (sem <= 0) sem <- sem + im
}
u <- matrix(0, n, 1) # inicia o vetor de resultados
amostra <- gnu0(sem) #chama gnu0




t <- as.numeric(substring(Sys.time(),
                          c(1,6,9,12,15,18),c(4,7,10,13,16,19))) #relógio/sist.
sem <- t[6]+t[5]*60+t[4]*3600
sem <- ia * (sem %% iq) - ir * (sem %/% iq)# retirar o efeito inicial


# uso de sementes para gerar sequências reprodutivel

set.seed(0)
runif(10)

set.seed(10)
runif(20)

# Histogramas 
n = 1000
hist(gna0(n,0))

hist(runif(n))

# Comparar os tempos dos geradores no R

n = 1000000
t1 = system.time(gna0(n))
t2 = system.time(runif(n))
t1/t2

x = 1.9822
all.equal(2, x)

# Exercícios!

# 1)

n = 100
testa_gerador = function(x){
  l = length(x)
  tab = table(cut(x, seq(0,1, by = 0.1))) / l
  v = as.vector(tab)
  r1 = all.equal(v, rep(0.1,10))
  r2 = v - rep(0.1,10)
  hist(x)
  list(prob=v, dif=r2, mean.dif=r1)
}

n = 100000000
set.seed(2)
x = runif(n)
testa_gerador(x)

testa_gerador(gna0(n))
testa_gerador(runif(n))
testa_gerador(rnorm(n))


# Função substring --------------------------------------------------------

# corta pedaços de um string ou de um vetor com string.
# o primeiro argumento é o string ou vetor com strings 
# o segundo é onde começa o corte e o segundo argumento é onde termina
a = c('d5555f0','d9595fdf0','dfdf88df')
substring(a,2,3)      # pega da segundad até a terceira posição de cada string no vetor "a"  
Sys.Date()
substring(Sys.Date(), 2, c(3,5)) # pega da segunda até a terceira e da segunda até a quinta posição do string Sys.Date()
substring(a,2, c(3,5)) # pega da segunda até a terceira do primeiro string em "a" e da segunda até a quinta posição do segundo string e assim por diante
substring(Sys.Date(), c(2,5), c(8,10)) # pega da segunda posição até a 8 e da 5 até a 10
