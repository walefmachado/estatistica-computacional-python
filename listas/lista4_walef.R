
# Lista 4 - Walef Machado de Mendonça
rm(list = ls())


# 1 -----------------------------------------------------------------------

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

# 2 -----------------------------------------------------------------------

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

# a) 
gexp = function(n,l){
  x = (-log(gna0(n)))/l
  return(x)
}

# b)
n = 10000
l = 3
a = gexp(n,l)
a

# c)
hist(a)

# d)
mean(a)
l**(-1)

var(a)
l**(-2)

sd(a)
sqrt(l**(-2))
