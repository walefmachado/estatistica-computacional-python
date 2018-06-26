
# Exercícios - Aula 2 - Walef Machado de Mendonça

rm(list = ls())

# 1 -----------------------------------------------------------------------

# a)
media1 = function(vet){
  soma = 0
  for (i in 1:length(vet)) {
    soma = soma + vet[i]
  }
  m = soma/(length(vet))
  return(m)
}

v = c(1,2,3,4,5,6,7,8,9,10)
media1(v)

# b)
media2 = function(vet){
  m = (sum(vet))/(length(vet))
  return(m)
}
media2(v)

# c)
d = rnorm(10, 5, 1)
media1(d)
media2(d)
mean(d) # para verificar os resultados

# 2 -----------------------------------------------------------------------

testet = function(x, mu0 = 0){
  n = length(x)
  S = sd(x)
  Xb = mean(x)
  tc = (Xb - mu0)/(S/sqrt(n))
  gl = n - 1
  valorp = 2*(1-pt(abs(tc), gl)) # pt: probabilidade da distribuição t 
  list(tc=tc, media=Xb, valorp=valorp, gl=gl)
}
testet(d,5)

# 3 -----------------------------------------------------------------------

testet = function(x, mu0=0, tipo ="bilateral"){
  n = length(x)
  S = sd(x)
  Xb = mean(x)
  tc = (Xb - mu0)/(S/sqrt(n))
  gl = n - 1
  if(tipo=="bilateral"){
    valorp = 2*(1-pt(abs(tc), gl))
  }
  if(tipo=="maior"){
    valorp = (pt(abs(tc), gl))
  }
  if(tipo=="menor"){
    valorp = (pt(abs(tc), gl)-1)
  }
  list(tc=tc, media=Xb, valorp=valorp, gl=gl)
}
testet(d,5, tipo = "maior")

w = rnorm(10, 3, 1)
tc1 = testet(w, mu0=5, tipo="maior") # unilateral dir., mu0 = 5
tc1
t.test(w, mu=5, alternative="g") # função do R

tc2 = testet(w, mu0=5, tipo="menor") # unilateral esq., mu0 = 5
tc2
t.test(w, mu=5, alternative="l")

tc3 = testet(w, mu0=5, tipo="bilateral") # bilateral
tc3
t.test(w, mu=5)

# 4 -----------------------------------------------------------------------

imrs_mg = read.csv("DadosConsulta.csv", sep = ";", dec = ",", h = T)
save(imrs_mg, file = "imrs_mg.Rdata")
load("imrs_mg.Rdata")

# Fim :D