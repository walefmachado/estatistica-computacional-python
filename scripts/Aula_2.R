# Aula 2 - Estatística Computacional
rm(list = ls())


# Estrutura condicional if/else -------------------------------------------

x = 1
if(x > 1){
  y = 5
}else{
  y = x
}
y

calor = F
temperatura = 35
if(temperatura > 30){
  calor = T
}
calor

media = 7
if(media >= 6){
  print('Você foi aprovado!')
}else{
  print('Você foi reprovado.')
}

# Estrutura while ---------------------------------------------------------

i = 1
while(i <= 35){
  print(i)
  i = i + 1
}

i = 0
while(i <= 10 & i >= 0){
  print(2 ** i)
  i = i + 1
}

# Estrutura Repeat --------------------------------------------------------

i = 1
repeat{
  print(i)
  i = 1 + i
  if(i > 35) break
}

# Estrutura For -----------------------------------------------------------

n = 10
x = matrix(0,n,1)
for(i in 1:n){aux=i*2+3-i;x[i]=aux}
x

for(i in 1:n){
  aux = i * 2 + 3 - i 
  x[i] = aux
}

# Funções do R ------------------------------------------------------------

?sd

dados = rnorm(1000000) # carrega 100 valores da N(0,1)
sd(dados)
mean(dados)
sd(x = dados)
sd(x = dados, na.rm = F)

# Funções -----------------------------------------------------------------

f = function(a,b){
  return(a ** b)
}
f(2,6)

# argumentos não usados
f1 = function(a,b){
  return(a ** 2)
}
f1(2,8)

# mas
f2 = function(a,b = 3){ # valor padrão em caso de parâmetro faltante
  return(a ** b)
}
f2(45,10)

resultado = function(media){
  if(media >= 6){
    return('Você foi aprovado!')
  }else{
    return('Você foi reprovado.')
  }
}
resultado(5)

soma_dados = function(n){
  k = sample(1:6, n, replace = TRUE)
  return(list(soma = sum(k), lanc = k))
}
s = soma_dados(5)

# moda 

moda = function(x){
  temp = table(x)
  return(names(temp)[temp == max(temp)])
}
x = c(2,2,2,5,5,5,6,4,1,3,8,7,5,6)
moda(x)


# Teste t para a média ----------------------------------------------------

testet = function(x, mu0 = 0){
  n = length(x)
  S = sd(x)
  Xb = mean(x)
  tc = (Xb - mu0)/(S/sqrt(n))
  gl = n - 1
  valorp = 2*(1-pt(abs(tc), gl)) # pt: probabilidade da distribuição t 
  list(tc=tc, media=Xb, valorp=valorp)
}


# a)
a = c(3.4, 5.6, 4, 6, 4.8, 9.1, 3.4, 4.5)
mean(a)
testet(a, 5)

# b)
t.test(a, mu=5, conf.level = 0.95)


# Exercicios --------------------------------------------------------------

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

getwd() # visualizar a pasta ativa
setwd("~/Dropbox/Estatística Computacional/Dados/") # definir a pasta ativa
emp = read.csv("emp_vga.csv",sep = ";",dec = ",",h = T)
emp
# visualizar tipos de dados
str(emp)
# transformar código do ibge em fator
emp$ibge7 = as.factor(emp$ibge7)
# salvar como .RData
save(emp,file="emp_vga.RData")

load("emp_vga.RData") # indicar o caminho do arquivo
emp
row.names(emp) = emp$ibge7
head(emp)
# coluna ibge7 ficou redundante - tirar
emp = emp[-1]
head(emp)
# se quisermos tirar também a coluna PBF
emp = emp[-2]
# se quisermos criar outro data frame
emp2 = emp
head(emp2)
# salvar como .RData
save(emp2,file="emp2_vga.RData")

atlas = read.csv("~/Dropbox/Estatística Computacional/Dados/atlas.csv",h = T)
atlas
# mostrar apenas 20 variáveis
head(atlas[,1:20])
# criar um dataframe com as 20 variáveis
dados = atlas[,1:20]
dim(dados)
# apenas MG
mg = subset(dados,uf == 31)
dim(mg)
head(mg)
# salvar como rdata
# mudar a pasta se necessário
setwd("~/Downloads/")
save(mg, file = "mg.Rdata")
# carregar conjunto de dados salvo
# ou definir a pasta primeiro com setwd()
load("mg.Rdata")
# ou ir em Files do RStudio

# ler
dados = read.table("nomearquivo.txt",h = T)
dados
# salvar
write.table(dados2,file = "nomearquivo2.txt")
# para arquivos .dat o procedimento é o mesmo



