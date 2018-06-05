# Aula 2 - Estatística Computacional
rm(list = ls())
# Mais funções  

notas = read.table("/home/walef/Dropbox/Estatística Computacional/Dados/notas.txt", h=T)
notas
names(notas)
plot(notas$mat, notas$est)
hist(notas$mat)
cor(notas)

# alguns comandos vistos
max(notas$mat)
min(notas$mat)
range(notas$mat)
range(notas)

# Hstogramas 
hist(notas$mat, breaks = 10)
hist(notas$mat, breaks = 20)
hist(notas$mat, breaks = 50)

# Definindo os intervalos exatos
hist(notas$mat, breaks = c(40,60,80,100))

# Melhor
classes = seq(45,100,5)
hist(notas$mat, breaks = classes)

# Mais Oções 
hist(notas$mat, breaks = classes, main = " Gráfico muito legal", col = "red")

d = c(2,2,2,3,1)
hist(d)

# Feio! Vamos alterar os intervalos:
hist(d, breaks = seq(0.5,3.5,1))

# Melhor. Agora vamos obter as frequências relativas:
hist(d, breaks = seq(0.5,3.5,1), probability = T)

# Frequência relativa do 1 nesses dados é 1/5 = 0.2, por exemplo 

data("faithful")
help("faithful")
names(faithful)

# variável escolhida
x = faithful$eruptions

n = length(x)
k = sqrt(n) # o número de classes é a raiz de n

hist(x, k)
# queremos ver a porcentagem dos dados que está dentro de cada intervalo da regra empírica 
# Primeiro vamos 

Xb = mean(x)
S = sd(x)
sum(x > Xb - S & x < Xb + S)
sum(x > Xb - S & x < Xb + S)/n

# 2 SD
sum(x > Xb - (2*S) & x < Xb + (2*S))/n

# 3 SD
sum(x > Xb - (3*S) & x < Xb + (3*S))/n
x_ord = sort(x)
pos = round(95/100 * n)
x_ord[pos]
sum(x < x_ord[pos])
quantile(x)

regra_emp  = function(x){
  s1 = sum(x > (mean(x)) - (sd(x)) & x < (mean(x)) + (sd(x)))/n *100
  s2 = sum(x > (mean(x)) - (2*(sd(x))) & x < (mean(x)) + (2*(sd(x))))/n * 100
  s3 = sum(x > (mean(x)) - (3*(sd(x))) & x < (mean(x)) + (3*(sd(x))))/n * 100
  list(sd1 = s1, sd2 = s2, sd3 =s3)
}
+
y = regra_emp(x)
y

qqnorm(x)
qqline(x)

## Mais funções no R

# matrizA: preencher uma matriz quadrada com valores:
# i*j, se i = j
# i+j, se i!= j
matrizA = function(n){
  A = matrix(0,n,n)
  for(i in 1:n){
    for(j in 1:n){
      if(i == j){
        A[i,j] = i * j
      }else{
        A[i,j] = i + j
      }
    }
  }
  return(A)
}

matrizA(3)
matrizA(10)

# Impor uma contição
amostra = function(epsilon, n){
  i = 0
  repeat{
    i = i+1
    xb = abs(mean(rnorm(n)))
    print(xb)
    if(xb < epsilon) break
  } 
  list(media=xb, n_iteraçoes=i)
}

# Seja X ~ exp(2)

fexp2 = function(x){
  if (x < 0) {
    fx = 0
  }else{
    fx = 2 * exp(-2 * x)
  }
  return(fx)
}

fexp2(3)

fexp2 = function(x){
  fx = ifelse(x < 0, 0, 2 * exp(-2 * x))
  return(fx)
}

curve(fexp2(x), from = 0, to = 4, ylab = "y")

fexp2 = function(x, a){
  fx = ifelse(x < 0, 0, a * exp(-a * x))
  return(fx)
}

curve(fexp2(3, 2), from = 0, to = 4, ylab = "y")
fexp2(3, 2)
integrate(fexp2, -Inf, Inf, a=2)
dexp(3, 2)

integrate(dexp, -Inf, Inf, rate = 2)
integrate(fexp2, 0, 2, a=2)
integrate(fexp2, 2, Inf, a=2)


moeda = function(n){
  sample(0:1, n, rep=T)
}
e1 = moeda(30)
e1

sum(e1==0)
sum(e1==0)/30
sum(e1==1)
sum(e1==1)/30

hist(e1, c(-0.5,0.5,1.5), probability = T)

table(e1)/30*100
barplot(table(e1)/30*100)

sample(c("C","E"), 10, rep=T)
(sum((sample(c("C","E"), 10, rep=T))=="C"))/10


n = 10
m = 10000
res = matrix(0, m, 3)
colnames(res) = c("n", "C", "E")
res[,1] = n
for(i in  1:m){
  prova = (sample(c("C","E"), n, rep=T))
  res[i,2] = sum(prova=="C") # certas
  res[i,3] = sum(prova=="E") # erradas
}
res
table(res[,2])
sum(table(res[,2]))

table(res[,2])/m*100
sum(table(res[,2]))
barplot(table(res[,2])/m*100)

dbinom(0:10,10,0.5)



