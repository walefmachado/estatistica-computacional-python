# Aula 5 - Estatística Computacional
rm(list = ls())



x = runif(100)
x
plot(x)

# vaŕios gráficos 
par(mfrow = c(2,4))
x = runif(1000000)
plot(x)
hist(x)

# outra forma
for(i in 1:4){
  x = runif(100)
  plot(x)
  hist(x)
}

# Exercício 

# função seno
plot(sin(1:100))

plot(sin(1:100), type = "l")

n = 100 
a = round(sin(1:n),1)
a 
plot(a, type = 'l')
table(a)
hist(a)
table(cut(a, seq(-1,1, by=0.2)))/n # a função cut divide em intervalos e a table nos dá a frequencia em cada intervalo.

# QQ plot: x = quantis esperados (teóricos) e y = quantis observados (em ordem). 

x = runif(n)
xo = sort(x)
q = seq(0, 1, by = (1/n))
q = seq(0, 1, length.out = n)
plot(q,xo)


# cores no R 
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# qqplot - uniforme
n = 100
q = seq(0,1,length.out = n)
x1 = runif(n)
xsort1 = sort(x1)
plot(q, xsort1, col ="cadetblue3")
abline(0,1,col = "red")


n = 100
q = seq(0,1,length.out = n)
x2 = runif(n)
xsort2 = sort(x2)
plot(xsort1, xsort2, col ="deepskyblue4")
abline(0,1,col = "red")



# qqplot no R
qqnorm(x1)
qqline(x1)
shapiro.test(x1)


# Distribuição emprírica --------------------------------------------------

# n = 10 -> 0.1, 0.23, 0.34, ... , 0.95
# F(x_{3})=(1/10)*3 = 0.3
# n = 15 -> 0.08, 0.12, 0.13, 0.18, ... , 0.9
# F(x_{3})=(1/15)*3

n = 100
x = runif(n)
xsort = sort(x)
i = (1:n)
y = i/n
plot(xsort, y)

# versão compacta
x = runif(100)
plot(sort(x), 1 :length(x) / length(x),
main = "Função distribuição empírica \n X uniforme")

# incluindo nomes dos eixos 
# ajuda para expresões matemáticas: help(plotmath)
x = runif(100)
plot(sort(x), 1 :length(x) / length(x),
     xlab = "x", ylab = expression(F[n]),
     main = "Função distribuição empírica \n X uniforme")




# classe ecfd para funções de distribuição empíricas 
# forma automática do R 
# uniforme(0,1)
plot(ecdf(runif(30)))

# normal padrão
plot(ecdf(rnorm(30)))

#exponencial com alpha = 2
plot(ecdf(rexp(30,2)))

n = 100
# normal com mu = 0 e sigma = 5
plot(ecdf(rnorm(n, mean = 0, sd = 5)))

# beta com a = 1 e b = 3
plot(ecdf(rbeta(n, 1, 3)))

# cauchy (0,0.5)
plot(ecdf(rcauchy(n, 0, 0.5)))


x = runif(1000)
hist(x, probability = T)
rug(x) # adiciona dados originais ao histograma
lines(density(x)) # estimação kernel

histog = hist(x)
histog$counts


# Teste Kolmogorov-Smornov

# K-S para U(0,1)
x = runif(10)
max(abs((1 : length(x)) / length(x) - sort(x)))

help("ks.test")

# sequência
y = 1:100
ks.test(y, "punif", 1, 100)

# U(0,1)
z = runif(100)
ks.test(z, "punif")

# seno
w = sin(1:100)
range(w)
ks.test(w, 'punif', min(w), max(w))

# normal 
u = rnorm(100)
range(u)
ks.test(rnorm(100), 'punif', min(u), max(u))


# teste Chi quadrado

help("chisq.test")

x = c(48,52)
x
y = chisq.test(x)
y
names(y)
y$observed
y$expected

x = c(57,43)
x
y = chisq.test(x)
y
names(y)
y$observed
y$expected

# Exercícios
# b)
q = (48 - 50) ** 2 / 50 + (52 - 50) ** 2 / 50
q
gl = 1
1 - pchisq(q, gl)

# c)
nc = round(rnorm(1, 50, 8), 0)
nc
x = c(nc, 100 - nc)
x
y = chisq.test(x)
y
names(y)
y$observed
y$expected


# 2)
x = rep(30, 5)
x
y = chisq.test(x)
y
y$observed
y$expected

x = c(15,25,40,22,18)
x
y = chisq.test(x)
y
y$observed
y$expected

x = c(0,0,90,0,60)
x
y = chisq.test(x)
y
y$observed
y$expected

x = c(0,0,9,0,6)
x
y = chisq.test(x)
y
y$observed
y$expected

x = 1:100
table(x)
chisq.test(table(x), simulate.p.value = T)

h = hist(x)
h$counts
chisq.test(h$counts)

x = rnorm(100)
table(x)
chisq.test(table(x), simulate.p.value = T)

h = hist(x)
h$counts
chisq.test(h$counts)

