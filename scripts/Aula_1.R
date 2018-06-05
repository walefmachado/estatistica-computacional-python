# Aula 1 - Estatística Computacional
rm(list = ls())

# Introdução ao R


# Aritmética --------------------------------------------------------------

265+6565
6595-5454
989*566
568/643
65^5
545^(1/2)

a = 5
b = 8
a+b
a-b
a^b
exp(1) # O numero dentro do parênteses é o expoente

# Variáveis ---------------------------------------------------------------

produto = 15.3 * 23.4 # salva o resultado na variável
produto # mostra variável

x = 45; y = 5; z = x/y

# outra opção
(z = x / y) # armazena e já mostra o valor 

j =   produto


# Vetores -----------------------------------------------------------------

(x = c(2, 3, 4, 5.1, 3.2))
1/x
range(x)
length(x)
(y = 1:6)
x1 = seq(from = 1, to = 10, by = 0.5)

# ou seq(1,10,0.5)
rep(x, each = 5) # repee cada elemento 5 vezes
x5 = rep(x, times=5)

x5[1] # acessa primeiro elemento de x5
x5[9] = 9
x5

seq(length=19, from=1, by=0.5) # tamanho, valor nicial, passo
seq(length=19, from=1, by=-0.5)

a = c()
a = numeric()
a = vector() 
a[5] = 5
is.vector(a) # testa se é vetor

# criando vetores

numeric(10)
integer(20)
rep(0,25)

w = c(TRUE, TRUE, FALSE, TRUE, FALSE) # vetor lógico
# ou w = c(T,T,F,T,F)
(x = c(1, 2, 3, 5 ,8, 9))
y = x > 5 # cria um vetor lógico com a resposta dos valores miores que 5
y
z = !y # z é a negação de y
z
(w = x[x > 2 & x < 6])
w1 = (x+1)[x>=3] # soma 1 aos elementos de x >=3
w1
which(x > 4) # retorna posições dos elementos que satisfazem 
x[-2] # retira elementos dos índices com -

sample(1:100, 10, replace=TRUE) # função sample - 
sample(c(2,6,5,9,3,1,7))

# Matrizes ----------------------------------------------------------------

matrix(3, 4, 5) # matriz de 0's com 3 linhas e 4 colunas

A = matrix(3, 4, 5)
B = matrix(3, 4, 5)
A * B # multiplicação de matrizes de mesmo tamanho - Produto de radamar (?)

A[1,2] # acessa a linha 1 e coluna 2 da matriz

A %*% B  
diag(1,3) # matriz de 1's com dimensão 3 x 3 

(A = matrix(0,3,2)) # valor, n linhas e n colunas
(B = matrix(c(1, 2, 3, 4), 2, 2)) # por coluna
(C = matrix(c(1, 2, 3, 4), 2, 2, byrow = T))

V = diag(A) # retorna a diagonal de A
A1 = solve(C) # inversa de A

# sistemas de equações
A = matrix(c(2, 3, 4, -2), 2, 2)
Y = c(10, 5)
X = solve(A, Y)
X

(m1 = matrix(1:6, nc=3))

# linhas 
margin.table(m1,margin = 1)

apply(m1, 1, sum)
rowSums(m1)
rowMeans(m1)

# colunas
margin.table(m1, margin = 2)
apply(m1, 2, sum)
colSums(m1)
colMeans(m1)

# solução de sistemas
mat = matrix(c(1, 5, 2, 3, -2, 1, -1, 1, -1), nc=3)
vec = c(10, 15, 7)
solve(mat, vec)

# inversa
solve(mat)

# Dataframe ---------------------------------------------------------------

d1 = data.frame(X = 1:10,
                Y = c(51, 54, 61, 67, 68, 75, 77, 75, 80, 82))
d1
names(d1)
d1$X
d1$Y
plot(d1)
plot(d1$Y, d1$X)

# acesso aos elementos 

d1[3,2]
d1[7,]
d1[,2]

x = c(50,10,45,100)
sort(x)
rank(x) # posto (posição dos valores em ordem)
order(x) # posições para o vetor ordenado 
?order


# Exercícios --------------------------------------------------------------

# 1) a)
a = c(4, 8, 2)

# b) 
a[-2]

# c)
b = c(-3, -2, -1, 0, 1, 2, 3)
b = seq(-3,3,1)

# d)
c = seq(2.4,10.4, 1)

# e)
d = seq(1,39,2)

# f)
e = seq(1,11,2)
o = seq(14,20,3)
e = c(e,o)

# g)
rep(c(1,2,3,4),each = 3)

# h) 
rep(c(4,3,2,1),each = 2)

# i) 
rep(seq(1,4,1))

# j)
seq(1,100,2)

# k)
seq(10,1,1)

# l) 
c("Paraná", "São Paulo", "Minas Gerais")

# 2)

d1 = data.frame(x = seq(0,50,1),
                x2 = x^2,
                exp = exp(x))

# 3)

# a)
z = seq(1,100,1)
d = 1/z

# b)
b = sum(c(1,1/seq(22,99,20)))
library(MASS)
fractions(b)


# c)
sum(1/((2/factorial(seq(1,100)))*2))



