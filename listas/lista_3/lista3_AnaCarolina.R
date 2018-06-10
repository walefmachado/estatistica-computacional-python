# Lista 3 - Estatística Computacional

# Ana Carolina de Oliveira

# 10/04/2018

#=======================================================

rm(list = ls())

#=======================================================

# 1)

normal = function(n){
  x = rnorm(n)
  return(x)
}

# a) 

regra_emp = function(x, tipo = "1S"){
  n = length(x)
  Xb = mean(x)
  S = sd(x)
  if(tipo == "1S"){
    prop = sum (x > Xb-S & x< Xb +S)/n
  }
  if( tipo=="2S"){
    prop = sum (x > Xb-2*S & x< Xb +2*S)/n
  }
  if( tipo=="3S"){
    prop =sum (x > Xb-3*S & x< Xb +3*S)/n
  }
  return(prop)
  
}

regra_emp(x, tipo="1S" )
regra_emp(x, tipo="2S" )
regra_emp(x, tipo="3S" )

# a regra empírica (ou a regra 68-95-99.7)

# n = 100 ( NÃO ATENDE À REGRA EMPÍRICA, MAS FICA MUITO PRÓXIMO)
x1 = normal(100)
regra_emp(x1, tipo="1S" )
regra_emp(x1, tipo="2S" )
regra_emp(x1, tipo="3S" )

# n = 1000 ( NÃO ATENDE À REGRA EMPÍRICA, MAS FICA MUITO PRÓXIMO))
x2 = normal(1000)
regra_emp(x2, tipo="1S" )
regra_emp(x2, tipo="2S" )
regra_emp(x2, tipo="3S" )

# n = 10000 ( ATENDE À REGRA EMPÍRICA)
x3 = normal(10000)
regra_emp(x3, tipo="1S" )
regra_emp(x3, tipo="2S" )
regra_emp(x3, tipo="3S" )

# b) 

qqnorm(x1)
qqnorm(x2)
qqnorm(x3)

#========================================================================================
# 2)

# a) 


moeda = function(n){
  x = sample(c("K","C"), n, rep = T)
  y = table(x)
  z = sum(x == "C")/n
  w = sum (x == "K")/n
  g = (barplot(table(x)))
  list(x=x,y=y, Cara=z, Coroa = w, Gráfico=g)

  }


 # b)

moeda(100)


moeda(1000)

#==========================================================================================

# 3)

# a)

dado = function(n){
  x = sample(c(1,2,3,4,5,6), n, rep = T)
  y = table(x)
  a1 = sum(x == 1)/n
  a2 = sum (x == 2)/n
  a3 = sum (x == 3)/n
  a4= sum (x == 4)/n
  a5 = sum (x == 5)/n
  a6 = sum (x == 6)/n
  g = (barplot(table(x)))
  list(x=x,y=y, "1"=a1, "2" = a2,"3" = a3,"4" = a4,"5" = a5,"6" = a6, Gráfico=g)
  
}
dado(100)
dado(1000)
dado(10000)

 # b)

# teoricamente, cada face tem ocorre com probabilidade 1/6 = 0,1666667. Neste sentido, 
# quanto maior 'n', a probabilidade da simulação fica próxima da probabilidade teórica. 



#=========================================================================================

# 4) Construir uma função para verificar quantos elementos de um vetor de dimensão n são menores ou
#iguais a uma constante k, real. Utilize as estruturas de repetição for, while e repeat para realizar
#tal tarefa (cada uma destas estruturas deverá ser implementada em uma diferente função).

#____________________________________________________________________________________________

#for

verifica1 = function(k,n){
  x= 1:n 
  y=0
  for( i in 1:n){
    if(x[i]<=k ){
      y=y+1
    }
  }
  return(y)
}
verifica1(20,15)
#____________________________________________________________________________________________

#  while

verifica2= function(k,n){
  x = 1:n
  y = 0
  i= 1
  while(i<=k & i<=n){
    x[i] = i
    i = i+1
    y = y+1
    
  }
  return(y)
}

verifica2(20,15)

#____________________________________________________________________________________________

# repeat
verifica3 = function(n,k){
  x = 1:n
  y = 0
  i= 1
  repeat{
    if(x[i] <= k){
      y = y + 1
    }
    i = i + 1
    if(i > n) break
  }
  print(y)
}

verifica3(20,15)

#____________________________________________________________________________________________

#=================================================================================================

# 5)

# matrizA: preencher uma matriz qualquer com valores:
# i*j, se i = j
# i+j, se i!= j

#a )
matrizA1 = function(n,k){
  A1 = matrix(0,n,k)
  for(i in 1:n){
    for(j in 1:k){
      if(i == j){
        A1[i,j] = i * j
      }else{
        A1[i,j] = i + j
      }
    }
  }
  return(A1)
}

#testar 3x3 5x8 1x4
matrizA1(3,3)

matrizA1(5,8)

matrizA1(1,4)

#____________________________________________________________________________________________

# b)

matrizA1Sum = function(n,k){
  A = matrix(0,n,k)
  soma= 0
  for(i in 1:n){
    for(j in 1:k){
      if(i == j){
        A[i,j] = i * j
      }else{
        A[i,j] = i + j
      }
      soma = soma + A[i,j] # tem que ficar dentro do for
    }
    }
  
  list(A=A, soma=soma)
}

matrizA1Sum(3,3)

matrizA1Sum(5,8)

matrizA1Sum(1,4)
#=================================================================================================

# 6)
f = function(x){
  fx = (3 * x^2)
  return(fx)
}
curve(f(x), from = 0, to = 1, ylab = "y") #  para ter uma melhor visualização da fdp

# a)
integrate(f, 0, 1)
# É uma fdp!!


# b) 
#f(0.5) VERIFICAR O QUE TA PEDINDO

f(0.5)


# c)
integrate(f, 0.14,0.71)


# d)
b = rbeta(1000,3,1)
hist(b)


# e)
dbeta(0.5,3,1)


# f)
pbeta(0.14,0.71,3,1) #conferir

#=================================================================================================

# 7)

normal = function(x, mean, sd){
  fx = (1/(sd * sqrt(2*pi)))*exp((-1/2)*(((x-mean)^2)/(sd^2)))
  return(fx)
}

# a)
normal(5,4,1)
dnorm(5,4,1)

# b)
integrate(dnorm, 4, Inf, mean = 4, sd=1)
#comparando
integrate(normal, 4, Inf, mean=4, sd=1)
#=================================================================================================






