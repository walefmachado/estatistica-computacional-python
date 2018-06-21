
# Lista 3 - Walef Machado de Mendonça
rm(list = ls())


# 1 -----------------------------------------------------------------------

# a)
regra_emp  = function(x, n){
  s1 = sum(x > (mean(x)) - (sd(x)) & x < (mean(x)) + (sd(x)))/n 
  s2 = sum(x > (mean(x)) - (2*(sd(x))) & x < (mean(x)) + (2*(sd(x))))/n
  s3 = sum(x > (mean(x)) - (3*(sd(x))) & x < (mean(x)) + (3*(sd(x))))/n
  list(sd1 = s1, sd2 = s2, sd3 =s3)
}
n = 100
regra_emp(rnorm(n), n)

n = 1000
regra_emp(rnorm(n), n)

n = 10000
regra_emp(rnorm(n), n)

# b)
n = 100
qqnorm(rnorm(n))

n = 1000
qqnorm(rnorm(n))

n = 10000
qqnorm(rnorm(n))

# 2 -----------------------------------------------------------------------

# a)
moeda = function(n){
  a = sample(c("C","K"), n, rep=T)
  b =table(a)
  c = table(a)/n
  list(amostra=a, freq_absoluta=b, freq_relativa=c)
}
moeda(100)$amostra

# b)
barplot(table(moeda(100)$amostra))

barplot(table(moeda(1000)$amostra)/1000 * 100)

# 3 -----------------------------------------------------------------------

# a)
dado = function(n){
  a = sample(1:6, n, replace = T)
  b =table(a)
  c = table(a)/n
  list(amostra=a, freq_absoluta=b, freq_relativa=c)  
}

# b)
n = 100
barplot(table(dado(n)$amostra)/n * 100)

n = 1000
barplot(table(dado(n)$amostra)/n * 100)

n = 10000000
barplot(table(dado(n)$amostra)/n * 100)

# 4 -----------------------------------------------------------------------
vetor = c(2,5,4,6,8,7,9,4,5,88,77,5,58,6,52,45)

confor = function(v, k){
  c = 0
  for(i in 1:length(v)){
    if(v[i] <= k){
      c = c + 1
    }
  }
  print(c)
}

confor(vetor, 5)

conwhile = function(v,k){
  c = 0
  i = 1
  while(i <= length(v)){
    if(v[i] <= k){
      c = c + 1
    }
    i = i + 1
  }
  print(c)
}

conwhile(vetor, 5)

conrepeat = function(v,k){
  c = 0
  i = 1
  repeat{
    if(v[i] <= k){
      c = c + 1
    }
    i = i + 1
    if(i >= length(v)) break
  }
  print(c)
}

conrepeat(vetor,5)

# 5 -----------------------------------------------------------------------

# a)
matrizA1 = function(r,c){
  A = matrix(0,r,c)
  for(i in 1:r){
    for(j in 1:c){
      if(i == j){
        A[i,j] = i * j
      }else{
        A[i,j] = i + j
      }
    }
  }
return(A)
}
matrizA1(3,3)
matrizA1(5,8)
matrizA1(1,4)

# b)
matrizA1S = function(r,c){
  A = matrix(0,r,c)
  soma = 0
  for(i in 1:r){
    for(j in 1:c){
      if(i == j){
        A[i,j] = i * j
      }else{
        A[i,j] = i + j
      }
      soma = soma + A[i,j]
    }
  }
  list(A=A, soma=soma)
}
matrizA1S(3,3)
matrizA1S(5,8)
matrizA1S(1,4)

# 6 -----------------------------------------------------------------------

f = function(x){
  fx = ifelse((0 < x & x < 1), (3 * x^2), 0)
  return(fx)
}

f(0.8)

# a)
curve(f(x), from = 0, to = 1, ylab = "y")
integrate(f, -Inf, Inf)

# b)
f(0.5)

# c)
integrate(f, 0.14, 0.71)

# d)
hist(rbeta(1000, 3, 1), 100)

# e)
f(0.5)
dbeta(0.5,3,1)  # ???? Quais parâmetros da beta???

# f) 
integrate(f, 0.14, 0.71)
integrate(pbeta, 0.14, 0.71, 3, 1)  # ????Quais parâmetros da beta???

# 7 -----------------------------------------------------------------------

# a)
normal = function(x, mean, sd){
  fx = (1/(sd * sqrt(2*pi)))*exp((-1/2)*(((x-mean)^2)/(sd^2)))
  return(fx)
}

# b)
normal(5,4,1)
dnorm(5,4,1)

# c)
integrate(normal, 4, Inf, mean=4, sd=1)
integrate(dnorm, 4, Inf, mean=4, sd=1)



