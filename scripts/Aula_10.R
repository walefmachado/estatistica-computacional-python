# Aula 10 - Estatística Computacional
rm(list = ls())


# função para retornar a média, soma de desvios em relação à média ao quadrado, ao cubo e a quarta potência e variância 
medsqk = function(x){
  n = length(x)
  if (n<=1) stop('Dimensão do vetor deve ser maior que 1!')
  xb = x[1]
  W2 = 0
  W3 = 0 
  W4 = 0
  for (ii in 2:n){
    aux = x[ii] - xb
    W4 = W4 + (ii**3 + 4*ii**2 + 6*ii - 3)* aux**4/ii**3 + 6*W2*aux**2/ii**2 - 4*W3*aux/ii
    W3 = W3 + (ii**2 -3*ii + 2)*aux**3/ii**2-3*W2*aux/ii
    W2 = W2 + (ii - 1)*aux**2/ii
    xb = xb + aux/ii
  }
  S2 = W2/(n-1)
  return(list(media = xb, variância = S2, SQ2 = W2, W3 = W3, W4 = W4))
}
x = c(1,2,3,4,5,7,8)
medsqk(x)

# função para retornar o vetor de médias, a matriz de somas de
# quadrados e produtos e a matriz de covariâncias
medcov = function(x){
    n = nrow(x)
    p = ncol(x)
    if (n <= 1) stop("Dimensão linha da matriz deve ser maior que 1!")
    xb = x[1,]
    W = matrix(0, p, p)
    for (ii in 2:n){
      aux = x[ii,] - xb
      W = W + (ii - 1) * aux %*% t(aux) / ii
      xb = xb + aux / ii
    }
    S = W / (n - 1)
    return(list(vetmedia = xb, covariancia = S, SQP = W))
 }
# uso
n <- 1000
p <- 5

library(mvtnorm) # Para gerarmos dados da normal multivariada
x <- rmvnorm(n,matrix(0, p, 1), diag(p)) # simular da normal pentavariada
medcov(x)

# comparar os resultados
medcov(x)$vetmedia
apply(x, 2 , mean) # Função pronta do R
medcov(x)$covariancia
var(x) # Função pronta do R 
medcov(x)$SQP
(n-1) * var(x) # Função pronta do R

# Exercício ---------------------------------------------------------------

media = function(x){
  s = 0
  for (i in 1:(length(x))){
    s = s + x[i]
  }
  m = s/length(x)
  return( m)
}

x = rnorm(1000)
media(x)
mean(x)

m_var = function(x){
  sq = 0
  sAq = 0
  for (i in 1:length(x)) {
    sq = sq + x[i]**2
    sAq = sAq + x[i]  
  }
  S2 = (sq- (sAq**2/length(x)))/(n-1)
  return(S2)
}
x = rnorm(1000)
media(x)
mean(x)
m_var(x)
var(x)

m_var2 = function(x){
  s1 = 0
  s = 0
  for (i in 1:length(x)){
    s1 = s + x[i]
  }
  m = s/length(x)
  for (i in 1:length(x)) {
    s = s + (x[i] - m)**2
  }
  S2 = s/(n-1)
  return(S2)
}
m_var2(x)
