# Lista 7 - Walef Machado de Mendonça
rm(list = ls())



media = function(x){
  s = 0
  for (i in 1:(length(x))){
    s = s + x[i]
  }
  m = s/length(x)
  return( m)
}

set.seed(1)
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
  S2 = (sq- (sAq**2/length(x)))/(length(x)-1)
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
  S2 = s/(length(x)-1)
  return(S2)
}
m_var2(x)
