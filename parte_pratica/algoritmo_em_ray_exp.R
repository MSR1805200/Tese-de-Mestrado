library(bayesmeta)


set.seed(42)
somar_dados_finitos = function(y){
  return( sum(y[is.finite(y)]) )
}


pi = 0.5
alfa = 0.9
mu = 0.4
n = 262
z = rbinom(n,1,pi)
y = x = z*rrayleigh(n,alfa) + (1-z)*rexp(n,mu)


plot(density(y),main="Exemplo 1")


Q = c(0,1)

k = 2

while( abs(Q[k] - Q[k-1]) >=1e-6){
  
  ## Passo E
  
  componente_1 = pi * drayleigh(x,alfa)
  componente_2 = (1-pi) * dexp(x,mu)
  
  tao1 = componente_1 / (componente_1 + componente_2) 
  
  tao2 = componente_2 / (componente_1 + componente_2)
  
  ## Passo M
  
  pi = somar_dados_finitos(tao1) / n
  
  mu_hat =  sum(y*tao2) / sum(tao2)
  
  alfa_hat = sqrt( sum(tao1 * y^2)/ sum((2 * tao1))  )
  
  k = k + 1
  
  Q[k] = sum(componente_1 + componente_2)
}
