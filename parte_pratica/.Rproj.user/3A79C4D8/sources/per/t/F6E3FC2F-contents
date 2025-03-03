set.seed(42)
somar_dados_finitos = function(y){
  return( sum(y[is.finite(y)]) )
}


pi = 0.9
mu1 = 0.4
mu2 = 0.3
sig1 = 0.05
sig2 = 0.02

n = 262
z = rbinom(n,1,pi)
y = z*rnorm(n,mean = mu1,sd = sig1) + (1-z)*rnorm(n,mean = mu2, sd = sig2)


plot(density(y),main="Exemplo 1")


Q = c(0,1)

k = 2

while( abs(Q[k] - Q[k-1]) >=1e-6){
  
  ## Passo E
  
  componente_1 = pi * dnorm(y, mean = mu1, sd = sig1)
  componente_2 = (1-pi) * dnorm(y, mean = mu2, sd = sig2)
  
  tao1 = componente_1 / (componente_1 + componente_2) 
  
  tao2 = componente_2 / (componente_1 + componente_2)
  
  ## Passo M
  
  pi = somar_dados_finitos(tao1) / n
  
  mu1 = somar_dados_finitos(tao1 * y) / somar_dados_finitos(tao1)
  
  sig1 = sqrt(somar_dados_finitos( ((y - mu1)^2) * tao1 ) / somar_dados_finitos(tao1))
  
  mu2 = somar_dados_finitos(tao2 * y) / somar_dados_finitos(tao2)
  
  sig2 = sqrt(somar_dados_finitos( ((y - mu2)^2) * tao2 ) / somar_dados_finitos(tao2))
  
  k = k + 1
  
  Q[k] = sum(componente_1 + componente_2)
}


library(mixtools)
gm<-normalmixEM(y,k=2,lambda=c(0.9,0.1),mu=c(0.4,0.3),sigma=c(0.05,0.02))

gm$lambda

gm$sigma

gm$mu
