#KGE
kge <- function(x,y, modified = T){
  r = cor(x,y,use = 'pairwise.complete')
  ux = mean(x, na.rm = T)
  uy = mean(y, na.rm = T)
  sx = sqrt(var(x, na.rm = T))
  sy = sqrt(var(y, na.rm = T))
  
  b = uy/ux
  g = sy/sx
  if(modified == T){g = (sy/uy)/(sx/ux)}
  
  t1 = (r-1)^2
  t2 = (b-1)^2
  t3 = (g-1)^2
  kge = 1-sqrt(t1+t2+t3)
  return(kge)
}

#R2
r2 <- function(pred, obs){
  ssr <- sum((obs-pred)^2, na.rm = T)
  obs_mean <- mean(obs, na.rm = T)
  sst <- sum((obs - obs_mean)^2, na.rm = T)
  
  r2 <- 1 - (ssr/sst)
  return(r2)
}

#RMSE
rmse <- function(pred, obs){
  ssr <- mean((obs-pred)^2,na.rm = T)
  return(sqrt(ssr))
}
