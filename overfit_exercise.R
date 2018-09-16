setwd("C:/Users/User/Documents/GitHub/learning_repo")
source("header.R")
data_creater=function(p){
  #p=number of points
  x=seq(0,2,2/(p-1))
  d=data.table(x,z=2*x^3+rnorm(p,0,3))
  #p=ggplot(d,aes(x=x,y=z))+geom_point()+ggtitle("The raw data")
  #print(p)
  return(d)
}
ess_counter=function(k,d){
  #k is the number of degrees, d is the datatable
  data_fun=data.table(z=d$z)
  while(k>0){
  data_fun=data.table(cbind(data_fun,d$x^k))
  k=k-1
  }
  for (i in 2:ncol(data_fun)) names(data_fun)[i]=paste(ncol(data_fun)-i+1)
  m=lm(data=data_fun, z~.)
  ess=sum((m$fitted.values-data_fun$z)^2)
  ls=list(data_fun,ess)
  return(ls)
}
simulate_ess=function(m){
  #m is the number of rows of the data table
  q=data_creater(m)
  ess_sol=data.table(degree=1:m,ess=rep(0,m))
  for (i in 1:m) ess_sol$ess[i]=ess_counter(i,q)[[2]]
  return(ess_sol)
}

##lets see an example. 
t=ess_counter(5,data_creater(30))
t[[1]]
t[[2]]
simulate_ess(30)
##

q=data.table(degree=1:30)
for (i in 1:100) q=cbind(q,data.table(simulate_ess(30)[,2]))
q$mean=apply(q[,-1],1,mean)
q$sd=apply(q[,-1],1,sd)
ess_solution=data.table(degree=1:30,ess=q$mean,stand.error=q$sd)
ggplot(ess_solution,aes(x=degree,y=ess))+geom_point()+ggtitle("The ess converges to zero as the degree increases. Overfitting is detected.")
