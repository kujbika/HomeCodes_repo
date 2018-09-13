x=data.table(x=seq(0,2.5,0.25))
u=data.table(u=rnorm(11,0,5))
data=data.table(x,u,reality=5*x^3,z=5*x^3+u)
names(data)=c("x","u","reality","z")
View(data)
p1=ggplot(data=data,aes(x=x,y=z))+geom_point()
p1
average=mean(data$z)
d0=p1+ggtitle("0 degree")+geom_hline(yintercept = average, col="blue")+annotate('text',x=0.35,y=average+2, label=paste0("y=",round(average,3)))
d0
m1=lm(data=data,z~x)
ess1=sum((m1$fitted.values-mean(data$z)))
ess1
ess_counter=function(k){
  while(k>0){
    data$q=x^k
    m1=update(m1,.~.+q, data=data)
    k=k-1
  }
  ess=sum((m1$fitted.values-mean(data$z)))
  return(summary(m1))
}
