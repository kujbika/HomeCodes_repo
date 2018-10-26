############################PFM Homework####################################
install.packages("stats")
library(stats)
############ Box - M�ller methodto generate random normally distributed numbers ##############


#1. If X, Y ~ N(0,1), X iid Y, what is the distribution of R = sqrt(X^2+Y^2) ?

x0=rnorm(10000,0,1)
y0=rnorm(10000,0,1)
r0=x0^2+y0^2
plot(ecdf(r0)) #R^2 eloszlasa
exp=plot(ecdf(rexp(10000,1/2))) #R^2 negyzet eloszlasa exponencialis(khi negyzet)
r0=sqrt(r0)
plot(density(r0), main="The distribution of R", xlab="x")
plot(ecdf(r0), main="Cumulative distribution of R", xlab="x")



#2. Generate a random r from the distribution of R

#generates random number based on a given sample
rgen = function(m,samp){
  s=sort(samp)
  n=length(samp)
  a=((1:n)-0.5)/n
  x=runif(m)
  rg=rep(0,m)
  for (i in 1:m){
    if (x[i]>=(0.5/n)&x[i]<=(1-0.5/n)){
      lower=which.max(a[a<=x[i]])
      upper=lower+1
      plus=(x[i]-a[lower])*n
      rg[i]=s[lower]+plus*(s[upper]-s[lower])
    }
    else if(x[i]<(0.5/n)){
      sdown=n/(n-1)*(mean(s)-min(s)) #n végtelen, ez konvergál egyhez
      f0=mean(s)-sdown
      plus=x[i]*n #kisebb, mint 0.5. ha nulla, akkor max súlyt kap f0, ha 0,5 akkor max súlyt kap a minimum
      rg[i]=f0*(1-plus/0.5)+(plus/0.5)*min(s)
    }
    else{
      sup=n/(n-1)*(max(s)-mean(s))
      f1=mean(s)+sup
      minus=-(1-x[i])*n
      rg[i]=f1*(0.5-minus/0.5)+(minus/0.5)*max(s)
    }}  
  rg
} 
r=rgen(5000,r0) #5000 numbers based on the distribution of R
m=c(r,r0)
plot(ecdf(m))



#3. Generate fi from [0, 2Pi] uniform. 

fi=runif(5000,0,2*pi)



#4-5. Compute x = r cos(fi);  y = r sin(fi).
#Convince yourself that this algorithm indeed generates two independent standard normal.
#Make histograms for x and y. Compute correlation of x and y.

x=r*cos(fi)
y=r*sin(fi)
plot(ecdf(x)) #based on the plot it is standard normal indeed
hist(x)
plot(density(x))
plot(ecdf(y)) #based on the plot it is standard normal indeed
hist(y)
plot(density(y))
ExpVal_x=mean(x)
ExpVal_y=mean(y)
varx=var(x)
vary=var(y)
cor(x,y) #uncorrelated normal variables
###############################second method - central limit theorem####################

rg=rep(0,1000)
for (i in 1:1000){
  m=sum(runif(10000))
  m=m/10000
  m=m-0.5
  m=sqrt(10000)*m
  rg[i]=m
}
hist(rg)
plot(ecdf(rg))
var(rg)
1/12 #variance of the uniformly distributed random variable