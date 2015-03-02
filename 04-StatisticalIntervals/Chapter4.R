# Chapter 4 routines

## Draw 25 samples from N(mu=50,sigma=2)
set.seed(661857)    # Set PRNG seed (for reproducibility)
x<-rnorm(n=25, mean=50, sd=2)
mean(x)
hist(x,col="darkgray",breaks=3)

## Generate 200 confidence intervals for samples of 25
## points from N(mu=50,sigma=2). Confidence level 0.95
set.seed(661857)    # Set PRNG seed (for reproducibility)
mu<-50
sd<-2
N<-25
K<-100
alpha<-0.05

X<-matrix(rnorm(K*N,mean=mu,sd=sd),nrow=N) #25x200 matrix
CI_lower<-apply(X,2,mean)-apply(X,2,sd)*qt(1-alpha/2,N-1)/sqrt(N)
CI_upper<-apply(X,2,mean)+apply(X,2,sd)*qt(1-alpha/2,N-1)/sqrt(N)
isout<-(CI_lower>mu)|(CI_upper<mu)

## Plot the CIs
#pdf("../figs/CIs.pdf",width=12,height=8)
plot(0,0,type="n",
     xlim=c(0,K),ylim=c(47,53),
     ylab="Resistance",
     xlab="Interval",
     main="Confidence Intervals",
     las=1)
for (i in 1:K){
    points(c(i,i),c(CI_lower[i],CI_upper[i]),
           type="l",lwd=ifelse(isout[i],3,2),
           col=ifelse(isout[i],"red","green"))
}
points(c(0,K),c(mu,mu),lty=2,lwd=3,type="l")
#dev.off()

