# Chapter 4 routines
#====================

set.seed(661857)                # Set PRNG seed (for reproducibility)
x<-rnorm(n=25, mean=50, sd=2)   # Draw 25 samples from N(mu=50,sigma=2)
mean(x)                         # Get the sample mean
hist(x,col="darkgray",breaks=3) # Plot a histogram

#=====

# Generate 100 confidence intervals for samples of 25
# points from N(mu=50,sigma=2). Confidence level 0.95
set.seed(661857)    # Set PRNG seed (for reproducibility)
mu<-50              # desired mean
sd<-2               # desired standard deviation
N<-25               # Desired sample size
K<-100              # Number of CIs
alpha<-0.05         # Significance level

X<-matrix(rnorm(K*N,mean=mu,sd=sd),     # Generate a column matrix of samples
          nrow=N)                       # of size N

# Calculate upper and lower bounds for the K confidence intervals
CI_lower<-apply(X,2,mean)-apply(X,2,sd)*qt(1-alpha/2,N-1)/sqrt(N)
CI_upper<-apply(X,2,mean)+apply(X,2,sd)*qt(1-alpha/2,N-1)/sqrt(N)
isout<-(CI_lower>mu)|(CI_upper<mu)  # Flag for which CIs that do not contain the
                                    # true parameter value

## Plot the CIs
# pdf("../figs/CIs.pdf",width=12,height=8)

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
points(c(0,K),c(mu,mu),
       lty=2,lwd=3,type="l")

# dev.off()

#=====

# Enclosure plot
x<-seq(42,58,0.001)
xup<-qnorm(0.975,mean=50,sd=2)
xlow<-qnorm(0.025,mean=50,sd=2)
ytop<-dnorm(xup,mean=50,sd=2)
data<-data.frame(x=x,y=dnorm(x,50,2))
shade1 <- rbind(c(xup,0), 
                subset(data, x > xup), 
                c(data[nrow(data), "X"], 0))
shade2 <- rbind(c(xlow,0), 
                subset(data, x < xlow), 
                c(data[nrow(data), "X"], 0))
shade3 <- rbind(c(xlow,0), 
                subset(data, (x > xlow)&(x < xup)), 
                c(xup,0))

# Generate plot (using ggplot2)
library(ggplot2)
# pdf("../figs/enclosure.pdf",width=12,height=5)
qplot(x=data$x, y=data$y,geom="line", xlim=c(42,58))+
    geom_segment(aes(x=xup,y=0,xend=xup,yend=ytop))+
    geom_segment(aes(x=xlow,y=0,xend=xlow,yend=ytop))+
    geom_polygon(data = shade1, aes(x, y), fill="#FFAAAA")+
    geom_polygon(data = shade2, aes(x, y), fill="#FFAAAA")+
    geom_polygon(data = shade3, aes(x, y), fill="#AAFFAA")+
    xlab("Resistance") + ylab("Probability density") +
    ggtitle("90% enclosure of a N(50,2) population")+
    theme(text = element_text(size=20))
# dev.off()

