# Function to plot the original distribution
distrplot<-function(dname,basefunc,nobs,...){
    # Detect if the distribution is discrete.
    # For now we only cover Poisson and Binomial.
    discrete <- basefunc == "pois" || basefunc == "binom"
    
    # Set graphical parameters
    par(las=1,
        tck=.02,
        bg="#EFEFEF")
    
    # Get samples (do a little truncation at the edges to prevent
    # some horrible plot distortions)
    x<-do.call(paste0("r",basefunc),
               list(n=nobs,...))
    x<-pmax(do.call(paste0("q",basefunc),
                    list(p=c(0.01),...)),
            pmin(do.call(paste0("q",basefunc),
                         list(p=c(0.99),...)),
                 x))
    
    # Get histogram
    h <- if (discrete) {
        # If discrete, number of breaks should be exactly the range.
        hist(x,
            breaks = max(x) - min(x),
            freq=T)
    } else {
        hist(x,
            breaks = 25,
            freq=T)
    }
    if (discrete) {
        # If discrete, adjust the breaks for prettier plot.
        h$breaks <- h$breaks-0.5
    }
    
    h$counts<-h$counts/nobs
    k<-(h$counts/h$density)[1]
    
    # Define plot limits and the points for the theoretical line
    xlim<-range(pretty(
        do.call(paste0("q",basefunc),
                list(p=c(0.01,0.99),...))))
    
    # Generate x-values to plot the distribution curve.
    xfit <- if (discrete) {
        # If discrete, should get an integer sequence.
        seq(xlim[1],
            xlim[2],
            by = 1)
    } else {
        seq(xlim[1]+0.01,
            xlim[2]-0.01,
            length.out = 500)
    }
    
    yfit<-k*do.call(paste0("d",basefunc),
                    list(x=xfit,...))
    
    ylim=range(pretty(c(0,1.2*max(yfit))))
    
    # Plot the histogram
    plot(h,
         col = 'cornsilk3', border = 'white',
         xlim = xlim, ylim = ylim,
         main = "", xlab = "", ylab = "",
         axes=F)
    
    # Plot distribution line
    lines(xfit,yfit,
          type = "l",
          col = "cornsilk4",
          lwd=2, 
          lty=1)
    if (discrete) {
        # If discrete, add points as well.
        points(xfit,yfit,
               col = "black",
               pch = 19)
    }
    # Plot Decorations
    box(which="outer")
    # If discrete, we plot all points in the x-axis,
    # if not, get pretty values.
    axis(1,at=if(discrete) xfit else pretty(xlim))
    axis(2,at=seq(ylim[1],ylim[2],length.out = 5),labels=F)
    legend("topright",
           legend="Theoretical distribution",
           lty=1,lwd=2,col="black")
    title(main=paste("Distribution of samples of",dname,"\n(n =",nobs,")"),
          xlab="x",
          ylab="Frequency")
}

# Function to plot the sampling distribution of means
meansplot<-function(dname,basefunc,nobs,...){
    # Set graphical parameters
    par(las=1,
        tck=.02,
        bg="#EFEFEF")
    
    # Get 999 means of samples of size "nobs"
    x<-colMeans(matrix(do.call(paste0("r",basefunc),
                               list(n=999*nobs,...)),
                       ncol=999))
    
    # Get histogram
    h<-hist(x,
            breaks = 25,
            freq=T)
    
    h$counts<-h$counts/nobs
    k<-(h$counts/h$density)[1]
    
    # Define plot limits and the points for the Gaussian line
    xlim<-range(pretty(
        do.call(paste0("q",basefunc),
                list(p=c(0.01,0.99),...))))
    
    xfit<-seq(xlim[1]+0.01,
              xlim[2]-0.01,
              length.out = 500)
    
    yfit<-k*dnorm(xfit,mean=mean(x),sd=sd(x))
    
    ylim=range(pretty(c(0,1.2*max(yfit))))
    
    # Plot histogram
    plot(h,
         col = 'cornsilk3', border = 'cornsilk4',
         xlim = xlim, ylim = ylim,
         main = "", xlab = "", ylab = "",
         axes=F)
    
    # Plot Gaussian fit
    lines(xfit,yfit,
          type = "l",
          col = "black",
          lwd=3, 
          lty=1)
    
    # Plot decorations
    box(which="outer")
    axis(1,at=pretty(xlim))
    axis(2,at=seq(ylim[1],ylim[2],length.out = 5),labels=F)
    legend("topright",
           legend=c("Best fit",
                    paste0("mu = ",signif(mean(x),3)),
                    paste0("sd = ",signif(sd(x),3))),
           lty=1,lwd=2,
           col=c("black","transparent","transparent"))
    title(main=paste("Distribution of sample means of",dname,"\n for n =",nobs),
          xlab="mean(x)",
          ylab="Frequency")
}

# Define server logic required to draw the two plots to demonstrate the CLT for 
# continuous distributions
shinyServer(function(input, output) {
    output$distrPlot <- renderPlot({
        switch(input$distr,
               Beta             =distrplot(dname="Beta(sh1=0.5,sh2=0.5)",
                                           basefunc="beta",
                                           nobs=input$nobs,
                                           shape1=0.5,
                                           shape2=0.5),
               
               Chi.squared      =distrplot(dname="Chi-squared(df=2)",
                                           basefunc="chisq",
                                           nobs=input$nobs,
                                           df=2),
               
               Exponential      =distrplot(dname="Exponential(rate=1)",
                                           basefunc="exp",
                                           nobs=input$nobs),
               
               F                =distrplot(dname="F(df1=5,df2=10)",
                                           basefunc="f",
                                           nobs=input$nobs,
                                           df1=5,
                                           df2=10),
               
               Gamma            =distrplot(dname="Gamma(shape=1,rate=1)",
                                           basefunc="gamma",
                                           nobs=input$nobs,
                                           shape=1),
               
               Lognormal        =distrplot(dname="Lognormal(mulog=0,sdlog=1)",
                                           basefunc="lnorm",
                                           nobs=input$nobs),
               
               Normal           =distrplot(dname="Normal(mu=0,sd=1)",
                                           basefunc="norm",
                                           nobs=input$nobs),
               
               T                =distrplot(dname="T(df=5)",
                                           basefunc="t",
                                           nobs=input$nobs,
                                           df=5),
               
               Uniform          =distrplot(dname="Uniform(min=0,max=1)",
                                           basefunc="unif",
                                           nobs=input$nobs),
               
               Weibull          =distrplot(dname="Weibull(shape=1,scale=0.5)",
                                           basefunc="weibull",
                                           nobs=input$nobs,
                                           shape=1,
                                           scale=1.5),
               
               Poisson          =distrplot(dname="Poisson(lambda=3.5)",
                                           basefunc="pois",
                                           nobs=input$nobs,
                                           lambda=3.5),
               
               Binomial          =distrplot(dname="Binomial(size=10,prob=0.7)",
                                            basefunc="binom",
                                            nobs=input$nobs,
                                            size=10,
                                            prob=0.7))
    })
    
    output$meansPlot <- renderPlot({
        switch(input$distr,
               Beta             =meansplot(dname="Beta(sh1=0.5,sh2=0.5)",
                                           basefunc="beta",
                                           nobs=input$nobs,
                                           shape1=0.5,
                                           shape2=0.5),
               
               Chi.squared      =meansplot(dname="Chi-squared(df=2)",
                                           basefunc="chisq",
                                           nobs=input$nobs,
                                           df=2),
               
               Exponential      =meansplot(dname="Exponential(rate=1)",
                                           basefunc="exp",
                                           nobs=input$nobs),
               
               F                =meansplot(dname="F(df1=5,df2=10)",
                                           basefunc="f",
                                           nobs=input$nobs,
                                           df1=5,
                                           df2=10),
               
               Gamma            =meansplot(dname="Gamma(shape=1,rate=1)",
                                           basefunc="gamma",
                                           nobs=input$nobs,
                                           shape=1),
               
               Lognormal        =meansplot(dname="Lognormal(mulog=0,sdlog=1)",
                                           basefunc="lnorm",
                                           nobs=input$nobs),
               
               Normal           =meansplot(dname="Normal(mu=0,sd=1)",
                                           basefunc="norm",
                                           nobs=input$nobs),
               
               T                =meansplot(dname="T(df=5)",
                                           basefunc="t",
                                           nobs=input$nobs,
                                           df=5),
               
               Uniform          =meansplot(dname="Uniform(min=0,max=1)",
                                           basefunc="unif",
                                           nobs=input$nobs),
               
               Weibull          =meansplot(dname="Weibull(shape=1,scale=0.5)",
                                           basefunc="weibull",
                                           nobs=input$nobs,
                                           shape=1,
                                           scale=1.5),
               
               Poisson          =meansplot(dname="Poisson(lambda=3)",
                                           basefunc="pois",
                                           nobs=input$nobs,
                                           lambda=3.5),
               
               Binomial          =meansplot(dname="Binomial(size=10,prob=0.7)",
                                           basefunc="binom",
                                           nobs=input$nobs,
                                           size=10,
                                           prob=0.7))
    })
    
})