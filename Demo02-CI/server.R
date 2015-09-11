shinyServer(function(input, output) {   
    output$graph <- renderPlot({
        # Set random seed
        set.seed (input$seed)
        
        # set parameters
        sd      <- input$sd    # standard deviation
        N       <- input$N     # sample size
        K       <- input$K     # number of CIs
        alpha   <- input$alpha # significance level
        
        # Generate a column matrix of samples of size N
        X <- matrix(rnorm(n     = K * N,
                          mean  = 0,
                          sd    = sd),     
                    nrow = N)
        
        # Calculate upper and lower bounds for the K confidence intervals
        CIhw     <- apply(X, 2, sd) * qt(1 - alpha/2, N-1) / sqrt(N)
        CImd     <- apply(X, 2, mean)
        CI_lower <- CImd - CIhw
        CI_upper <- CImd + CIhw
        
        # Flag for which CIs that do not contain the true parameter value
        isout    <- (CI_lower > 0) | (CI_upper < 0)
        
        # Prepare plot
        plot(x    = 0, 
             y    = 0, 
             type = "n",
             xlim = c(0, K),
             ylim = c(-8, 8),
             ylab = "Values",
             las  = 1)
        
        # Plot intervals
        for (i in 1:K){
            points(x    = c(i, i),
                   y    = c(CI_lower[i], CI_upper[i]),
                   type = "l",
                   lwd  = ifelse(isout[i], 3, 2),
                   col  = ifelse(isout[i], "red", "green"))
        }
        grid(NA, NULL)
        points(x    = c(0, K),
               y    = c(0, 0),
               lty  = 3,
               lwd  = 3,
               type = "l")
    })
})