# Define UI for application that draw the plots to demonstrate the CLT
shinyUI(fluidPage(
      
      # Application title
      titlePanel("Central Limit Theorem - Continuous Distributions"),
      
      # Sidebar with:
      # - A dropdown menu to choose the distribution
      # - A slider to select the sample size for the CLT experiment
      sidebarLayout(
            sidebarPanel(
                  selectInput("distr",
                              "Distribution",
                              choices = c("Beta",
                                          "Chi.squared",
                                          "Exponential",
                                          "F",
                                          "Gamma",
                                          "Lognormal",
                                          "Normal",
                                          "T",
                                          "Uniform",
                                          "Weibull",
                                          "Poisson",
                                          "Binomial"),
                              selected = "Lognormal"),
                  sliderInput("nobs",
                              "Sample size",
                              min = 10,
                              max = 500,
                              step = 10,
                              value = 30,
                              ticks = F,
                              animate=list(interval=500,loop=F))
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                  plotOutput("distrPlot"),
                  plotOutput("meansPlot"),
                  tableOutput('contents')
            )
      )
))