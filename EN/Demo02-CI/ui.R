shinyUI(fluidPage(
    #Title
    # titlePanel("Confidence Intervals"),
    
    # Readme
    fluidRow(
          column(12,
                 includeHTML("README.html")
          )
    ),
    hr(),
    hr(),
    #Sidebar Panel
    sidebarLayout(
        position = "right",
        sidebarPanel(
            h3("Parameters"),
            #
            # Significance Level slider
            sliderInput(
                inputId = "alpha", 
                label   = h5("Significance level (alpha)"),
                min     = 0.01,
                max     = 0.99,
                step    = 0.01,
                value   = 0.05
            ),
            #
            # Sample size slider
            sliderInput(
                inputId = "N", 
                label   = h5("Sample size"),
                min     = 5,
                max     = 50,
                step    = 5,
                value   = 10,
                animate = TRUE
            ),
            #
            # Standard deviation slider
            sliderInput(
                inputId = "sd", 
                label   = h5("Standard deviation"),
                min     = 1,
                max     = 10,
                step    = 1,
                value   = 2,
                animate = TRUE
            ),
            #
            # Number of intervals slider
            sliderInput(
                inputId = "K", 
                label   = h5("Number of intervals"),
                min     = 10,
                max     = 200,
                step    = 10,
                value   = 100,
                animate = TRUE
            ),
            #
            # Random seed box
            numericInput(
                inputId = "seed",
                label   = h5("Random seed"),
                value   = 1, 
                min     = 1,
                max     = 999999)
            ),
        #
        #Main Panel
        mainPanel(
            #Shows the plot output
            plotOutput("graph")
        )
    )
))
