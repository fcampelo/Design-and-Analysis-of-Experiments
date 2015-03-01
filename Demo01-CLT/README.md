This is a small application intended at illustrating some concepts related to the [Central Limit Theorem](http://www.math.uah.edu/stat/sample/CLT.html).

The idea is quite simple: you pick a distribution of choice (**distr**) 
from a drop-down menu, and select the target sample size (**n**) using a slider. The app then shows you two plots:

1. Histogram containing **n** samples from distribution **distr**, overlayed with the theoretical pdf of the selected distribution;  
2. Histogram of 999 mean values of **n** samples, overlayed with the best fitting Gaussian.

From these two plots it is possible to observe a few interesting characteristics, such as the Gaussian sample distribution of the means and the decreasing variance of the sample distribution of means.

This app was created using [Shiny](http://shiny.rstudio.com), which is a web application framework for [R](http://www.r-project.org) developed by Winston Chang and the fine folks from [RStudio](http://www.rstudio.com). The source code for the app above is somewhat simple, and consists of the two files shown to the right. Easy, right? ;-)

This README page was also written in RStudio, using [R Markdown](http://rmarkdown.rstudio.com).

I hope you enjoy this little demo. This is my very first shiny app, so comments, suggestions and (preferrably constructive) criticism are all very welcome.

Cheers,  
Felipe
