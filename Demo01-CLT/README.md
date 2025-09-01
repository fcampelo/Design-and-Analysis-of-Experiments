This is a small application intended at illustrating some concepts related to the [Central Limit Theorem](http://www.math.uah.edu/stat/sample/CLT.html).

The idea is quite simple: you pick a distribution of choice (**distr**) 
from a drop-down menu, and select the target sample size (**n**) using a slider. 
The app then shows you two plots:

1. Histogram containing **n** samples from distribution **distr**, overlayed with 
the theoretical pdf of the selected distribution;  
2. Histogram of 999 mean values of **n** samples, overlayed with the best fitting 
Gaussian.

From these two plots it is possible to observe a few interesting characteristics, 
such as the Gaussian sample distribution of the means and the decreasing 
variance of the sample distribution of means.

Cheers,  
Felipe
