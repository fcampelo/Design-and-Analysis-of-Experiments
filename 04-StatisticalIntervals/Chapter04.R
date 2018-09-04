# Chapter 4 routines
#====================

# Enclosure plot
x    <- seq(from = 42, to = 58, by = 0.001)
xq   <- qnorm(p = c(0.025, 0.975), mean = 50, sd = 2)
ytop <- dnorm(x = xq[2], mean = 50, sd = 2)
data <- data.frame(x = x, y = dnorm(x = x, mean = 50, sd = 2))

shade1 <- rbind(c(xq[2], 0), 
                subset(data, x > xq[2]), 
                c(data[nrow(data), "X"], 0))
shade2 <- rbind(c(xq[1], 0), 
                subset(data, x < xq[1]), 
                c(data[nrow(data), "X"], 0))
shade3 <- rbind(c(xq[1], 0), 
                subset(data, (x > xq[1]) & (x < xq[2])), 
                c(xq[2], 0))

# Generate plot (using ggplot2)
library(ggplot2)

# pdf("../figs/enclosure.pdf", width = 12, height = 5)
mp <- ggplot(data, aes(x = x, y = y))
mp + 
  geom_line() + 
  xlim(42, 58) + 
  geom_segment(aes(x = xq[2], y = 0, xend = xq[2], yend = ytop)) +
  geom_segment(aes(x = xq[1], y = 0, xend = xq[1], yend = ytop)) +
  geom_polygon(data = shade1, aes(x = x, y = y), fill="#FFAAAA") +
  geom_polygon(data = shade2, aes(x = x, y = y), fill="#FFAAAA") +
  geom_polygon(data = shade3, aes(x = x, y = y), fill="#AAFFAA") +
  xlab("X") + ylab("P(X)") +
  ggtitle("95% enclosure of a population", 
          subtitle = "(Normal distribution, mean = 50, sd = 2)") +
  theme(text = element_text(size = 20))
# dev.off()


#====================

# Quantile plots
## 1: Standard Normal Distribution

x    <- seq(from = -4, to = 4, by = 0.01)
mypr <- c(0.01, 0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975, 0.99)
xq   <- qnorm(p = mypr)
ytop <- dnorm(x = xq)
data <- data.frame(x = x, y = dnorm(x = x))

# Generate plot (using ggplot2)
library(ggplot2)
# pdf("../figs/quantiles-normal.pdf", width = 12, height = 5)
mp <- ggplot(data, aes(x = x, y = y)) + geom_line()

for (i in seq(xq)){
  mp <- mp + 
    geom_segment(x = xq[i], y = 0, xend = xq[i], yend = ytop[i], 
                 lty = 1, color = "#999999")
}

mp + geom_text(data = data.frame(x = xq, y = 0, mypr = round(mypr, 3)),
               aes(x = xq, 
                   y = y , 
                   label = mypr), angle = 90, hjust = 0, vjust = 1) +
  xlab("X") + ylab("P(X)") +
  ggtitle("Quantiles of the standard Normal distribution") +
  theme(text = element_text(size = 20))
# dev.off()




## 2: Chi-squared Distribution for N = 15
n    <- 15
x    <- seq(from = 0, to = 50, by = 0.1)
mypr <- c(0.01, 0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975, 0.99)
xq   <- qchisq(p = mypr, df = n - 1)
ytop <- dchisq(x = xq, df = n - 1)
data <- data.frame(x = x, y = dchisq(x = x, df = n - 1))

# Generate plot (using ggplot2)
library(ggplot2)
# pdf("../figs/quantiles-chisq.pdf", width = 12, height = 5)
mp <- ggplot(data, aes(x = x, y = y)) + geom_line()

for (i in seq(xq)){
  mp <- mp + 
    geom_segment(x = xq[i], y = 0, xend = xq[i], yend = ytop[i], 
                 lty = 1, color = "#999999")
}

mp + geom_text(data = data.frame(x = xq, y = 0, mypr = round(mypr, 3)),
               aes(x = xq, 
                   y = y , 
                   label = mypr), angle = 90, hjust = 0, vjust = 1) +
  xlab("X") + ylab("P(X)") +
  ggtitle("Quantiles of the Chi-squared distribution",
          subtitle = ("with 14 degrees-of-freedom")) +
  theme(text = element_text(size = 20))
# dev.off()


#====================

# Generate 100 confidence intervals for samples of 25
# points from N(mu = 50, sigma = 2). Confidence level 0.95
set.seed(661857)    # Set PRNG seed (for reproducibility)
mu <- 50            # desired mean
sd <- 2             # desired standard deviation
N  <- 25            # Desired sample size
K  <- 100           # Number of CIs
alpha <- 0.05       # Significance level

X <- matrix(rnorm(K * N, mean = mu, sd = sd), # Generate a column matrix of 
            nrow = N)                         # samples of size N

# Calculate upper and lower bounds for the K confidence intervals
Xbar <- apply(X, MARGIN = 2, FUN = mean)
S    <- apply(X, MARGIN = 2, FUN = sd)

CI_lower <- Xbar + S * qt(alpha / 2, N - 1) / sqrt(N)
CI_upper <- Xbar + S * qt(1 - alpha / 2, N - 1) / sqrt(N)

isout <- (CI_lower > mu) | (CI_upper < mu)  # Flag CIs that do not contain the
                                            # true parameter value

## Plot the CIs
# pdf("../figs/CIs.pdf", width = 12, height = 8)

plot(0, 0, type="n",
     xlim = c(0, K),
     ylim = c(47, 53),
     ylab = "X",
     xlab = "Interval",
     main = "Confidence Intervals",
     las  = 1)
for (i in 1:K){
    points(x = c(i, i), y = c(CI_lower[i], CI_upper[i]),
           type = "l",
           lwd  = ifelse(isout[i], 3, 2),
           col  = ifelse(isout[i], "red", "green"))
}
points(x = c(0, K), y = c(mu, mu),
       lty  = 2,
       lwd  = 3,
       type = "l")

# dev.off()

#=====



