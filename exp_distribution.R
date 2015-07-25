library(ggplot2)

#simulation
set.seed(2534)
nosim <- 1000
lambda <- 0.2
data <- data.frame( x = rexp(nosim * 40, lambda))

averages <- data.frame(x = apply(matrix(data$x,nosim),1,mean))

# sample mean of 40 exponential distribution with 1,000 simulations
(sample.mean <- mean(averages$x))

# mean from theoretical mean of the distribution
(theoretical.mean <- 1/lambda)

# sample variance
(sample.variance <- var(averages$x))
# theoretical variance
(theoretical.var <- 1 / (40 * lambda^2))



# density plot
g <- ggplot(averages, aes(x = x))
g <- g + geom_density()
g <- g + geom_vline( xintercept = sample.mean, size = 1, color = 'red') 
g

# comparison to normal distribution

mu <- 1 / lambda
sigma <- 1 / lambda
g <- ggplot(averages, aes(x = x))
g <- g + geom_histogram(alpha = .10, binwidth=0.1, colour = "black", aes(y = ..density..))
g <- g + stat_function(geom = "line", 
                       fun = dnorm, 
                       arg = list(mean = mu, sd = sigma/sqrt(40)),
                       size = 2, 
                       colour = "red", 
                       fill = NA)
g



# expotential distribution
g <- ggplot(data, aes(x = x))
g <- g + geom_histogram(alpha = .20, binwidth=0.8, colour = "black", aes(y = ..density..))
g <- g + stat_function(fun = dexp, 
                       arg = list(rate = lambda), 
                       size = 1, 
                       colour = "red")
g


qqnorm(averages$x, main ="Normal Q-Q Plot")
qqline(averages$x, col = "3")

par(mar = c(5,5,0,0) + 0.5, mgp = c(2,1,0))
y <- (averages$x - mu) / (sigma / sqrt(40))  # normalize the data
qqnorm(y, main = NULL)
qqline(y)
