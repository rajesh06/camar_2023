easyr::begin() # An open source library created by my Oliver Wyman colleague Bryce Chamberlain
set.seed(14159)


# Simulate Claims ---------------------------------------------------------

claims <- rlnorm(n = 100, meanlog = 10.5, sdlog = 1.5)
# mean = ~112e3 sd = 326e3



# True Distribution -------------------------------------------------------

exp(10.5 + 1.5^2/2) #mean
exp(10.5 + 1.5^2/2) * sqrt(exp(1.5^2) - 1) #sd
mean(claims); sd(claims)
# mean = ~86e3 sd = 145e3 # compare to true values



# MLE ---------------------------------------------------------------------

mu <- claims |> log() |> mean()
sigma <- claims |> log() |> sd()
exp(mu + sigma^2/2) #mean
exp(mu + sigma^2/2) * sqrt(exp(sigma^2) - 1) #sd
# mean = ~98e3 sd = 266e3 # compare to true values


# Bayesian MCMC -----------------------------------------------------------

fit <- rstan::stan(file = 'lognormal.stan', model_name = 'camar_2023', 
  data = list(y = claims, N = length(claims)), chains = 3, iter = 5000, warmup = 1000)
params <- rstan::extract(object = fit, pars = c('mu', 'sigma'))


# Risk --------------------------------------------------------------------

par(mfrow = c(2,1))
plot(x = params$mu, y = params$sigma, ylab = 'sigma', xlab = 'mu', 
  main = 'MLE', type = 'n')
points(x = claims |> log() |> mean(), 
  y = claims |> log() |> sd(), pch = 4, cex = 3, col = 'blue')

plot(x = params$mu, y = params$sigma, ylab = 'sigma', xlab = 'mu', 
  main = 'Bayesian MCMC')
points(x = 10.5, y = 1.5, pch = 4, cex = 3, col = 'red')
points(x = claims |> log() |> mean(), 
  y = claims |> log() |> sd(), pch = 4, cex = 3, col = 'blue')


