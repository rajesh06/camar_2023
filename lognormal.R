easyr::begin() # An open source library created by my Oliver Wyman colleague Bryce Chamberlain
set.seed(14159)

claims <- rlnorm(n = 100, meanlog = 10.5, sdlog = 1.5)
# mean = ~112e3 sd = 326e3
exp(10.5 + 1.5^2/2) #mean
exp(10.5 + 1.5^2/2) * sqrt(exp(1.5^2) - 1) #sd
mean(claims); sd(claims)

fit <- rstan::stan(file = 'lognormal.stan', model_name = 'camar_2023', 
  data = list(y = claims, N = length(claims)), chains = 3, iter = 5000, warmup = 1000)
params <- rstan::extract(object = fit, pars = c('mu', 'sigma'))

par(mfrow = c(2,1))
plot(x = params$mu, y = params$sigma, ylab = 'sigma', xlab = 'mu', 
  main = 'Bayesian MCMC')
points(x = 10.5, y = 1.5, pch = 4, cex = 3, col = 'red')

plot(x = params$mu, y = params$sigma, ylab = 'sigma', xlab = 'mu', 
  main = 'MLE', type = 'n')
points(x = claims |> log() |> mean(), 
  y = claims |> log() |> sd(), pch = 4, cex = 3, col = 'blue')
