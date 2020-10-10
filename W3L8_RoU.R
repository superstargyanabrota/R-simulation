set.seed(10)

##############################################
#### RoU region for exponetial
##############################################
# to plot the regions
u <- seq(0, 1, length = 1e4)
v <- -2*u * log(u)

# accept-reject from C
N <- 5e3
samples <- matrix(0, nrow = N, ncol = 2)
n <- 0
while(n < N)
{
  prop.u <- runif(1, min = 0, max = 1)
  prop.v <- runif(1, min = 0, max = .8)
  if(prop.v <= -2*prop.u * log(prop.u))
  {
    n <- n+1
    samples[n, ] <- c(prop.u,prop.v)
  }
}

# define color based on regions
color <- 0
for(i in 1:10)
{
  color <- color + i*(samples[,2] > i*samples[,1] & samples[,2] < (i+1)*samples[,1])
}
# plot C with colors
par(mfrow = c(1,2))
plot(u, v, type = 'l', main = "C region for Exp")
abline(h = 0)
points(samples, col = color + 1, pch = 16)

x <- samples[,2]/samples[,1]
y <- dexp(x)
plot(x, y, col = color + 1, pch = 16, main = "Exponential samples with density")



##############################################
#### RoU region for normal
### ignoring constants
##############################################
# to plot the regions
u <- seq(0, (2*pi)^(-.25), length = 1e3)
v <- -4*u^2 *(log(u) + log(2*pi)/4)


N <- 5e3
samples <- matrix(0, nrow = N, ncol = 2)
n <- 0
while(n < N)
{
  prop.u <- runif(1, min = 0, max = (2*pi)^(-.25))
  prop.v <- runif(1, min = -1, max = 1 )
  if(abs(prop.v) <= sqrt(-4* prop.u^2 *(log(prop.u) + log(2*pi)/4) ) )
  {
    n <- n+1
    samples[n, ] <- c(prop.u,prop.v)
  }
}

# define color based on regions
color <- 0
for(i in 0:3)
{
  color <- color + (i+1)*(samples[,2] > i*samples[,1] & samples[,2] < (i+1)*samples[,1])
}
for(i in (-1:-3) )
{
  color <- color + (i+8)*(samples[,2] > i*samples[,1] & samples[,2] < (i+1)*samples[,1])
}
par(mfrow = c(1,2))
plot(u, sqrt(v), type = 'l', main = "C region for N(0,1)", ylim = c(-.8, .8))
lines(u, -sqrt(v) )

points(samples, col = color + 1, pch = 16)

x <- samples[,2]/samples[,1]
y <- dnorm(x)
plot(x, y, col = color + 1, pch = 16, main = "Normal samples with density")
