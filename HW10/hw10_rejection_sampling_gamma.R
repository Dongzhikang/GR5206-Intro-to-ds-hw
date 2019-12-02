# HW10: Rejection sampling for Gamma distribution estimate.
#
# In this exercise, we will go over rejection sampling for estimating
# the gamma distribution Gamma(10, 0.3).
#
# 1. Load the packages `ggplot2`, `tibble` and `tidyverse`.
#    Create a function `f <- function(x)` that takes as input
#    one scalar `x` and returns the gamma density for `shape = 10`
#    and `scale = 0.3`.
#    Example : `f(1) = 0.01664854`.
#    Create a function `g <- function(x)` that takes as input scalar `x` and returns
#    the density of uniform distribution between 0 and 10.
#    Example : `g(2) = 0.1`.
#    These functions will make the computations easier.
#    Find the maximum of `f`.
#    To do that, use `optimize()` to find where `-f` reaches its minimum and
#    and assign it to `mode`. Then, compute the corresponding value for `f` and
#    assign it to `M_best`.
## Do not modify this line!
library(ggplot2)
library(tibble)
library(tidyverse)
f <- function(x) dgamma(x, shape=10, scale = .3)
g <- function(x) dunif(x, 0, 10)
mode <- optimize(f,c(0,10), maximum = T)$maximum
M_best<-f(mode)
# 2. Use `set.seed()` to set seed to `0` and save seed to `seed1` using `.Random.seed`.
#    Create a function `rejection_sampling <- function(M, f, g, seed = 0, n = 1e3)`
#    which takes as input a scalar `M`, two functions `f` and `g`, a seed and a number `n`,
#    and returns samples of `f` between 0 and 10 using rejection sampling.
#    The function should :
#      - set the seed to `seed` using `set.seed()`.
#        Do that inside the function so each call
#        to the function will use the same random seed.
#      - generate `n` samples from `runif(1000, 1, 10)` into `y`
#      - generate `n` samples from `runif(1000, 0, 1)` into `u`
#      - return the values of `y` such that `u < f(y) / (M *  g(y))`.
#    Example: `rejection_sampling(1, f, g, 0, 3)` will return `[1] 2.655087 3.721239`.
## Do not modify this line!
set.seed(0)
seed1 <- .Random.seed
rejection_sampling <- function(M, f, g, seed=0, n=1e3){
  set.seed(seed)
  y <- runif(n, 0, 10)
  u <- runif(n, 0, 1)
  b<-(u < f(y) / (M *  g(y)))
  return(y[b])
}
rejection_sampling(1, f, g, 0, 3)

# 3. Set `n` to `1e3`
#    Generate a tibble `result` which gives the approximated acceptance ratio for
#    different constants M. It should ciontain three columns :
#     - `M` that contains a sequence of number from `M_best` to `5` spaced by `0.025`.
#       You can use `seq()` to generate this sequence.
#     - `Y` that contains samples of the distribution when using rejection sampling
#       with constant `M`. You can use `map()` and `rejection_sampling()` to compute it.
#     - `acceptance_rate` that contains the ratio of samples accepted and `n`
#    `result` should print to:
#    # A tibble: 183 x 3
#          M Y           acceptance_rate
#      <dbl> <list>                <dbl>
#    1 0.439 <dbl [482]>           0.482
#    2 0.464 <dbl [478]>           0.478
#    3 0.489 <dbl [476]>           0.476
#    4 0.514 <dbl [469]>           0.469
#    5 0.539 <dbl [466]>           0.466
#    6 0.564 <dbl [464]>           0.464
#    7 0.589 <dbl [459]>           0.459
#    8 0.614 <dbl [458]>           0.458
#    9 0.639 <dbl [454]>           0.454
#    10 0.664 <dbl [450]>           0.45
#    # … with 173 more rows
## Do not modify this line!
n <- 1e3
result <- tibble(M=seq(M_best, 5, .025))%>%
  mutate(Y = map(M, rejection_sampling, f=f, g=g),
         acceptance_rate=map(Y, function(x) length(x)/n))%>%
  unnest(acceptance_rate)

result
# 4. Now, let's plot it to see how acceptance ratio changes:
#    - use `ggplot()` to initialize the plot object.
#    - use `geom_line()` to plot the line.
#    - use `labs` with `x` set to `"M"`, `y` set to `"Acceptance rate"` and
#      `title` set to `"The acceptance rate decreases with M"`.
#    - use `theme_light()` to set a light background.
#    Save the plot to `acceptance_plot`.
## Do not modify this line!

acceptance_plot <- ggplot(result)+
  geom_line(aes(M, acceptance_rate))+
  labs(x = "M",
       y = "Acceptance rate",
       title = "The acceptance rate decreases with M")+
  theme_light()
acceptance_plot

# 5. As we were using a distribution defined between 0 and 10, it won't help
#    estimating the whole distribution.
#    We now want to sample from a distribution defined on `[0, Inf)`,
#    and we'll use the Gaussian which is supported on `(-Inf, Inf)`.
#    In order to get the smallest `M` value possible, we can decide to sample from
#    a Gaussian that has the same mode as `f`.
#    Create a function `g2(x)` that represents the gaussian density centered
#    at `mode` with standard deviation `sd = 3`.
#    Compute the new value for `M2`. (It is equal to the maximum of `f/g`,
#    and equal to the value of `f/g` at `mode`.)
#    Set the seed to `1` and save it to `seed` using `.Random.seed`.
#    Generate tibble `result2`, in which :
#     - the first column `y` represents a `n` absolute values of samples from
#       normal distribution with mean `mode` and `sd = 3`. You can use `rnorm()`.
#     - the second column `u` represents `n` samples from uniform distribution
#       between 0 and 1 using `runif()`,
#     - the third column `likelihood_ratio` is the ratio of `f(y)` and `M2 * g(y)`
#     - the fourth column `selected` is set to boolean `u < likelihood_ratio`.
#    `result2` should print to:
#    # A tibble: 10,000 x 4
#    y     u likelihood_ratio selected
#    <dbl> <dbl>            <dbl> <lgl>
#    1 0.821 0.211      0.0142      FALSE
#    2 3.25  0.115      0.862       TRUE
#    3 0.193 0.145      0.000000296 FALSE
#    4 7.49  0.310      0.00408     FALSE
#    5 3.69  0.150      0.649       TRUE
#    6 0.239 0.527      0.00000168  FALSE
#    7 4.16  0.118      0.423       TRUE
#    8 4.91  0.477      0.179       FALSE
#    9 4.43  0.308      0.319       TRUE
#    10 1.78  0.305      0.533       TRUE
#    # … with 9,990 more rows
## Do not modify this line!
g2 <- function(x){
  dnorm(x, mean = mode, sd = 3)
}
f_dev_g <- function(x){
  -1*f(x)/g2(x)
}
mod2 <- optimize(f_dev_g, lower = 0, upper = 10)
mode2 <- mod2[[1]]
(M2 <- -1*f_dev_g(mode2))
set.seed(1)
seed <- .Random.seed
result2 <- tibble(y = rnorm(n, mode, sd = 3),
                  u = runif(n, 0 ,1),
                  likelihood_ratio = f(y) / (M2 * g2(y)),
                  selected = u < likelihood_ratio)
result2
# 6. To visualize it, let's plot the two distributions with the accepted and
#    rejected samples.
#    - Use `ggplot()` to initialize plot object, in which the aesthetics are
#      set to `aes(x = y)`.
#    - Use `geom_point()` to draw sampled points with `data` set to `result2`,
#      and the aesthetics set so that you plot `M2 * u(y) * g2(y)` against `y`,
#      and `color` to `selected`.
#    - Use `stat_function()` to draw the plot of density `f` with `size = 2`.
#    - Use `stat_function()` to draw the plot of density `M2 * g2`
#      setting `color` to `"red"` and  `size = 2`.
#    - Use `labs()` to generate axis labels `x` to `"x"`, `y` to `"Density"`
#      and `title` to `"Acceptance illustration"`
#    - Use `theme_light()`.
#   Save it to `good_rejection_sampling_plot`.
## Do not modify this line!
good_rejection_sampling_plot <- ggplot(result2, aes(x=y))+
  geom_point(aes(y=M2*u*g2(y), color = selected))+
  stat_function(aes(x=y), fun = f, size=2)+
  stat_function(aes(x=y), fun = function(x) M2*g2(x), color="red", size=2)+
  labs(x="x",
       y= "Density",
       title = "Acceptance illustration")+
  theme_light()
good_rejection_sampling_plot


