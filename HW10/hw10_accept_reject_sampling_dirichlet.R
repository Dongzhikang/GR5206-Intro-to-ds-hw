# HW10: Acceptance-Rejection Sampling for Dirichlet distribution
#
# In this exercise, we will use Acceptance-Rejection Sampling method 
# to sample from the Dirichlet distribution.
# 
# The Dirichlet distribution, often denoted `Dir(alpha)`, is a family
# of continuous multivariate probability distributions parameterized by 
# a vector `alpha` of positive real numbers.
# 
# It is commonly used as prior distributions in Bayesian statistics, 
# as the Dirichlet distribution is the conjugate prior of the 
# categorical distribution and multinomial distribution.
# 
# Source: https://en.wikipedia.org/wiki/Dirichlet_distribution
#
# 1. In this part, we will sample from a Dirichlet distribution 
#    using package `MCMCpack`.
#    (1)
#    Load the `tidyverse`, `MCMCpack` and `ggplot2` packages.
#    Set the random seed to zero and save the random seed vector to 
#    `seed`. (hint: use the command `seed <- .Random.seed`).
#    (2)
#    Generate 1000 samples from `Dir(2, 2, 2)` using `rdirichlet()`.
#    Save the result into a tibble `X`
#    and rename the columns `x1`, `x2`, and `x3`.
#    The first rows of `X` should be:
#    # A tibble: 1,000 x 3
#      x1     x2     x3
#      <dbl>  <dbl>  <dbl>
#    1 0.660 0.216  0.125 
#    2 0.242 0.458  0.300 
#    3 0.556 0.279  0.165 
#    4 0.243 0.336  0.421 
#    5 0.108 0.478  0.414 
#    6 0.403 0.371  0.226 
#    7 0.497 0.403  0.0997
#    8 0.272 0.343  0.385 
#    9 0.200 0.0708 0.730 
#   10 0.151 0.467  0.382 
#   # … with 990 more rows
## Do not modify this line!
library(tidyverse)
library(MCMCpack)
library(ggplot2)
set.seed(0)
seed <- .Random.seed
X <- as_tibble(rdirichlet(1000,c(2,2,2)))%>%
  rename("x1"="V1",
         "x2"="V2",
         "x3"="V3")
X

# 2. Draw the density plot of the samples and save it into `density_plot`.
#    In our case, `K = 3`, and `a1 = a2 = a3 = 2`. This distribution is 
#    essentially a bivariate one, as `X3 = 1 − X1 − X2`. 
#    Therefore, drawing samples from this Dirichlet distribution is equivalent 
#    to drawing samples of `(X1, X2)`. We only need to draw the heatmap of the
#    `(X1, X2)`.
#    You can use:
#    - `ggplot()` to initialize a ggplot object.
#      Set `data` to `X`.
#      Set `mapping` to `aes(x1, x2)`.
#    - `stat_density_2d()` to plot the density.
#      - Set `fill` in `aes()` to `..density..`.
#      - Set `geom` to `raster`.
#      - Set `contour` to `FALSE`.
#    - `geom_density_2d()` to add contours.
#    - `scale_x_continuous()` to customize x scale.
#      - Set `expand` to `c(0, 0)`.
#    - `scale_y_continuous()` to customize y scale.
#      - Set `expand` to `c(0, 0)`.
#    - `labs()`
#      - Set `title` to `"Density plot of Dir(2, 2, 2)"`.
#      - Set `subtitle` to 
#        `"The samples are distributed in the lower left corner."`.
#    - `theme_light()` to set the theme to light.
#    - `theme()` to remove the legend.
#      - Set `legend.position` to `none`.
## Do not modify this line!
density_plot <- ggplot(X, aes(x1, x2))+
  stat_density_2d(aes(fill=..density..), geom='raster',contour = F)+
  geom_density_2d()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  labs(title = "Density plot of Dir(2, 2, 2)",
       subtitle = "The samples are distributed in the lower left corner.")+
  theme_light()+
  theme(legend.position = "none")
density_plot

# 3. Now, we will use the Acceptance-Rejection Sampling method to simulate 
#    from this bivariate distribution of `(X1,X2)`.
#    Recall that `f(x)` is our target distribution, `g(x)` is our proposed
#    distribution. We want `f(x) ≤ M * g(x)` for `M > 1`.
#    For this part, we will use a two-dimensional uniform distribution on 
#    `[0, 1] x [0, 1]` as our proposal distribution. Note that sampling from
#    a two-dimensional uniform distribution is just sampling from a 
#    one-dimensional twice.
#    (1)
#    Write a function `f(x)` for the target distribution, 
#    in this case `Dir(2, 2, 2)`.
#    It takes in a vector of length 2, representing `x1` and `x2`.
#    The output is the density of the distribution `Dir(2, 2, 2)`.
#    Do not use the functions from `MCMCpack`.
#    For example, `f(c(0, 0)) = 0` and `f(c(0.3, 0.3)) = 4.32`.
#    (2)
#    Find the optimal `M` i.e. the smallest possible value and save it 
#    into scalar `M`.
#    Use `f(x)` to compute `M` instead of using functions from `MCMCpack`.
#    Recall that the density of uniform distribution is always one. 
#    To ensure for every x, `f(x) ≤ M * g(x)`, we just need to make sure 
#    `M` is larger than the highest point of `f(x)`.
#    To check your answer, verify that `round(M, 1)` equals to `4.4`.
#    (3)
#    Write another function `g(x)` for the proposal distribution, 
#    in this case a two-dimensional uniform distribution.
#    It takes in a vector of length 2, representing `x1` and `x2`.
#    The output is the density of the uniform distribution.
#    Remember the density should be zero is the input is not
#    within the support.
#    For example, `g(c(0, 0)) = 1` and `g(c(-1, -1)) = 0`.
## Do not modify this line!
f <- function(x) {
  x3 <-1-sum(x)
  if(x3<0) x3<-0
  prod(x)*x3/(1/gamma(6))
}
g <- function(x) dunif(x[1])*dunif(x[2])
fg <- function(x) {
  f(x)/g(x)
}

M <-fg(c(1/3,1/3))
M

# 4. (1)
#    Use `f(x)`, `g(x)`, and `M` that we just defined to write a 
#    sampling function `sample_one()` to draw one sample from 
#    `Dir(2, 2, 2)`. There is no input.
#    The output is a tibble with 3 columns:
#    - The fisrt two columns `x1` and `x2` contain the pair of 
#      sample `x1` and `x2`.
#    - The third column `count` contains the the number of 
#      attempts to obtain this sample.
#    You can use the following skeleton to write your function:
#    ```
#    sample_one <- function() {
#      count <- 0
#      accepted <- FALSE
#      while (!accepted) {
#        ## your code goes here
#      }
#      ## your code goes here
#    }
#    ```
#    For example, if you set seed to zero, `sample_one()` would return:
#    # A tibble: 1 x 3
#         x1    x2 count
#      <dbl> <dbl> <dbl>
#    1 0.126 0.267     7
#    `(0.126, 0.267, 0.607)` is the sample we draw (with 
#    `0.601 = 1 - 0.126 - 0.26`). To obtain this sample, we sampled 
#    from `g(x)` 7 times before the sample is accepted.
#    (2)
#    Write a sampling function `sample_n(n, seed)` that 
#    draws samples from `Dir(2, 2, 2)`.
#    Use `sample_one()` that we just defined.
#    The input `n` is the number of samples we will draw.
#    The input `seed` is the seed used within the function. 
#    The default seed should be set to zero. 
#    Remember to use `set.seed()` inside the function.
#    The output is a tibble with 3 columns:
#    - The fisrt two columns `x1` and `x2` contain the pair of 
#      sample `x1` and `x2`.
#    - The third column `count` contains the the number of 
#      attempts to obtain this sample.
#    For example, `sample_n(100, 100)` would return
#    # A tibble: 100 x 3
#         x1    x2 count
#      <dbl> <dbl> <dbl>
#    1 0.308 0.258     1
#    2 0.280 0.398     3
#    3 0.205 0.358     1
#    4 0.208 0.307     8
#    5 0.236 0.275     1
#    6 0.445 0.358     3
#    7 0.327 0.389     9
#    8 0.397 0.393     5
#    9 0.186 0.348    16
#   10 0.219 0.292     7
#   # … with 90 more rows
## Do not modify this line!
set.seed(0)
sample_one<-function(){
  count<-0
  accepted<-F
  while(!accepted){
    x1<-runif(1)
    x2<-runif(1)
    u<-runif(1)
    count<-count+1
    if(f(c(x1,x2))/g(c(x1,x2))>=M*max(runif(1),u)){
      return(tibble(x1=x1,x2=x2,count=count))
      break
    }
    if(count>500){
      return("you mess up")
      break
    }
  }
}

sample_n <- function(n, seed=0){
  set.seed(seed)
  x1 <- c()
  x2 <- c()
  co <- c()
  for (i in 1:n){
    s <- sample_one()
    x1[i] <- s[[1]]
    x2[i] <- s[[2]]
    co[i] <- s[[3]]
  }
  return(tibble(x1=x1,x2=x2,count=co))
}
sample_n(100, 100)


# 5. (1)
#    Draw 1000 samples from `Dir(2, 2, 2)` using `sample_n()` with 
#    default seed and save the samples in `samples`.
#    The first rows of `samples` should be:
#    # A tibble: 1,000 x 3
#         x1    x2 count
#      <dbl> <dbl> <dbl>
#    1 0.126 0.267     7
#    2 0.122 0.245    16
#    3 0.208 0.229    10
#    4 0.557 0.329     6
#    5 0.181 0.530     1
#    6 0.269 0.181    12
#    7 0.340 0.262     4
#    8 0.131 0.374     3
#    9 0.143 0.415     3
#   10 0.109 0.382     7
#   # … with 990 more rows
#    (2)
#    Compute the acceptance rate using `count` column in `samples` 
#    and save it in scalar `acceptance_rate`.
#    You can see that the acceptance rate is around 15%. It is very 
#    low and we would like it to be higher to get a more efficient procedure.
## Do not modify this line!
samples <- sample_n(1000)
acceptance_rate <- 1000/sum(samples$count)


# 6. Plot the dot plot of the samples and save it into `sample_plot`.
#    You can use:
#    - `ggplot()` to initialize a ggplot object.
#      Set `data` to `samples`.
#      Set `mapping` to `aes(x1, x2)`.
#    - `geom_point()` to plot the dots.
#      - Set `alpha` to `0.8`.
#    - `labs()`
#      - Set `title` to `"Samples from Dir(2, 2, 2)"`.
#      - Set `subtitle` to `"It is similar to ones that drawn from rdirichlet."`.
#    - `theme_light()` to set the theme to light.
## Do not modify this line!
sample_plot <- ggplot(samples)+
  geom_point(aes(x1,x2), alpha=.8)+
  labs(subtitle = "It is similar to ones that drawn from rdirichlet.",
       title = "Samples from Dir(2, 2, 2)")+
  theme_light()


# 7. The acceptance rate of using Uniform distribution is really low since
#    the shape of these two distribution are not alike.
#    We need to find a distribution that has a peak around the peak of the 
#    dirichlet distribution, but that doesn't decay too fast to 0 so that the 
#    constant `M` dosn't have to be too high.
#    Let's suppose that we can sample efficiently from a Beta distribution.
#    We can find parameters such that the Beta distribution will have a shape
#    that is closer to the Dirichlet distribution than the uniform distribution.
#    To make the sampling more efficient, we will use the product of two
#    Beta distribution as the proposal distribution. 
#    In order to have a peak around the peak of the dirichlet distribution, 
#    and not decay too fast ot 0, the alpha and beta will all be 1.1. 
#    (It will give only a slightly better convergence rate. We encourage you 
#    to play with different alphas and betas once the exercises is done to see 
#    how `M` and the acceptance rate evolve.)
#    (1)
#    Write a function `g2(x)` for the product of two Beta distribution.
#    It takes in a vector of length 2, representing `x1` and `x2`.
#    The output is the product of the values of the 
#    density of a `Beta(1.1, 1.1)` distribution at points `x1` and `x2`.
#    You can use the `dbeta()` function.
#    For example, `g2(c(0, 0)) = 0` and `g2(c(0.5, 0.5)) = 1.123137`.
#    (2)
#    Set M to `4.06` and save it into `M2`.
#    This value makes sure for all x, `f(x) <= M2 * g2(x)`.
## Do not modify this line!
g2 <- function(x) dbeta(x[1],1.1,1.1)*dbeta(x[2],1.1,1.1)
M2 <- 4.06

# 8. (1)
#    Use `f(x)`, `g2(x)`, and `M2` that we just defined to write a sampling 
#    function `sample_one2()` to draw one sample from `Dir(2, 2, 2)`.
#    There is no input.
#    The output is a tibble with 3 columns:
#    - The fisrt two columns `x1` and `x2` contain the pair of 
#      sample `x1` and `x2`.
#    - The third column `count` contains the the number of 
#      attempts to obtain this sample.
#    For example, if you set seed to zero, `sample_one2()` would return:
#    # A tibble: 1 x 3
#         x1    x2 count
#      <dbl> <dbl> <dbl>
#    1 0.433 0.313    11
#    (2)
#    Write a sampling function `sample_n2(n, seed)` that 
#    draws samples from `Dir(2, 2, 2)`.
#    Use `sample_one2()` that we just defined.
#    The input `n` is the number of samples we will draw.
#    The input `seed` is the seed used within the function. 
#    The default seed should be set to zero. 
#    Remember to use `set.seed()` inside the function.
#    The output is a tibble with 3 columns:
#    - The fisrt two columns `x1` and `x2` contain the pair of 
#      sample `x1` and `x2`.
#    - The third column `count` contains the the number of 
#      attempts to obtain this sample.
#    For example, `sample_n2(100, 100)` would return
#    # A tibble: 100 x 3
#          x1     x2 count
#       <dbl>  <dbl> <dbl>
#    1 0.316  0.550      1
#    2 0.218  0.338      8
#    3 0.587  0.134      1
#    4 0.458  0.255      2
#    5 0.401  0.474      9
#    6 0.549  0.248      1
#    7 0.229  0.362     14
#    8 0.279  0.0851     2
#    9 0.0689 0.310      4
#    10 0.143  0.338      7
#    # … with 90 more rows
#    (3)
#    Draw 100 samples from `sample_n2()` with default seed and 
#    save the acceptance rate to `acceptance_rate2`. You can see 
#    that the acceptance rate is around 16%.
#    It is slightly better and the procedure will be faster.
#    (given that we know how to sample from a beta distribution).
#    Sampling from a Dirichlet distribution can be tricky and expensive.
#    You can try to think of other distributions that would help!
## Do not modify this line!
sample_one2<-function(){
  count<-0
  accepted<-F
  while(!accepted){
    x<-rbeta(2,1.1,1.1)
    u<-runif(2)
    count<-count+1
    if(f(x)/g2(x)>M2*max(u)){
      return(tibble(x1=x[1],x2=x[2],count=count))
      break
    }
  }
}

sample_n2 <- function(n, seed=0){
  set.seed(seed)
  x1 <- c()
  x2 <- c()
  co <- c()
  for (i in 1:n){
    s <- sample_one2()
    x1[i] <- s[[1]]
    x2[i] <- s[[2]]
    co[i] <- s[[3]]
  }
  return(tibble(x1=x1,x2=x2,count=co))
}
sample_n2(100, 100)
acceptance_rate2 <- sample_n2(100)

