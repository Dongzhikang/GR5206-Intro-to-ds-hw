# HW9: Inverse Gaussian distribution
#
# In this exercise, we will generate random data, estimate the parameters
# and compare the result of method of moments and maximum likelihood estimators.
# We will focus on the inverse gaussian distribution.
# For details, here is the link: [https://en.wikipedia.org/wiki/Inverse_Gaussian_distribution]
#
# Throughout the exercise:
#    - Do NOT use `for`, `while` or `repeat` loops.
#    - Use `%>%` to structure your operations.
#    - Use `theme_light()` for the plots.
#    - For graphs with titles, make the format as
# `theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))`.
#
# 1. Load the packages `statmod`, `ggplot2` and `tidyverse`.
#    Set your seed to `100`. Store the seed to `seed1`.
#    Generate 1000 samples from a inverse gaussian
#    distribution with parameters `mean` = 2, `shape` = 3.
#    To do this, you should use:
#      - `set.seed()` to set random seed
#      - `rinvgauss()` to generate random variables from inverse gaussian
#    distribution
#    Save your simulation into `random_data`.
## Do not modify this line!
library(statmod)
library(ggplot2)
library(tidyverse)
set.seed(100)
seed1 <- .Random.seed
random_data <- rinvgauss(1000,mean = 2,shape = 3)


# 2. Create a function `mom_invgauss()` that takes an input `par`. The function
#    should finally return the moments of mean and variance of inverse gassian
#    distribution represented by parameters. You can find formulas on the website
#    presented above.
#    To do this, you can follow the steps:
#      - create a vector of length 2, use two parameters stored in the
#        `par` to represent the mean and variance of the inverse gaussian
#        distribution.
#      - Store the returned vector to `moments` and return it.
#    Example: `mom_invgauss(c(1,1))` would return `[1] 1 1`.
#
#    Create a function `obj_invgauss()` that takes in `x` which represents
#    the data, and an input `par` as parameters of inverse gaussian distribution.
#    The funtion should return the squared error between the true mean and
#    variance of `x` and the inverse gaussian distribution specified by `par`.
#    To do this, you can follow the steps:
#      - create a vector called `moments` that stores mean and
#        variance of `x`.
#      - use `mom_invgauss()` that we just implemented to
#        generate estimated moments.
#      - calculate the differences between estimated moments
#        and true moments, and store returned value to `differences`.
#      - return the sum of `differences` as the sum of square errors.
#    Example: `obj_invgauss(c(1,1),c(1,2,3))` would return `[1] 1`.
#
#    Create a function `par_invgauss1()` that take `x` as an input and returns the
#    estimated parameters.
#    To do this, you can use:
#      - `optim()` to optimize the objective function
#        - choose startpoint as `mean` = 1 and `shape` = 1.
#      - Then extract and return the optimal parameters.
#    Example: `par_invgauss1(c(1,2,3))` would return `[1] 2.00 8.00`
#    printed in 2 digits.
## Do not modify this line!
mom_invgauss <- function(par){
  moments <- c(par[1],par[1]^3/par[2])
  return(moments)
}
obj_invgauss <- function(par,x){
  moments <- c(mean(x),var(x))
  differences <-sum((mom_invgauss(par)-moments)^2)
  return(differences)
}

par_invgauss1 <- function(x){
  op <- optim(c(1,1),obj_invgauss,x=x)
  return(op$par)
}
par_invgauss1(c(1,2,3))

# 3. Use `par_invgauss1()` to estimate the parameters of `random_data`. Store
#    returned estimated parameters to `par1`.
#    Create a list that owns two parameters in `par1`, store returned list
#    to `dparams`.
#    Use `tibble()` to transform `random_data` into a tibble, store the
#    returned data frame to `random_data_tbl`.
#    Draw a qq-plot to plot the estimated distribution vs. true value.
#    To do this, you can use:
#      - `ggplot()` to plot the `random_data`.
#      - `geom_qq()` to draw a inverse gaussian distribution with parameters
#        of `dparams`.
#      - `stat_qq_line()` to draw a straight line that takes inverse gaussian
#        distribution with parameters of `dparams`, make the `color` as red.
#      - `labs()` to name the title as `"The sample seems to distribute along the straight line"`,
#        subtitle as `"The MOM estimators fit well"`,
#        x-axis as `"Theoretical values"`,
#        y-axis as `"Actual values"`.
#    Store returned graph to `g1`.
## Do not modify this line!
par1 <- par_invgauss1(random_data)
dparams <- list(mean=par1[1],shape = par1[2])
random_data_tbl <- tibble(random_data)
g1 <- ggplot(random_data_tbl)+
  geom_qq(aes(sample = random_data),distribution = qinvgauss,dparams = dparams)+
  stat_qq_line(aes(sample=random_data),distribution = qinvgauss,dparams = dparams,color = 'red')+
  labs(title = "The sample seems to distribute along the straight line",
       subtitle = "The MOM estimators fit well",
       x = "Theoretical values",
       y = "Actual values")+
  theme_light()+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
g1
# 4. Create a function `nll_invgauss` to calculate the negative loglikelihood
#    of our data under the parameters. The function will take two inputs `par`
#    as parameters(a vector of length two) and `x` as data. It returns the
#    calculated negative loglikelihood.
#    To do this, you can use:
#      - `dinvgauss()` to calculate the likelihood
#        - specify `log = TRUE` to calculate the loglikelihood.
#
#    Set your seed to `100`. Store the seed to `seed2`.
#    To do this, you should use:
#      - `set.seed()` to set random seed
#
#    Now we will use some package to calculate MLE.
#    To do this, you can use:
#      - `optim()` and `nll_invgauss()` you just created. Inside the function,
#      you should:
#        - choose startpoint as `mean` = 1 and `shape` = 1.
#        - specify `hessian = TRUE`
#        - specify `lower = 0`
#        - specify `method = "L-BFGS-B"`
#        - specify `x` as `random_data`
#        - DON'T sepcify any other parameters in `optim()`
#    Store the returned object to `fit`.
#
#    Use `solve()` to get the inverse of hessian matrix from `fit`, then use
#    `sqrt()` and `diag()` to get the standard error of each variable.
#    Store returned standard error to `se`.
#
#    Extract parameters of fit and save them to `par2`. Then, calculate lower
#    and upper bound confidence interval. Save the estimated lower and upper bound
#    confidence interval into `lower` and`upper` respectively, they should both be
#    vector of length 2.
#    You can use 1.96 for the 0.975 quantile of the standard normal distribution.
## Do not modify this line!
nll_invgauss <- function(par,x){
  -(sum(dinvgauss(x,par[1],par[2],log=T)))
}
set.seed(100)
seed2 <- .Random.seed
fit<-optim(c(1,1),nll_invgauss,hessian = T,lower = 0,method = "L-BFGS-B",x = random_data)
se <- sqrt(diag(solve(fit$hessian)))
par2 <- fit$par
lower <- par2-1.96*se
upper <- par2+1.96*se
# 5. Follow the link given above to find the formulas of mu hat and shape hat.
#    Use those formulas to estimate parameters directly and save returned value to
#    `mu_hat` and `shape_hat` accordingly. Here, the results should be same as `par2`.
## Do not modify this line!

mu_hat <-mean(random_data)
shape_hat <- length(random_data)/sum(1/random_data-1/mu_hat)
# 6. Use `ks.test()` to test if parameters from MLE fit well. The argument
#    should take in `random_data`, `pinvgauss` and two parameters in `par2`.
## Do not modify this line!

ks.test(random_data,pinvgauss,par2[1],par2[2])

# 7. Compare the result of MOM and MLE by ploting the estimated density curves
#    together on the same plot.
#    You should add a histogram and the estimated density of
#    the original `random_data`, then add the two estimated density curves.
#    To do this, you can use:
#      - `ggplot()` to take in `random_data_tbl` and set `x` as `random_data`.
#      - `geom_histogram()` to add a hitogram with density scale, specify `bins=15`.
#      - `geom_density()` to add the density curve, specify `color = "Data"`.
#      - `stat_function` to add the three curves for MOM, MLE and True Value.
#        - specify `color = "MOM"`, `color = "MLE"` and `color = "True Value"`
#          for each curve.
#      - `labs()` to names the title as
#        `"Both MOM and MLE did good jobs to estimate true distribution"`.
#        x-axis as `"Random data"`,
#        y-axis as`"Density"`.
#        Store returned graph to `g2`.
#
## Do not modify this line!
g2 <- ggplot(random_data_tbl)+
  geom_histogram(aes(x=random_data,y=..density..),bins=15)+
  geom_density(aes(random_data,color="Data"))+
  stat_function(aes(random_data,color="MOM"),fun = dinvgauss,args = list(mean=par1[1],shape = par1[2]))+
  stat_function(aes(random_data,color = "MLE"),fun = dinvgauss,args = list(mean=par2[1],shape=par2[2]))+
  stat_function(aes(random_data,color = "True Value"),fun = dinvgauss,args = list(mean=2,shape=3))+
  labs(title = "Both MOM and MLE did good jobs to estimate true distribution",
       x = "Random data",
       y = "Density")+
  theme_light()+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
g2
