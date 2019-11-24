# HW9: Log normal distribution
#
# In this exercise, we will study Log Normal distribution. 
# A log-normal distribution is a continuous probability distribution 
# of a random variable whose logarithm is normally distributed. 
# Thus, if the random variable X is log-normally distributed, 
# then Y = ln(X) has a normal distribution.
# Source: https://en.wikipedia.org/wiki/Log-normal_distribution
#
# 1. Set the random seed to zero and save the random seed vector to `seed`. 
#    (hint: use the command `seed <- .Random.seed`).
#    Generate 1000 samples from a log-normal distribution
#    with parameters `meanlog = 2`, `sdlog = 1`, add with random
#    noise from a normal distribution with mean 0 and standard deviation 0.05.
#    To do this, you should use:
#      - `set.seed()` to set random seed
#      - `rlnorm()` to generate random variables from log-normal distribution
#      - `rnorm()` to generate random variables from normal distribution
#    Save the generated vector into `x`.
## Do not modify this line!
set.seed(0)
seed <- .Random.seed
x <- rlnorm(1000,meanlog = 2,sdlog=1)+rnorm(1000,0,.05)


# 2. Now we want to estimate the parameters from these simulated points.
#    Let's create a function `mom_lnorm` that takes an input `par`, a
#    vector of length 2, representing respectively `meanlog` and `sdlog` 
#    in the log-normal distribution. 
#    The function will output a vector of length 2, representing respectively 
#    the expectation and variance of the log-normal distribution.
#    Note: `expectation = exp(meanlog + 0.5 * sdlog^2)`,
#          `variance = exp(2 * meanlog + sdlog^2) * (exp(sdlog^2) - 1)`.
#    Example: `mom_lnorm(c(1, 1))` would return `[1]  4.481689 34.512613`.
#    Then create a function factory `obj_lnorm_factory` that takes an input `x`
#    representing the data, and return a function which takes an input `par` as
#    parameter and returns the sum of squared error between the 
#    true mean and variance of `x` and the log-normal distribution
#    specified by `par`.
#    Example: `obj_lnorm_factory(c(0, 1))(c(1, 1))` would return `[1] 1172.712`.
## Do not modify this line!
mom_lnorm <- function(par) c(exp(par[1]+.5*par[2]**2),exp(2*par[1]+par[2]^2)*(exp(par[2]^2)-1))
obj_lnorm_factory <- function(x){
  function(par) (mom_lnorm(par)[1]-mean(x))^2+(mom_lnorm(par)[2]-var(x))^2
}


# 3. (1) Create a function `par_lnorm` to estimate the parameters (the 
#    moments of the distribution) from the data.
#    Goal: estimate the parameters that minimize the squared difference
#    between the true and estimated moments with a confidence interval.
#    Input: `x`, representing the data.
#    Output: # A tibble: 2 x 5
#              parameter value     se lower upper
#              <chr>     <dbl>  <dbl> <dbl> <dbl>
#            1 mu_mom      ...    ...   ...   ...
#            2 sd_mom      ...    ...   ...   ...
#    where
#    `mu_mom` is the estimate for `meanlog`.
#    `sd_mom` is the estimate for `sdlog`.
#    `se` is the standard error of the estimate.
#    `lower` and `upper` are the lower and upper bound
#    of the 95% confidence interval.
#    You can use:
#    - `optim()` to compute the optimal value.
#      - Set startpoint as `c(1, 1)`.
#      - Set `fn` to `obj_lnorm_factory(x)`.
#      - Set `hessian` to `TRUE`.
#    - `solve()` to compute the inverse of hessian matrix.
#    - `sqrt()` and `diag()` to compute the standard error.
#    - `tibble()` to create a tibble.
#    (2) Use `par_lnorm` with the data `x` simulated in question 1 to estimate 
#    the moments. Save the tibble to `result_mom`.
#    Save the estimate of `meanlog` to scalar `mu_mom`.
#    Save the estimate of `sdlog` to scalar `sd_mom`.
## Do not modify this line!
par_lnorm <- function(x) {
  op <- optim(c(1,1),obj_lnorm_factory(x),hessian = T)
  value <- op$par
  se <- sqrt(diag(solve(op$hessian)))
  lower <- value-1.96*se
  upper <- value+1.96*se
  return(tibble(parameter=c("mu_mom","sd_mom"),
                value = value,
                se = se,
                lower = lower,
                upper = upper))
}
result_mom <- par_lnorm(x)

mu_mom <- as.numeric(result_mom[1,2])
sd_mom <- as.numeric(result_mom[2,2])
# 4. After using the method of moments, let's try maximum likelihood estimators.
#    Create a function `nll_lnorm` to calculate the negative log-likelihood
#    of our data under the parameters. The function will take two inputs `par`,
#    a vector of length two representing the moments, and `x`, the data. 
#    It returns the negative loglikelihood.
#    To do this, you can use:
#      - `dlnorm()` to calculate the likelihood
#      - specify `log = TRUE` to calculate the loglikelihood.
#    Then use your `nll_lnorm()` and `optim()` to estimate the parameters
#    (i.e. minimize the negative loglikelihood). Choose your startpoint to be `c(1, 1)`.
#    Save your estimated parameter `meanlog` into `mu_mle` and `sdlog` into `sd_mle`.
## Do not modify this line!
nll_lnorm <- function(par,x) -sum(dlnorm(x,par[1],par[2],log=T))
op <-optim(c(1, 1),nll_lnorm,x=x)
mu_mle <- op$par[1]
sd_mle <- op$par[2]

# 5. Compare the result of MOM and MLE by ploting the estimated density curves
#    together on the same plot.
#    You should first load `ggplot2` and then add a histogram and the estimated
#    density of the original `x`, then add the two estimated density curves.
#    To do this, you can use:
#      - `as.data.frame()` to tranform `x` into a dataframe.
#      - `ggplot()` to initialize a ggplot object.
#        Set `aes(x = x)`.
#      - `geom_histogram()` to add an histogram with density scale.
#        - set `y` to `..density..` in the aesthetics and specify `bins=50`.
#      - `geom_density()` to add the density curve.
#        - specify `color = "Data"`.
#      - `stat_function` to add the two curves for MOM and MLE.
#        - specify `color = "MOM"` and `color = "MLE"` for each curve.
#      - `labs()` to format the labels such that:
#        - `title = "Both methods seem to do a good job"`.
#        - `x = "Simulated x"`.
#        - `y = "Density"`.
#      - `theme_light()` to change the theme of plots.
#      - `theme()` to change the title and subtitle to the middle of the plot.
#        - Set its argument `plot.title` using `element_text(hjust = 0.5)`.
#    Save your figure into `compare_plot`.
#    
## Do not modify this line!
library(ggplot2)
compare_plot <- ggplot(as.data.frame(x),aes(x=x))+
  geom_histogram(aes(y=..density..),bins=50)+
  geom_density(aes(color="Data"))+
  stat_function(aes(color="MOM"),fun = dlnorm,args = list(meanlog=mu_mom,sdlog=sd_mom))+
  stat_function(aes(color="MLE"),fun = dlnorm,args = list(meanlog=mu_mle,sdlog=sd_mle))+
  labs(title = "Both methods seem to do a good job",
       x = "Simulated x",
       y = "Density")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))
compare_plot



