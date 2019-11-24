# HW9: Distribution
#
# In this exercise, you will use a simulated data, estimate the parameters
# and compare the result of method of moments and maximum likelihood estimator.
# We will focus on the beta distribution[https://en.wikipedia.org/wiki/Beta_distribution]
#
# 1. Set your seed to `5206`, and generate 1000 samples from a beta distribution
#    with parameters shape1($\alpha$) = 5, shape2($\beta$) = 10.
#    Save your simulation into `x`.
## Do not modify this line!
set.seed(5206)
x <- rbeta(1000,shape1 = 5, shape2 = 10)


# 2. We want to estimate the parameters from these simulated points.
#    For beta distribution, we have closed form for the method-of-moments estimates of
#    the parameters. You can find the equations through the wikipedia link above (in
#    section Method of moments).
#    Use the closed form to estimate $\alpha$ and $\beta$. Save the estimated $\alpha$
#    into `alpha0` and $\beta$ into `beta0`.
## Do not modify this line!
x_bar <- mean(x)
v_bar <- var(x)
alpha0 <- x_bar*(x_bar*(1-x_bar)/v_bar-1)
beta0 <- (1-x_bar)*(x_bar*(1-x_bar)/v_bar-1)

# 3. Now instead of using the closed form equations, we will try a numerical method.
#    Let's first create a function `mom_beta` that takes an input `par`, a
#    vector of length 2, representing respectively $\alpha$ and $\beta$ in the
#    beta distribution. The function will output a vector of length 2,
#    representing respectively the mean and variance of the beta distribution.
#    You can find formulas on your probability text book or on the website above.
#    Then create a function factory `obj_beta_factory` that takes an input `x`
#    representing the data, and return a function which takes an input `par` as
#    parameter and beta distribution and returns the sum of squared error
#    between the true mean and variance of `x` and the beta distribution
#    specified by `par`.
## Do not modify this line!
mom_beta <- function(par) return(c(par[1]/sum(par),par[1]*par[2]/((sum(par))^2*(sum(par)+1))))

obj_beta_factory <- {
  force(x)
  function(x) function(par) return(sum(c(sum((mean(x)-par[1]/sum(par))^2),sum((var(x)-par[1]*par[2]/((sum(par))^2*(sum(par)+1)))^2))))
}


# 4. Then create a function `par_beta` to estimate the parameters from the data.
#    The function takes an input `x` representing the data, and returns the
#    estimated parameters.
#    To do this, you can use:
#      - `obj_beta_factory()` you just created to create an objective function for
#        optimization for our sampled data `x`.
#      - `optim()` to optimize the objective function
#        - choose startpoint as $\alpha=1$ and $\beta=1$
#      - Then extract and return the optimal parameters
#    Implement your `par_beta` on the simulated data `x` in question #1.
#    Save your estimated parameter $\alpha$ into `alpha1` and $\beta$ into `beta1`.
## Do not modify this line!

par_beta <- function(x) {
  force(x)
  f <- obj_beta_factory(x)
  optim(c(1,1),f)$par
}
alpha1 <- par_beta(x)[1]
beta1 <- par_beta(x)[2]
# 5. After using the method of moments, let's try maximum likelihood estimators.
#    Create a function `nll_beta` to calculate the negative loglikelihood
#    of our data under the parameters. The function will take two inputs `par`
#    as parameters(a vector of length two) and `x` as data. It returns the
#    calculated negative loglikelihood.
#    To do this, you can use:
#      - `dbeta()` to calculate the likelihood
#        - specify `log = TRUE` to calculate the loglikelihood.
#      - `optim()` and `nll_beta()` you just created
#        - choose startpoint as $\alpha=1$ and $\beta=1$
#        - specify `hessian = T`
#        - specify `lower = 0`
#        - specify `method = "L-BFGS-B"`
#      - `solve()` to get the inverse of hessian matrix
#      - `sqrt()` and `diag()` to get the standard error of each variable
#    Save your estimated parameter $\alpha$ into `alpha2` and $\beta$ into `beta2`.
#    Save the estimated lower and upper bound confidence interval into `lower` and
#    `upper` respectively, they should both be vector of length 2.
#    You can use 1.96 for the 0.975 quantile of the standard normal distribution.
## Do not modify this line!
nll_beta <- function(par, x){
  -sum(dbeta(x,par[1],par[2],log = T))
}
op <- optim(c(1,1),nll_beta,x=x,hessian = T,lower = 0,method = "L-BFGS-B")
alpha2 <- op$par[1]
beta2 <- op$par[2]
sd <-sqrt(diag(solve(op$hessian)))
upper <- c(alpha2+1.96*sd[1],beta2+1.96*sd[2])
lower <- c(alpha2-1.96*sd[1],beta2-1.96*sd[2])
# 6. Compare the result of MOM (use `alpha1` and `beta1`) and MLE by ploting the
#    estimated density curves together on the same plot.
#    You should first load `ggplot2` and then add a histogram and the estimated
#    density of the original `x`, then add the two estimated density curves.
#    To do this, you can use:
#      - `as.data.frame()` to tranform `x` into a dataframe.
#      - `ggplot()` to initialize a ggplot object.
#        Set its arguments `data` and `mapping`.
#      - `geom_histogram()` to add a hitogram with density scale.
#        - specify `bins=20`.
#      - `geom_density()` to add the density curve.
#        - specify `color = "Data"`.
#      - `stat_function` to add the three curves for MOM, MLE and True Value.
#        - specify `color = "MOM"`, `color = "MLE"` and `color = "True Value"`
#          for each curve.
#      - `labs()` to format the labels such that:
#        - `title = "Both of the methods seem to do a good job"`.
#        - `x = "Simulated x"`.
#        - `y = "Density"`.
#      - `theme_light()` to change the theme of plots.
#      - `theme()` to change the title and subtitle to the middle of the plot.
#        - Set its argument `plot.title` using `element_text(hjust = 0.5)`.
#    Save your figure into `g1`.
## Do not modify this line!
library(ggplot2)

g1 <-ggplot(as.data.frame(x))+
  geom_histogram(aes(x,y = ..density..), bins = 20)+
  geom_density(aes(x,color="Data"))+
  stat_function(aes(x,color="MOM"),fun = dbeta, args=list(shape1 = alpha0,shape2 = beta0))+
  stat_function(aes(x,color="MLE"), fun = dbeta, args=list(shape1 = alpha2,shape2 = beta2))+
  stat_function(aes(x,color="True Value"), fun = dbeta, args=list(shape1 = 5,shape2 = 10))+
  labs(title = "Both of the methods seem to do a good job",
       x = "Simulated x",
       y = "Density")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))
g1

