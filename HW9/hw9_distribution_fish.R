# HW9: distribution_fish
#
# In this exercise, we will walk you through a complete process of fitting
# distributions.
# We suggest the functions you can use to create the tibbles and the plots, but
# you are free to use the methods you are the most comfortable with.
# Make sure that the outputs look exactly like the ones you are supposed to create.
#
# Throughout the exercise:
#    - Do NOT use `for`, `while` or `repeat` loops.
#    - Use `%>%` to structure your operations.
#    - Use `theme_light()` for the plots.
#    - When defining functions, the "create" parts are not mandatory. We suggest
#      creating intermediate variables to increase the readability of your code,
#      and hopefully to decrease the chance of mistakes.
#
# 1. Load the packages `tidyverse` and `gamlss`.
#    Use `read_csv()` to read the dataset `fish.csv` (located in the directory
#    `data/`) into a tibble `data`.
#    This dataset has information about the number of fish caught by visitors
#    (`count`) at a state park, whose distribution will be explored in this
#    homework.
#    To facilitate fitting, pull `count` out of `data` and store it as a vector
#    `cnt` of length 250, using methods such as `pull()`.
## Do not modify this line!
library(tidyverse)
library(gamlss)
data <- read_csv("data/fish.csv")
cnt <- pull(data)

# 2. Before getting deep, we will do some visualization to get a feel for how
#    `count` is distributed. To begin with, plot a histogram for `count`.
#    To do this, you can use:
#       - `ggplot()` to initialize a ggplot object and specify variables to plot.
#       - `geom_histogram()` to draw a histogram.
#       - `labs()` to format the labels such that:
#         - `title = "Count of Fish Caught"`
#         - `subtitle = "A great many visitors caught no fish at all."`
#         - `x = "Fish Caught (n)"`
#         - `y = "Count (n)"`
#    Store the plot into a ggplot object `g1`.
## Do not modify this line!
library(ggplot2)
g1 <- ggplot(data)+
  geom_histogram(aes(count))+
  labs(title = "Count of Fish Caught",
       subtitle = "A great many visitors caught no fish at all.",
       x = "Fish Caught (n)",
       y = "Count (n)")+
  theme_light()
g1


# 3. Plot the empirical cumulative distribution (ECDF) for `count`.
#    To do this, you can use:
#       - `ggplot()` to initialize a ggplot object and specify variables to plot.
#       - `stat_ecdf()` to draw the ECDF.
#       - `labs()` to format the labels such that:
#         - `title = "Empirical Distribution of Fish Caught"`
#         - `subtitle = "Distribution is right-skewed, suggesting a Poisson distribution with a low mean."`
#         - `x = "Fish Caught (n)"`
#         - `y = "Count (n)"`
#    Store the plot into a ggplot object `g2`.
## Do not modify this line!
theme_set(theme_light())
g2 <- ggplot(data)+
  stat_ecdf(aes(count))+
  labs(title = "Empirical Distribution of Fish Caught",
       subtitle = "Distribution is right-skewed, suggesting a Poisson distribution with a low mean.",
       x = "Fish Caught (n)",
       y = "ECDF")
g2
# 4. The histogram and ECDF plots for `count` suggest a zero-inflated Poisson
#    (ZIP) distribution, which concerns a random event with excess zero-count
#    data in unit time. In our case, the number of fish caught was zero-inflated
#    by visitors who did not fish and thus caught no fish at all.
#    The ZIP model employs two parameters, pi and lambda. While pi corresponds
#    to a binary distribution that generates structural zeros, lambda corresponds
#    to the Poisson distribution that generates counts.
#    To find the ZIP model that best fits our data, we will derive the method
#    of moments estimators (MME) and maximum likelihood estimators (MLE) for
#    the two parameters.
#    Let's start with the MME.
#    (1) Define a function `par_pois()` that
#        - Takes an input vector `x`.
#        - Creates a vector `m` of `c(mean(x), var(x))`.
#        - Returns a vector of `c(lambda, pi)`, where `lambda` and `pi` are the
#          MMEs defined in equations 2-3 and 2-4 at
#          https://projecteuclid.org/download/pdf_1/euclid.involve/1513733747.
#          The two elements of the output should be named `lambda` and `pi`.
#    (2) Apply your function `par_pois()` to `cnt` and store the result into
#        a vector `cnt_pois_mme`.
#        To check your result, `round(cnt_pois_mme, 2)` prints to:
#             lambda     pi
#               4.31   0.56
## Do not modify this line!
par_pois <- function(x){
  m <- c(mean(x),var(x))
  return(c(lambda=m[1]+m[2]/m[1]-1,pi=(m[2]-m[1])/(m[1]^2+m[2]-m[1])))
}

cnt_pois_mme <- par_pois(cnt)
round(cnt_pois_mme, 2)
# 5. Unlike the MMEs, we cannot find the explicit expressions for the MLEs.
#    Instead, we will solve the MLEs by optimization.
#    Define a function `nll_pois_factory()` that
#       - Takes an input vector `x`.
#       - Creates
#           - An integer `n`, which is the length of `x`.
#           - An integer `y`, which is the number of elements in `x` taking the
#             value 0.
#           - A float `xbar`, which is the mean of `x`.
#       - Returns a function that
#         - Takes an input vector `par` of `c(lambda, pi)`.
#         - Returns a value for the negative log-likelihood function defined
#           in equation 2-8 at
#           https://projecteuclid.org/download/pdf_1/euclid.involve/1513733747.
#           - You can leave out the last term involving the factorial of `x`,
#             as it is not relevant when taking the gradients w.r.t. the two
#             parameters.
#    To check your result, `nll_pois_factory(cnt)(c(4, 0.5))` prints to:
#    [1] -48.84884
## Do not modify this line!
nll_pois_factory <- function(x){
  force(x)
  n <- length(x)
  y <- sum(x==0)
  xbar <-mean(x)
  function(par){
    -(y*log(par[2]+(1-par[2])*exp(-par[1]))+(n-y)*log(1-par[2])-(n-y)*par[1]+n*xbar*log(par[1]))
  }
}

nll_pois_factory(cnt)(c(4, 0.5))

# 6. Define a function `par_pois2()` that
#       - Takes an input vector `x`.
#       - Creates
#         - A vector `par0` by applying `par_pois()` to `x`.
#         - A function `nll_pois_x` by applying `nll_pois_factory()` to `x`.
#         - A list `fit` that stores the result of the optimizing `nll_pois_x`
#           over `par0`.
#           - To do this, you can use `optim()` with
#             - `method = "L-BFGS-B"`;
#             - `hessian = TRUE`;
#             - `lower = c(0.001, 0)`;
#             - `upper = c(Inf, 0.999)`.
#         - A Fisher Information matrix `fisher_info` from `fit$hessian` using
#           `solve()`.
#         - A vector `se` for the standard errors, which are the square roots
#           of the diagonal elements of `fisher_info`.
#       - Returns a tibble of size 2 x 5 with columns
#         - `parameter`: `lambda` and `pi`;
#         - `value`: the MLEs (i.e., `fit$par`);
#         - `se`: the standard errors;
#         - `lower` and `upper`: the bounds for the 95% confidence intervals.
#            You can use `1.96` for the 0.975% quantile of the standard normal
#            distribution.
#    Apply your function `par_pois2()` to `cnt` and store the result into a
#    tibble `fit_mle`.
#    To check your result, `fit_mle %>% mutate_at(2:5, round, 1)` prints to:
#    # A tibble: 2 x 5
#      parameter value    se lower upper
#      <chr>     <dbl> <dbl> <dbl> <dbl>
#    1 lambda      4.3   0.2   3.9   4.7
#    2 pi          0.6   0     0.5   0.6
## Do not modify this line!
par_pois2 <- function(x){
  force(x)
  par0 <- par_pois(x)
  nll_pois_x <-nll_pois_factory(x)
  fit <- optim(par0,nll_pois_x,method = "L-BFGS-B",hessian = T,lower = c(0.001, 0), upper = c(Inf, 0.999))
  fisher_info <- solve(fit$hessian)
  return(tibble(parameter = c("lambda","pi"),
                value = fit$par,
                se =sqrt(diag(fisher_info)),
                lower = fit$par-1.96*sqrt(diag(fisher_info)),
                upper = fit$par+1.96*sqrt(diag(fisher_info))))
}
fit_mle <-par_pois2(cnt) 
fit_mle%>% mutate_at(2:5, round, 1)

# 7. We can see that the standard errors are small, meaning that the predictions
#    are pretty accurate. To confirm the result, we will check the goodness of
#    fit by the qq-plot.
#    First, create a vector `cnt_pois_mle` of length 2 for the MLEs from
#    `fit_mle`, using methods such as `pull()`.
#    Then, create a list `dparams_mle` of two components, `mu` and `sigma`,
#    where `mu` corresponds to `cnt_pois_mle[0]` (i.e., `lambda`) and
#    `sigma` corresponds `cnt_pois_mle[1]` (i.e., `pi`).
#    Lastly, plot a qq-plot of actual vs. fitted distributions of `count` in
#    `data`.
#    To do this, you can use:
#       - `ggplot()` to initialize a ggplot object and specify variables to plot.
#       - `stat_qq()` to draw the qq-plot, setting
#         - `distribution = qZIP`.
#         - `dparams = dparams_mle`.
#       - `stat_qq_line()` to draw a qq-line for reference, setting
#         - `distribution = qZIP`.
#         - `dparams = dparams_mle`.
#         - `color = "red"`.
#       - `labs()` to format the labels such that:
#         - `title = "Good Fit Using MLEs"`
#         - `subtitle = "Distributions of sample and theoretical quantiles agree."`
#         - `x = "Theoretical Quantiles"`
#         - `y = "Sample Quantiles"`
#    Store the plot into a ggplot object `g3`.
#    We can see that the MLEs have pretty good fit for our data.
## Do not modify this line!
cnt_pois_mle <- pull(fit_mle,var = value)
dparams_mle <-list(mu=cnt_pois_mle[1], sigma = cnt_pois_mle[2])
g3 <- ggplot(data)+
  stat_qq(aes(sample=count),distribution = qZIP,dparams = dparams_mle)+
  stat_qq_line(aes(sample=count),distribution = qZIP,dparams = dparams_mle,color="red")+
  labs(title = "Good Fit Using MLEs",
       subtitle = "Distributions of sample and theoretical quantiles agree.",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")+
  theme_light()
g3


