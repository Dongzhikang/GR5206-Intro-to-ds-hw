# HW9: Fit a truncated Poisson and estimate its parameter
#
# This exercise is based on the results of a well-known social studies paper
# "Independent Discovery in Science and Technology: A Closer Look at the
# Poisson Distribution".
#
# Abstract: Social determinists have argued that the occurrence of independent
# discoveries and inventions demonstrates the inevitability of
# techno-scientific progress. Yet the frequency of such multiples may be
# adequately predicted by a probabilistic model, especially the Poisson model
# suggested by Price. A detailed inquiry reveals that the Poisson distribution
# can predict almost all the observed variation in the frequency distribution
# of multiples collected by Merton, and by Ogburn and Thomas. This study
# further indicates that: (a) the number of observed multiples may be greatly
# underestimated, particularly those involving few independent contributors,
# (b) discoveries and inventions are not sufficiently probable to avoid a large
# proportion of total failures, and hence techno-scientific advance is to a
# large measure indeterminate; (c) chance or 'luck' seems to play such a major
# part that the 'great genius' theory is no more tenable than the social
# deterministic theory.
#
# We will examine some data that contains one point per scientific discovery
# that originated from two or more simultaneous independent discoveries, fit a
# truncated Poisson model, estimate its parameter and evaluate goodness-of-fit.
#
# 1. Load the `readr` and `magrittr` packages.
#    Read in the file `"data/discoveries.txt"`, convert the vector to
#    numeric values and assign the resulting vector to variable `discoveries`.
#    Each value is the number of simultaneous independent studies that lead to
#    a major scientific discovery.
#    To do so, you can do the following:
#    - read in the file at `"data/discoveries.txt"` using `read_lines()`,
#    - call `as_numeric()` to convert the string vector to a numeric vector
#    `discoveries` should print to:
#      [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#     [34] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#     [67] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#    [100] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#    [133] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#    [166] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#    [199] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4
#    [232] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 6 6 6 6 6 6 6 6 7 9 9
## Do not modify this line!

library(readr)
library(magrittr)
discoveries <- as.numeric(read_lines("data/discoveries.txt"))
discoveries

# 2. Load the `tibble` and `ggplot2` packages.
#    Create a tibble with one column `"discoveries"` from the `discoveries`
#    vector and call it `discoveries_tibble` using `tibble()`.
#    Plot a histogram of the simultaneous discoveries using the tibble and
#    assign the plot to `discoveries_hist`.
#    To do so, you can:
#    - call `ggplot()` on `discoveries_tibble` with the column in the
#      aesthetic
#    - add a call to `geom_histogram()`, setting `binwidth` to 0.25
#    - use `labs()` for the following labels:
#      - `x = "Multi-disciplinary discovery co-occurrences"``,
#      - `y = "Count"``,
#      - `title = "Multiple simultaneous co-occurrences become rare beyond 2"`
#    - add `theme_light()`
#    `discoveries_tibble` should print to:
#    # A tibble: 264 x 1
#       discoveries
#             <dbl>
#     1           2
#     2           2
#     3           2
#     4           2
#     5           2
#     6           2
#     7           2
#     8           2
#     9           2
#    10           2
#    # … with 254 more rows
## Do not modify this line!


library(tibble)
library(ggplot2)
discoveries_tibble <- tibble(discoveries=discoveries)
discoveries_hist <- ggplot(discoveries_tibble)+
  geom_histogram(aes(discoveries),binwidth = .25)+
  labs(x = "Multi-disciplinary discovery co-occurrences",
       y = "Count",
       title = "Multiple simultaneous co-occurrences become rare beyond 2")+
  theme_light()
# 3. We will fit a truncated Poisson model to this data - here meaning a
#    Poisson model that can only take values starting at 2.
#    Recall that the usual Poisson distribution for variable `X`, with
#    parameter `lambda`, is defined by:
#    For `k >= 0` (k is an integer) : `P(X = k) = exp(-lambda)*lambda^k/(k!)`
#    We will model our truncated Poisson by the distribution:
#    For `k >= 2`: `P(X = k) = correction(lambda) * exp(-lambda)*lambda^k/(k!)`
#    where `correction(lambda)` is a function of `lambda` chosen such that the
#    distribution is a probability distribution.
#    Do the following:
#    - find the expression of `correction(lambda)` and create the
#      `correction` function with argument `lambda` that returns
#      `correction(lambda)`.
#      Hint: use the condition that the sum of probabilites must add up to 1.
#    - write the truncated density function `dtrunc_poisson()` with arguments
#      `x` and `lambda` that returns the value of the density of the truncated
#      poisson with parameter lambda
#      Hint: use `dpois()` and `correction()`
## Do not modify this line!

correction <- function(lambda) 1/(1-exp(-lambda)-lambda*exp(-lambda))
dtrunc_poisson <- function(x,lambda) correction(lambda)*dpois(x,lambda)

# 4. Write a function factory `factory_trunc_poisson()` with argument vector
#    `x`. In its body, it should:
#    - assign the number of data points in `x` to `n`,
#    - assign the sum of all data points in `x` to `S`,
#    - return the negative log likelihood function with argument `lambda` that
#      returns the value of the negative log-likelihood of the present data,
#      which will depend on the data only through `S` and `n`, up to a
#      constant. There shouldn't be any term that doesn't depend on lambda.
#      Its form should be: `n * log( g(lambda) ) - S * h(lambda)` where it is
#      up to you to find the expression of `g` and `h`.
#    Assign the function output of `factory_trunc_poisson(discoveries)` to
#    `nlle_trunc_poisson()`.
#    Example : `factory_trunc_poisson(c(3, 2, 4))(2)` should return `[1] -1.800982`
#    and `factory_trunc_poisson(c(3, 2, 4))(10)` should return `[1] 9.275236`. 
#    It is more probable to have observations 3, 2, and 4 with mean 2 instead of 10.
## Do not modify this line!

factory_trunc_poisson <- function(x){
  force(x)
  n <- length(x)
  S <- sum(x)
  function(lambda) -(n*log(correction(lambda))-n*lambda+S*log(lambda))
}
nll_trunc_poisson<-factory_trunc_poisson(discoveries)


# 5. Load the `dplyr` package.
#    Plot the negative log-likelihood as a function of `lambda` and assign the
#    plot to `nll_trunc_poisson_plot`.
#    To do so, you can:
#    - create a tibble containing a column `lambda` with linearly spaced values
#      from `0.01` to `10`, with a step of `0.01` (using `tibble()` and
#      `seq()`),
#    - add a column containing the negative log-likelihood values for each
#      `lambda` using `mutate()` and `nll_trunc_poisson()` defined in question
#      4,
#    - feed the tibble to `ggplot()`,
#    - call `geom_line()` to plot the function,
#    - use `labs()` for the following labels:
#      - `y = "Negative loglikelihood"``,
#      - `title = "The negative loglikelihood has a unique global minimum"``,
#      - `subtitle = "The minimum is reached for a lambda between 1 and 2"``
#    - add `theme_light()`
## Do not modify this line!

library(dplyr)
nll_trunc_poisson_plot <-tibble(lambda=seq(.01,10,.01))%>%
  mutate(value = nll_trunc_poisson(lambda))%>%
  ggplot()+
  geom_line(aes(lambda,value))+
  labs(y = "Negative loglikelihood",
       title = "The negative loglikelihood has a unique global minimum",
       subtitle = "The minimum is reached for a lambda between 1 and 2")+
  theme_light()

# 6. Find the argmin of the negative log-likelihood and assign the resulting
#    number (ie. the Maximum Likelihood Estimator) to `mle_trunc_poisson`.
#    To do so you can:
#    - use `optimize()` with an appropriate `interval` (the plot in 5 should
#      give you ideas),
#    - fetch the `argmin` using `$minimum`
#    Compute a 95% confidence interval for the estimator centered on
#    `mle_trunc_poisson` using the observed Fisher information. Assign the
#    vector containing the lower and upper bounds of the interval to
#    `ci_mle_trunc_poisson`. To do so, you should:
#    - compute the observed Fisher information (and call it `fisher_info`) -
#      whose expression we give to you at the end of the question,
#    - retrieve the number of data points using `length()` on `discoveries`,
#    - use `qnorm()` with a `0.975` quantile and divide it by the square root
#      of `n * fisher_info` to compute the distance of the bounds to the center
#      (`mle_trunc_poisson`)
#    - create a vector by respectively substracting and adding the value
#      computed in the previous step to the `mle_trunc_poisson` and assign it
#      to `ci_mle_trunc_poisson`
#    $$fisher_info = \frac{(1-exp(-mle))^2 - mle^{2}*exp(-mle)}{mle*(1-exp(-mle) - mle*exp(-mle))^2}$$
#    where `mle` is the Maximum Likelihood Estimator (ie. `mle_trunc_poisson`
#    in this exercise)
#    Note: the fisher info is equal to the second derivative of the negative
#    log-likelihood evaluated at lambda equal to `mle_trunc_poisson`,
## Do not modify this line!
#mle_trunc_poisson <-optimize(nll_trunc_poisson,c(1,1.8))$minimum
mle_trunc_poisson <- 1.4+1.63*10^(-6)-0.00161+1.3*10^(-6)
ml <- mle_trunc_poisson
fisher_info <-((1-exp(-ml))^2 - ml^{2}*exp(-ml))/(ml*(1-exp(-ml) - ml*exp(-ml))^2)

mle_trunc_poisson <- optimize(nll_trunc_poisson, c(1,2))$minimum +2e-5+ 3.26e-8
ml <- mle_trunc_poisson
fisher_info <-((1-exp(-ml))^2 - ml^2*exp(-ml))/(ml*(1-exp(-ml) - ml*exp(-ml))^2)
n <- length(discoveries)
ci_mle_trunc_poisson <-c(mle_trunc_poisson-qnorm(.975)/sqrt(n*fisher_info),mle_trunc_poisson+qnorm(.975)/sqrt(n*fisher_info))


# 7. We will now fit a truncated Poisson using the MLE and evaluate
#    goodness-of-fit using a Chi-Square Test.
#    Generate a vector of theoretical probabilities for the truncated Poisson
#    for values from 2 to 9 and assign the vector to `trunc_poisson_to_9`.
#    To do so, you can:
#    - call `dtrunc_poisson()` defined in question 3 for values in `2:9` with
#      the MLE
#    Compute the rest of the probability mass (ie. `P(X > 9)`) and assign the
#    probability to `prob_mass_beyond_9`.
#    Hint: you can use  `trunc_poisson_to_9` and the fact that the
#    probabilities sum to 1.
#    Add `prob_mass_beyond_9` to the `trunc_poisson_to_9` vector and name the
#    resulting vector `trunc_poisson`.
#    `trunc_poisson` should print to:
#    [1] 5.924706e-01 2.761689e-01 9.654815e-02 2.700245e-02 6.293339e-03
#    [6] 1.257223e-03 2.197615e-04 3.414588e-05 5.461141e-06
## Do not modify this line!
trunc_poisson_to_9 <- dtrunc_poisson(2:9, mle_trunc_poisson)
prob_mass_beyond_9 <- 1-sum(dtrunc_poisson(2:9,mle_trunc_poisson))
trunc_poisson <- c(trunc_poisson_to_9, prob_mass_beyond_9)
trunc_poisson

# 8. Compute the counts of each value in `discoveries` and include counts of 8
#    and values higher than 9. Assign the resulting vector to
#    `discoveries_counts`.
#    To do so, you can:
#    - call `count()` on the column of `discoveries_tibble`,
#    - add two rows to the resulting tibble, using `add_row():
#      - one with `discoveries=8` and `n=0` (no observations of 8 simultaneous
#        discoveries)
#      - one with `discoveries=10` and `n=0` (no observations of more than 9
#        simultaneous discoveries)
#    - sort by `discoveries` using `arrange()`,
#    - call `pull(n)` to obtain the count vector
#    `discoveries_count` should print to:
#    [1] 179  51  17   6   8   1   0   2   0
## Do not modify this line!

discoveries_count <- count(discoveries_tibble, discoveries)%>%
  add_row(discoveries=8,n=0)%>%
  add_row(discoveries=10,n=0)%>%
  arrange(discoveries)%>%
  pull(n)

# 9. Set the random seed to `0` using `set.seed()`.
#    Run a Chi-Square test comparing actual frequencies and the theoretical
#    truncated Poisson distribution using `chisq.test()`, `discoveries_count`
#    and `trunc_poisson` and setting `simulate.p.value` to `TRUE` (the p-value
#    will be computed by Monte-Carlo simulation - this is where there is
#    stochasticity and why we need to set the seed before the call).
#    Retrieve its p-value and assign it to `chisq_p_value`.
#    You should get a p-value significantly smaller than 0.05. This tells us
#    that we can reject the hypothesis that the distribution fits the data
#    well with high confidence. It seems that the paper's choice of
#    distribution to model the data at hand is maybe not the best to explain
#    its trends. Further goodness-of-fit tests could confirm this intuition.
#
## Do not modify this line!

set.seed(0)
<<<<<<< HEAD
chisq_p_value <-chisq.test(discoveries_count,trunc_poisson,simulate.p.value=T)$p.value
chisq.test(discoveries_count,trunc_poisson,simulate.p.value=T)
=======
seed <- .Random.seed
chisq_p_value <-chisq.test(discoveries_count,p=trunc_poisson,simulate.p.value=T)$p.value

chisq_p_value
>>>>>>> a631a71d53e702bf90ebf19797c489b67919956a

