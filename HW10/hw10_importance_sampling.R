# HW10: Importance sampling in Monte Carlo integration
#
# In this exercise, we will go over a simple application of monte carlo
# estimation of an integral. We will estimate the standard normal CDF
# between 1.5 and 2.5, namely `P(x > 1.5 & x < 2.5)` where `x ~ norm(0, 1)`.
# We can't derive the integral manually, thus we will use monte carlo estimation.
# The idea of Importance Sampling is to sample from a second distribution that
# will generate most of the samples in the desired interval, to have a fast
# convergence and find an appropriate acceptance rate to get the correct
# distribution approximation.
#
# 1. Create a function `in_set <- function(x, minx, maxx)` that takes as input
#    a numeric vector `x` and the limit `minx` and `maxx` and returns a vector
#    of `1` or `0` depending on whether the corresponding element of `x` falls
#    into the interval `[minx, maxx]` os not.
#    (It returns numeric `0` if not in this interval and `1` otherwise)
#    Example : `in_set(c(1, 2), 2, 2.5)` is `c(0, 1)`.
#    Create a function `norm_density <- function(x)` that takes as input
#    one scalar `x` and returns the standard normal density of that value.
#    Example : `norm_density(2) = 0.05399097`.
#    Create a function `g <- function(x)` that takes as input
#    one scalar `x` and returns `in_set(x, 1.5, 2.5) * norm_density(x)`.
#    Example : `g(2) = 0.05399097`.
## Do not modify this line!
in_set <- function(x, minx, maxx) ifelse(between(x, minx, maxx),1,0)
norm_density <- function(x) dnorm(x)
g <- function(x) in_set(x, 1.5, 2.5)*norm_density(x)
# 2. Load the `tidyverse`, `tibble` and `ggplot2` packages.
#    Use `set.seed()` to set seed to `0` and save it into `seed1` using
#    `.Random.seed`. Use samples from standard normal to
#    estimate the cumulative distribution function between 1.5 and 2.5.
#    Create a tibble called `data` with three columns:
#      - `x` are 1000 samples generated randomly from standard normal using `rnorm`
#      - `y`, that is equal to 1 if `x` is in `[1.5, 2.5]` and 0 otherwise.
#        (you can use `in_set()`)
#      - `z` the density evaluated at point `x`. (you can use `norm_density`)
#      - inside `labs`, set `x` to "X", `y` to `"Density Function"` and `title`
#        to `"Standard Normal density"`, `subtitle` to
#        `"Integral interval shown in red"`.
#    `data` should print to:
#    # A tibble: 1,000 x 3
#             x     y      z
#         <dbl> <int>  <dbl>
#    1  1.26        0 0.180
#    2 -0.326       0 0.378
#    3  1.33        0 0.165
#    4  1.27        0 0.178
#    5  0.415       0 0.366
#    6 -1.54        0 0.122
#    7 -0.929       0 0.259
#    8 -0.295       0 0.382
#    9 -0.00577     0 0.399
#    10  2.40        1 0.0221
#    # … with 990 more rows
#    Estimate the integral value by calculating the mean of `data$y` and
#    save the result to `cdf_estimate`. Save the standard deviation of
#    `data$y` to `sd_estimate`.
#    (As we are using the same distribution as the function,
#    the integral is an approximation of `P(1.5 < x < 2.5))`
#    Calculate the true value by using difference of `pnorm()` at 2.5 and 1.5
#    and save it into `cdf_gold`. Compare our estimated `cdf_estimate` and
#    `cdf_gold`. We can notice that the variance of the estimate `sd_estimate`
#    is pretty high.
## Do not modify this line!
set.seed(0)
seed1 <- .Random.seed
data <- tibble(x=rnorm(1000), y = in_set(x, 1.5, 2.5), z = norm_density(x))
cdf_estimate <- mean(data$y)
sd_estimate <- sqrt(var(data$y))
cdf_gold <- pnorm(2.5)-pnorm(1.5)

# 3. Plot the density function of standard normal to explore why this would happen.
#    To plot the figure :
#     - use `ggplot()` to initialize the ggplot object on `data`.
#     - use `geom_line()` with `mapping` set to `aes(x = x, y = z)` to draw
#       the full normal plot.
#     - use `geom_line()` with `data` `filter()` by `y == 1`, with `mapping`
#       set to `aes(x = x, y = z)` and `color` set to `red` to highlight the
#       area we want to take integral of.
#     - inside `labs()`, set `x` to `"X"`, `y` to `"Density Function"`,
#       `title` to `"Standard Normal density"` and `subtitle` to
#       `"Integral interval shown in red"`.
#     - use `theme_light()`.
#    Save the plot to `norm_plot`.
#    Then we can see the reason why the variance is high when sampling from
#    `norm(0, 1)` : we have relatively low probability to get samples
#    within `(1.5, 2.5)` range.
#    Thus, the value tend to vary a lot (and it has high standard deviation).
#    We should use another distribution which has a higher concentration over
#    the range `(1.5, 2.5)`.
## Do not modify this line!
norm_plot <- ggplot(data)+
  geom_line(aes(x,z))+
  geom_line(data = data%>%filter(y==1),mapping = aes(x, z),color="red")+
  labs(x="X",
       y="Density Function",
       title = "Standard Normal density",
       subtitle = "Integral interval shown in red")+
  theme_light()
norm_plot

# 4. Now, we will explore this effect by using three different distributions
#    and use Importance Sampling to estimate the integral.
#    - set `n` to 1e4.
#    - Use `set.seed()` to set seed to `0` and save seed to `seed2` using
#    `.Random.seed`.
#    - Generate `n` samples from `uniform(1.5, 2.5)` and save it to `uniform`.
#    - Use `set.seed()` to set seed to `0` and save seed to `seed3` using
#    `.Random.seed`.
#    - Generate `n` samples from `normal(0, 1)` and save them into `original`.
#    - Generate tibble `fit` using `tribble()`, inside which:
#      - set three formula `~x` to represent our samples, `~name` to
#        record the distribution, `~g` to caculate the corresponding density
#        value.
#      - then add these rows by specifying the values:
#             `uniform`, `"uniform"`, `dunif(uniform, 1.5, 2.5)`,
#             `original`, `"original"`, `dnorm(original, 0, 1)`
#    - Use `mutate` to create column `g_over_f` using `map2()` and `g()` on
#      columns `x` and `f`.
#    Save the result into `fit`. It should print to :
#    # A tibble: 2 x 4
#    x              name     f              g_over_f
#    <list>         <chr>    <list>         <list>
#    1 <dbl [10,000]> uniform  <dbl [1]>      <dbl [10,000]>
#    2 <dbl [10,000]> original <dbl [10,000]> <dbl [10,000]>                                                                                   3 <dbl [1,… uniform(1.… <dbl [1,… <int [1,… <dbl [1… <dbl [1… <dbl [1…                                                                                                                                                                                 3 <dbl [1,000]> uniform(1.5, 2.… <dbl [1,000… <int [1,000… <dbl [1,000… <dbl [1,000… <dbl [1,000…
## Do not modify this line!
n <- 1e4
set.seed(0)
seed2 <- .Random.seed
uniform <- runif(n, 1.5, 2.5)
set.seed(0)
seed3 <- .Random.seed
original <- rnorm(n)
fit <- tribble(~x,~name,~f,
               uniform, "uniform", 1,
               original, "original", dnorm(original, 0, 1))%>%
  mutate(g_over_f = map2(x,f,function(x,y) g(x)/y))
fit

# 5. Calculate the expectation of `f(x)/g(x)` over distribution `g` by calculating
#    the mean of column `z` for all of our samples.
#    To do that, use `transmute()` to create new columns:
#      - use `mean()` and `map_dbl()` to generate
#        column `mean` recording the estimated integral under each distribution,
#        and `sd() / sqrt(n)` and `map_dbl()` to calculate column `se` which
#        is the variance of `f(x)/g(x)` under each set of samples.
#      - create column `upper` by setting it to `mean + 1.96 * se` and `lower`
#        by setting to `mean - 1.96 * se`.
#    Save the result tibble into `result` and it should print to:
#    # A tibble: 2 x 4
#        mean       se  lower  upper
#       <dbl>    <dbl>  <dbl>  <dbl>
#    1 0.0607 0.000326 0.0600 0.0613
#    2 0.0613 0.00240  0.0566 0.0660#'
## Do not modify this line!
result <- fit%>%
  transmute(mean = map_dbl(g_over_f,mean),
            se = map_dbl(g_over_f,~sd(.x)/sqrt(n)))%>%
  mutate(lower = mean-se*1.96,
         upper = mean+se*1.96)
result

# 6. We will notice that for sample distribution from `uniform(1.5, 2.5)`,
#    we have smaller variance and closer estimate.
#    To explore the effect of different uniform intervals on estimation variance,
#    we will calculate the estimation for different uniform intervals centered
#    around 2.
#    - generate a sequence of possible uniform interval `width` using `seq()`
#      ranging from 0.1 to 3 with interval 1e-2.
#    - create a function `generate_sample <- function(w, seed = 0, n = 1e4)`,
#      in which `w` represents the uniform interval width, `seed` represents the
#      seed number, and `n` represents the sample size.
#      Inside the function:
#        - first set the seed to `seed` by `set.seed()`.
#        - return `n` samples using `runif()` and set `min` to `2 - w/2`, `max` to
#          `2 + w/2`. (The function will return the corresponding samples from the
#          uniform distribution specified by width `w`).
#    - create tibble `subsamples`, in which each row represents a different sample
#      size:
#      - use `tribble()`, set three formula `~width` to represent our sample size,
#        `~uniform` to record the uniform samples.
#      - then specify each row by `width` and its corresponding samples by `map`
#        and `generate_sample`.
#      - `unnest()` the tibble by `c(width, samples)`.
#      - use `mutate()` and `map2()` to calculate weighted sample values `g_over_f`
#        for each interval `width` and corresponding `samples` by customising
#      `function(width, sample) g(sample) / dunif(sample, 2 - width/2, 2 + with/2)`
#      - calculate the estimation result by `transmute()` to create new columns:
#        - use `mean()` and `map_dbl()` to generate column `mean` recording the
#          estimated integral under each distribution, and `sd() / sqrt(n)` and
#          `map_dbl()` to calculate column `se` which
#          is the variance of `f(x)/g(x)` under each set of samples.
#        - create column `upper` by setting it to `mean + 1.96 * se` and `lower`
#          by setting to `mean - 1.96 * se`.
#    Save the tibble to `result2`. it should print to:
#    # A tibble: 201 x 4
#        mean       se  lower  upper
#       <dbl>    <dbl>  <dbl>  <dbl>
#    1 0.0607 0.000326 0.0600 0.0613
#    2 0.0606 0.000333 0.0599 0.0612
#    3 0.0606 0.000341 0.0599 0.0613
#    4 0.0605 0.000348 0.0598 0.0612
#    5 0.0604 0.000354 0.0597 0.0611
#    6 0.0604 0.000361 0.0597 0.0611
#    7 0.0604 0.000368 0.0597 0.0611
#    8 0.0604 0.000374 0.0597 0.0611
#    9 0.0603 0.000380 0.0596 0.0611
#    10 0.0604 0.000386 0.0596 0.0612
#    # … with 191 more rows
## Do not modify this line!
width <- seq(1,3,by=1e-2)
generate_sample <- function(w, seed = 0, n = 1e4){
  set.seed(seed)
  runif(n, 2-w/2,2+w/2)
}
subsamples <- tribble(~width,~samples,
                      width, map(width,generate_sample))%>%
  unnest(c(width,samples))%>%
  mutate(g_over_f = map2(width,samples,function(width, sample) g(sample) / dunif(sample, 2 - width/2, 2 + width/2)))


result2 <- subsamples%>%
  transmute(mean = map_dbl(g_over_f,mean),
            se = map_dbl(g_over_f,~sd(.x)/sqrt(n)))%>%
  mutate(lower = mean-1.96*se,
         upper= mean+1.96*se)

# 7. Next, we can visualize the variance trend as the interval width changes.
#     - use `ggplot` to initialize the plot object over `result2`. Set `mapping`
#       to `aes(y = mean, x = width)`.
#     - use `geom_line` to plot variance curves.
#     - use `geom_ribbon()` to draw standard deviation shade to the plot,
#       set `mapping` to `aes(ymin = lower, ymax = upper)`, set `alpha`
#       to 0.2 and `fill` to `"orange"`.
#     - add gold line by `geom_line` with `mapping` set to
#       `aes(y = cdf_gold, x = width)` and `color` set to "red".
#     - use `scale_x_reverse()` to reverse the axis.
#     - inside `labs`, set `title` to `"MC estimate for different uniform interval"`,
#       set `subtitle` to `"Variance decreases with the interval width"`,
#       `x` to `"Interval Width"` and `y` to `"MC Estimate"`.
#      - use `theme_light()`.
#    Save the plot into `variance_plot`.
## Do not modify this line!
variance_plot <- ggplot(result2)+
  geom_line(aes(y=mean,x=width))+
  geom_ribbon(aes(x=width,ymin=lower,ymax=upper),alpha=.2,fill='orange')+
  geom_line(aes(y=cdf_gold,x=width),color="red")+
  scale_x_reverse()+
  labs(title = "MC estimate for different uniform interval",
       subtitle = "Variance decreases with the interval width",
       x="Interval Width",
       y = "MC Estimate")+
  theme_light()
variance_plot


