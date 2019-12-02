# HW10: Monte Carlo Integration
#
# In this exercise, we will walk you through the process of using Monte Carlo
# method to approximate complicated integrals. There are three independent parts
# in this exercise: 1-2, 3-5 and 6-7.
# We suggest the functions you can use to create the tibbles and the plots, but
# you are free to use the methods you are the most comfortable with.
# Make sure that the outputs look exactly like the ones you are supposed to create.
#
# Throughout the exercise:
#    - Use `%>%` to structure your operations.
#    - Use `theme_light()` for the plots.
#    - Do NOT use `for`, `while` or `repeat` loops.
#    - When defining functions, you do not need to follow the exact process as
#      suggested by the instructions. However, make sure you are using the same
#      parameter settings, inputs and outputs in order to pass the test.
#
# 1. Load the package `tidyverse`.
#    Let X ~ Cauchy(0, 1) be a standard Cauchy random variable.
#    We would like to approximate the probability P(X > 2), i.e., the integral
#    of g(x) = `1 / (pi * (1 + x^2))` over x from 2 to inf.
#    First, let's compute the exact probability as a reference using `pcauchy()`
#    and store the result as a float `p1`.
#    Next, we will use Monte Carlo (MC) method to estimate the integral.
#    Consider f(x) as the p.d.f for Cauchy(0, 1). Then g(x) / f(x) is an
#    indicator function 1(X > 2).
#       - Create an integer `n` as `1e4`.
#       - Set the seed to `0` using `set.seed()`.
#         Store the seed `.Random.seed` as a vector `seed` of length 626.
#       - Draw `n` i.i.d. samples from Cauchy(0, 1) using `rcauchy()`.
#         Store the result as a vector `x_cauc`.
#       - Define a function `g_over_f1_cauc` that
#         - Takes an input vector `x`.
#         - Returns a vector that maps `x[i]` to `TRUE` if `x[i] > 2` and
#           `FALSE` otherwise.
#       - Compute the MC estimator and its standard error using `g_over_f1_cauc`.
#         Store the results as a vector `est1_cauc` of the form c(estimate, se).
#    To check your result,
#       - `g_over_f1_cauc(x1_cauc)[1:5]` prints to:
#         [1] FALSE FALSE  TRUE FALSE FALSE
#       - `round(est1_cauc, 4)` prints to:
#         [1] 0.1500 0.0036
## Do not modify this line!
library(tidyverse)
p1 <- 1-pcauchy(2)
n <- 1e4 
set.seed(0)
seed <- .Random.seed
x1_cauc <- rcauchy(n)
g_over_f1_cauc <- function(x) return(x>2)
a <- g_over_f1_cauc(x1_cauc)
est1_cauc <- c(mean(a), sqrt(var(a)/n))

# 2. To improve the precision of our estimator, we will use importance sampling.
#    Essentially, we want to decrease cases sampled out of `[2, inf)` to reduce
#    the variance of the estimator.
#    Consider X ~ Uniform(0, 2) with f(x) = 1 / 2.
#    Given that the Cauchy distribution is symmetric, we can write
#       - P(X > 2) = 1 - P(X < 2)
#                  = 0.5 - the integral of g(x) over x from 0 to 2.
#    Then, with f(x) = 1 / 2 and g(x) / f(x) = 2 / (pi * (1 + x^2)):
#       - Set the seed to `0` using `set.seed()`.
#         Store the seed `.Random.seed` as a vector `seed` of length 626.
#       - Draw `n` i.i.d. samples from Uniform(0, 2) using `runif()`.
#         Store the result as a vector `x2_cauc`.
#       - Define a function `g_over_f2_cauc` that
#         - Takes an input vector `x`.
#         - Returns a vector that maps `x[i]` to `g(x[i]) / f(x[i])`.
#       - Compute the MC estimator and its standard error using `g_over_f2_cauc`.
#         Note that you should use 0.5 - mean(g(x) / f(x)) as we have rewritten
#         the integral.
#         Store the results as a vector `est2_cauc` of the form c(estimate, se).
#    To check your result,
#       - `g_over_f2_cauc(x2_cauc)[1:5]` prints to:
#         [1] 0.1509915 0.4965913 0.4096903 0.2752779 0.1480730
#       - `round(est2_cauc, 4)` prints to:
#         [1] 0.1473 0.0017
#    We can see that `est2_cauc` is closer to `p1` with a reduced variance.
## Do not modify this line!
set.seed(0)
seed <- .Random.seed
x2_cauc <- runif(n, 0, 2)
g <- function(x) 1/(pi*(1+x^2))
g_over_f2_cauc <- function(x){
  return(2*g(x))
}

est2_cauc <- c((0.5-mean(g_over_f2_cauc(x2_cauc))),sqrt(var(0.5-g_over_f2_cauc(x2_cauc))/n))


# 3. Estimate the integral of `g(x) = exp(-x^2)` over x from 0 to 1.
#    First, let's compute the exact probability as a reference. We can show that
#    the integral = sqrt(pi) * P(0 < X < 1) when X ~ N(0, 1 / sqrt(2)).
#    Create a float `p2` to store the exact result using `pnorm()`.
#    Next, we will use MC to estimate the integral.
#    Consider X ~ Uniform(0, 1) with f(x) = 1. Then g(x) / f(x) = g(x).
#       - Create an integer `n` as `1e3`.
#       - Set the seed to `0` using `set.seed()`.
#         Store the seed `.Random.seed` as a vector `seed` of length 626.
#       - Draw `n` i.i.d. samples from Uniform(0, 1) using `runif()`.
#         Store the result as a vector `x_exp`.
#       - Define a function `g_over_f_exp` that
#         - Takes an input vector `x`.
#         - Returns a vector that maps `x[i]` to `g(x[i])`.
#       - Compute the MC estimator and its standard error using `g_over_f_exp`.
#         Store the results as a vector `est1_exp` of the form c(estimate, se).
#    To check your result,
#       - `g_over_f_exp(x_exp)[1:5]` prints to:
#         [1] 0.4475058 0.9319325 0.8706840 0.7202471 0.4383045
#       - `round(est1_exp, 4)` prints to:
#         [1] 0.7469 0.0064
## Do not modify this line!
p2 <- sqrt(pi)*(pnorm(1,0,1/sqrt(2))-pnorm(0,0,1/sqrt(2)))
n <- 1e3
set.seed(0)
seed <- .Random.seed
x_exp <- runif(n)
g_over_f_exp <- function(x) exp(-x^2)

est1_exp <- c(mean(g_over_f_exp(x_exp)),sqrt(var(g_over_f_exp(x_exp))/n))
# 4. To improve the precision of our estimator, we will first try antithetic
#    variates.
#    Consider X ~ Uniform(0, 1). Then X and 1 - X are antithetic variates.
#       - Compute the MC estimator and its standard error from `x_exp` and
#         `1 - x_exp` using `g_over_f_exp`.
#         Store the results as a vector `est2_exp` of the form c(estimate, se).
#       - Compute the correlation between g(x) and g(1 - x) using `cor()`.
#         Store the result as a float `cor1_exp`.
#    To check your result,
#       - `round(est2_exp, 4)` prints to:
#         [1] 0.7469 0.0009
#       - `round(cor1_exp, 4)` prints to
#         [1] -0.9595
#    We can see that the new estimator is more accurate with a reduced variance,
#    and the correlation between the antithetic variates is negative as expected.
## Do not modify this line!
g_over_f_anti_exp <- function(x) exp(-(1-x)^2)
estimate<-mean(c(g_over_f_exp(x_exp),g_over_f_anti_exp(x_exp)))
se <- sqrt(var(g_over_f_exp(x_exp)+g_over_f_anti_exp(x_exp))/(4*n))
est2_exp <- c(estimate, se)
cor1_exp<- cor(g_over_f_exp(x_exp),g_over_f_anti_exp(x_exp))
# 5. Next, we will try control variates to improve precision.
#    Consider the control variate h(x) = -x^2.
#       - Define a function `h` that
#         - Takes an input vector `x`.
#         - Returns a vector that maps `x[i]` to `h(x[i])`, where `h(x) = -x^2`.
#       - Create a float `c` by `-cov(g(x), h(x)) / var(h(x))`.
#       - Given that the integral of h(x) over x in [0, 1] is `- 1 / 3`, compute
#         the MC estimator and its standard error from `x_exp` using
#         `g_over_f_exp` and `h`.
#         Store the results as a vector `est3_exp` of the form c(estimate, se).
#       - Compute the correlation between g(x) and h(x) using `cor()`.
#         Store the result as a float `cor2_exp`.
#    To check your result,
#       - `h(x_exp)[1:5]` prints to:
#         [1] -0.80406587 -0.07049485 -0.13847620 -0.32816098 -0.82484139
#       - `round(est3_exp, 4)` prints to:
#         [1] 0.7473 0.0008
#       - `round(cor2_exp, 4)` prints to
#         [1] 0.9924
#    We can see that the new estimator is more accurate with a reduced variance,
#    and the correlation between `exp(-x^2)` and `-x^2` is large as expected.
## Do not modify this line!
h <- function(x) -x^2
c <- -cov(exp(-x_exp^2),h(x_exp))/var(h(x_exp))
estimate <- mean(g_over_f_exp(x_exp)+c*(h(x_exp)+1/3))
se <- sqrt(var(g_over_f_exp(x_exp)+c*(h(x_exp)+1/3))/n)
est3_exp <- c(estimate,se)
cor2_exp <- cor(exp(-x_exp^2),h(x_exp))
# 6. Compute the area of A = {(x, y): x^2 + y^2 <= 1}.
#    In other words, estimate the double integral of g(x, y) = 1 over x and y,
#    when the integration domain is restricted to A.
#    We know that A is a circle centered at the origin with radius = 1, so its
#    area would be pi, but let's perform a MC integration for practice.
#    Consider two independent random variables X ~ Uniform(-1, 1) and
#    Y ~ Uniform(-1, 1), with joint p.d.f.:
#       - f(x, y) = 1 / 4, for -1 <= x <= 1 and -1 <= y <= 1.
#    Then g(x, y) / f(x, y) is 4 * 1(x^2 + y^2 <= 1), where 1(x^2 + y^2 <= 1)
#    is an indicator function.
#       - Create an integer `n` as `1e4`.
#       - Set the seed to `0` using `set.seed()`.
#         Store the seed `.Random.seed` as a vector `seed_x` of length 626.
#       - Draw `n` i.i.d. samples from Uniform(-1, 1) using `runif()`.
#         Store the result as a vector `x_circ`.
#       - Set the seed to `1` using `set.seed()`.
#         Store the seed `.Random.seed` as a vector `seed_y` of length 626.
#       - Draw `n` i.i.d. samples from Uniform(-1, 1) using `runif()`.
#         Store the result as a vector `y_circ`.
#       - Define a function `g_over_f_circ` that
#         - Takes input vectors `x` and `y`.
#         - Returns a vector that maps `(x[i], y[i])` to `4` if
#           `(x[i])^2 + (y[i])^2 <= 1` and `0` otherwise.
#       - Compute the MC estimator and its standard error using `g_over_f_circ`.
#         Store the results as a vector `est_circ` of the form c(estimate, se).
#    To check your result,
#       - `g_over_f_circ(x_circ, y_circ)[1:5]` prints to
#         [1] 4 4 4 4 0
#       - `round(est_circ, 4)` prints to:
#         [1] 3.1068 0.0167
## Do not modify this line!
n <- 1e4
set.seed(0)
seed_x <- .Random.seed
x_circ <- runif(n, -1, 1)
set.seed(1)
seed_y <- .Random.seed
y_circ <- runif(n, -1, 1)
g_over_f_circ <- function(x, y) ifelse(x^2+y^2<=1,4,0)
est_circ <- c(mean(g_over_f_circ(x_circ, y_circ)),sqrt(var(g_over_f_circ(x_circ, y_circ))/n))

# 7. Our final step is to visualize the MC integration.
#    (1) Create a tibble `circ` of size 10000 x 3 with columns `x_circ`,
#        `y_circ` and `selected`, where `selected` is a boolean column indicating
#        whether the pair (x_circ, y_circ) is in the circle `x^2 + y^2 <= 1`.
#        To do this, you can use:
#           - `tibble()` to create a tibble from `x_circ` and `y_circ`.
#           - `mutate()` to add a column `selected`, by applying `g_over_f_circ`
#             to `x_circ` and `y_circ` and checking if the values equal 4.
#        To check your result, `circ` prints to:
#          x_circ y_circ selected
#           <dbl>  <dbl> <lgl>
#        1  0.793 -0.469 TRUE
#        2 -0.469 -0.256 TRUE
#        3 -0.256  0.146 TRUE
#        4  0.146  0.816 TRUE
#        5  0.816 -0.597 FALSE
#        # … with 9,995 more rows
#    (2) Draw a scatterplot for `y_circ` vs. `x_circ`. Color the scatterplot by
#        `selected`.
#        To do this, you can use:
#           - `ggplot()` to initialize a ggplot object and specify variables to plot.
#           - `geom_point()` to draw the scatterplot, setting `shape = 20`.
#           - `stat_function` to plot
#             - The upper circle `function(x) sqrt(1 - x^2)`, setting `size = 1.5`.
#             - The lower circle `function(x) -sqrt(1 - x^2)`, setting `size = 1.5`.
#           - `coord_fixed()` to fix the aspect.
#           - `labs()` to format the labels such that:
#             - `title = "Monte Carlo Integration of x^2 + y^2 <= 1"`
#             - `x = "x"`
#             - `y = "y"`
#        Store the plot as a ggplot object `g`.
## Do not modify this line!
circ <- tibble(x_circ=x_circ,y_circ=y_circ)%>%
  mutate(selected= g_over_f_circ(x_circ, y_circ)==4)
g <- ggplot(circ)+
  geom_point(aes(x_circ,y_circ,color=selected),shape=20)+
  stat_function(aes(x_circ,y_circ),fun = ~sqrt(1-.x^2),size=1.5)+
  stat_function(aes(x_circ,y_circ),fun = ~-sqrt(1-.x^2),size=1.5)+
  coord_fixed()+
  labs(title = "Monte Carlo Integration of x^2 + y^2 <= 1",
       x="x",
       y="y")+
  theme_light()
g

