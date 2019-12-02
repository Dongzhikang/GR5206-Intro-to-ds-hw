# HW10: LCG for generating distributions
#
# In this exercise, we will walk you through the process of using LCGs to generate
# distributions.
# We suggest the functions you can use to create the tibbles and the plots, but
# you are free to use the methods you are the most comfortable with.
# Make sure that the outputs look exactly like the ones you are supposed to create.
#
# Throughout the exercise:
#    - Use `%>%` to structure your operations.
#    - Use `theme_light()` for the plots.
#    - Do NOT use `for`, `while` or `repeat` loops except for question 1.
#    - When defining functions, you do not need to follow the exact process as
#      suggested by the instructions. However, make sure you are using the same
#      parameter settings, inputs and outputs in order to pass the test.
#
# 1. Load the package `tidyverse`.
#    In this exercise, we will create an LCG to simulate a Uniform distribution
#    and utilize it to simulate some other distributions.
#    First, define a function `lcg_unif` to simulate a Uniform Distribution in
#    [0, 1] using the recurrence relation formula, $X_n = (aX_n − 1 + c) mod m$.
#    `lcg_unif` should:
#       - Take inputs
#         - `n`, the number of samples to generate.
#         - `x`, defaulted to `2^32 - 1`.
#         - `m`, defaulted to `2^32`.
#         - `a`, defaulted to `1103515245`.
#         - `c`, defaulted to `12345`.
#       - Create a vector `rng` of length `n`.
#       - Loop over 1 to `n` to update
#         - `x` using the formula.
#         - `rng[i]` by `x / m`.
#       - Return the vector `rng` for the simulated samples.
#    Use `lcg_unif()` to generate 5000 samples from the Uniform(0, 1) and
#    store the result into a vector `unif_rng`.
#    To check your result, `head(unif_rng, 5)` prints to:
#    [1] 0.74307060 0.87848759 0.77028656 0.04326916 0.83987619
## Do not modify this line!
library(tidyverse)
lcg_unif <- function(n, x=2^32-1, m=2^32, a=1103515245, c=12345){
  rng <-vector(length = n)
  for (i in 1:n) {
    x <- (a*x+c)%%m
    rng[i] <- x/m
  }
  return(rng)
}

unif_rng <- lcg_unif(5000)
head(unif_rng, 5)

# 2. To check if our parameter choices are good, we would like to draw a
#    scatterplot for successive pairs (X[i], X[i + 1]) of the samples generated
#    by our LCG. Ideally, these pairs should be independent and should fill the
#    entire sample space.
#    (1) Create a tibble `unif_pairs` of size 4999 x 2 with columns `X1` and
#        `X2`, where `X1` is `unif_rng[1:4999]` and `X2` is `unif_rng[2:5000]`.
#        To do this, you can use:
#           - `tibble()` to make a tibble for `unif_rng[1:4999]` and
#             `unif_rng[2:5000]`.
#           - `rename()` to rename the columns as `X1` and `X2`.
#        To check your result, `unif_pairs` prints to:
#        # A tibble: 4,999 x 2
#             X1     X2
#           <dbl>  <dbl>
#        1 0.743  0.878
#        2 0.878  0.770
#        3 0.770  0.0433
#        4 0.0433 0.840
#        5 0.840  0.388
#        # … with 4,994 more rows
#    (2) Draw the scatterplot for the successive pairs using `unif_pairs`.
#        To do this, you can use:
#           - `ggplot()` to initialize a ggplot object and specify variables to plot.
#           - `geom_point()` to draw the scatterplot.
#           - `labs()` to format the labels such that:
#             - `title = "Successive Pairs of Simulated X ~ Unif(0, 1)"`
#             - `subtitle = "Good generator as pairs are independent and uniformly distributed."`
#             - `x = "X_n"`
#             - `y = "X_{n+1}"`
#        Store the plot into a ggplot object `g1`.
## Do not modify this line!
unif_pairs <- tibble(X1 = unif_rng[1:4999],
                     X2 = unif_rng[2:5000])
unif_pairs
g1 <- ggplot(unif_pairs)+
  geom_point(aes(X1,X2))+
  labs(title = "Successive Pairs of Simulated X ~ Unif(0, 1)",
       subtitle = "Good generator as pairs are independent and uniformly distributed.",
       x = "X_n",
       y = "X_{n+1}")+
  theme_light()
g1
# 3. With `lcg_unif`, we would like to first similuate an exponential distribution.
#    Define a function `lcg_exp` that
#       - Takes inputs
#         - `n`, the number of samples to generate.
#         - `rate`, the rate parameter of the exponential distribution,
#           defaulted to `1.5`.
#       - Creates
#         - A vector `U` of length `n` for samples from Uniform(0, 1) using
#           `lcg_unif()`.
#         - A vector `E` of length `n` for samples from Exponential(rate) using
#           `- 1 / rate * log(U)`.
#       - Returns a tibble of size n x 1 with the column `E`.
#    Use `lcg_exp()` to generate 5000 samples for Exponential(1.5) and store the
#    result into a vector `exp_rng`.
#    To check your result, `exp_rng` prints to:
#    # A tibble: 5,000 x 1
#           E
#       <dbl>
#    1 0.198
#    2 0.0864
#    3 0.174
#    4 2.09
#    5 0.116
#    # … with 4,995 more rows
## Do not modify this line!
lcg_exp <- function(n,rate=1.5){
  U <- c(lcg_unif(n))
  E <- -1/rate*log(U)
  return(tibble(E=E))
}
exp_rng <- lcg_exp(5000)

# 4. Draw a histogram with density plot to compare the simulated distribution
#    and the actual one.
#    To do this, you can use:
#       - `ggplot()` to initialize a ggplot object and specify variables to plot.
#       - `geom_histogram()` to draw the density histogram, setting
#          `fill = "white", color = "black"`.
#          Hint: set `y` as density instead of count in the aesthetics.
#       - `geom_density()` to draw the density plot, setting `aes(color = "Simulated")`.
#       - `stat_function()` to add a density curve for Exponential(1.5) using
#         `dexp`, setting `aes(color = "Theoretical")`.
#       - `labs()` to format the labels such that:
#         - `title = "PDF of X ~ Exponential(1.5)"`
#         - `subtitle = "Simulated values are close to actual ones with n = 5000."`
#         - `x = "X"`
#         - `y = "Density"`
#    Store the plot into a ggplot object `g2`.
## Do not modify this line!
g2 <- ggplot(exp_rng)+
  geom_histogram(aes(E,y=..density..),fill='white', color='black')+
  geom_density(aes(E,color = "Simulated"))+
  stat_function(aes(E,color = "Theoretical"),fun = dexp, args = list(rate=1.5))+
    labs(title = "PDF of X ~ Exponential(1.5)",
         subtitle = "Simulated values are close to actual ones with n = 5000.",
         x = "X",
         y = "Density")+
    theme_light()
g2


# 5. With `lcg_unif`, we can also simulate a normal distribution. We will use
#    the Box-Muller transformation to generate a pair of independent normal
#    variables (Z1, Z2) by transforming a pair of independent Uniform(0, 1)
#    random variables (U1, U2). The formulas follow:
#       - `Z1 = mean + sd * sqrt(-2 * log(U1)) * cos(2 * pi * U2)`.
#       - `Z2 = mean + sd * sqrt(-2 * log(U1)) * sin(2 * pi * U2)`.
#    With the formulas, define a function `lcg_norm` that
#       - Takes inputs
#         - `n`, the number of samples to generate.
#         - `mean`, the mean parameter of the normal distribution, defaulted to 0.
#         - `sd`, the standard deviation parameter of the normal distribution,
#           defaulted to 1.
#       - Creates
#         - A vector `U1` of length `n` for samples from Uniform(0, 1) using
#           `lcg_unif()` with `x = 2^32 - 1`.
#         - A vector `U2` of length `n` for samples from Uniform(0, 1) using
#           `lcg_unif()` with `x = 2^32 - 2`.
#         - A vector `Z1` of length `n` for samples from Normal(mean, sd) using
#           the formulas above.
#         - A vector `Z2` of length `n` for samples from Normal(mean, sd) using
#           the formulas above.
#       - Returns a tibble of size n x 2 with columns `Z1` and `Z2`.
#    Use `lcg_norm()` to generate 5000 samples for a pair of independent
#    Normal(0, 1) and store the result into a vector `norm_rng`.
#    To check your result, `norm_rng` prints to:
#    # A tibble: 5,000 x 2
#          Z1      Z2
#       <dbl>   <dbl>
#    1 -0.768  0.0670
#    2 -0.237 -0.451
#    3 -0.612  0.384
#    4  0.848 -2.36
#    5 -0.241  0.539
#    # … with 4,995 more rows
## Do not modify this line!
lcg_norm <- function(n,mean=0,sd=1){
  U1 <- lcg_unif(n,x=2^32-1)
  U2 <- lcg_unif(n,x=2^32-2)
  z1 <- mean + sd * sqrt(-2 * log(U1)) * cos(2 * pi * U2)
  z2 <- mean + sd * sqrt(-2 * log(U1)) * sin(2 * pi * U2)
  return(tibble(Z1=z1,Z2=z2))
}

norm_rng <- lcg_norm(5000)
# 6. Draw a histogram with density plot to compare the simulated distribution
#    and the actual one.
#    To do this, you can use:
#       - `gather()` to collect `Z1` and `Z2` as a `key` column with corresponding
#         `value`.
#       - `ggplot()` to initialize a ggplot object and specify variables to plot.
#       - `geom_histogram()` to draw the density histogram, setting
#          `fill = "white", color = "black"`.
#          Hint: set `y` as density instead of count in the aesthetics.
#       - `geom_density()` to draw the density plot, setting `aes(color = "Simulated")`.
#       - `stat_function()` to add a density curve for Normal(0, 1) using `dnorm`,
#         setting `aes(color = "Theoretical")`.
#       - `facet_wrap()` to facet the plot by `key`.
#       - `labs()` to format the labels such that:
#         - `title = "PDFs of Two Independent Standard Normal Random Variables Generated by Box-Muller."`
#         - `subtitle = "Simulated values are close to actual ones with n = 5000."`
#         - `x = "Z"`
#         - `y = "Density"`
#    Store the plot into a ggplot object `g3`.
## Do not modify this line!
g3 <- norm_rng%>%
  gather(key = 'key',value = 'value')%>%
  ggplot()+
  geom_histogram(aes(x=value,y=..density..),fill="white",color = "black")+
  geom_density(aes(value,color = "Simulated"))+
  stat_function(aes(value,color = "Theoretical"), fun=dnorm,args = list(mean=0,sd=1))+
  facet_wrap(~key)+
  labs(title = "PDFs of Two Independent Standard Normal Random Variables Generated by Box-Muller.",
       subtitle = "Simulated values are close to actual ones with n = 5000.",
       x = "Z",
       y = "Density")+
  theme_light()
g3


