# HW10: Inverse Transform Sampling
#
# In this exercise, we will learn to apply the inverse transform method.
# We will focus on the weibull distribution.
# For details, here is the link: [https://en.wikipedia.org/wiki/Weibull_distribution]
#
# Throughout the exercise:
#    - Do NOT use `for`, `while` or `repeat` loops.
#    - Use `%>%` to structure your operations.
#    - Use `theme_light()` for the plots.
#    - For graphs with titles, use `theme()` to set
#      `plot.title = element_text(hjust = 0.5)` and
#      `plot.subtitle = element_text(hjust = 0.5)`.
#
# 1. Let `U` be a uniform random variable over [0, 1]. Let `X` be a weibull
#    random variable with cdf `F` such that `F(X) ~ U[0, 1]`. Based on the
#    link given above, derive the formula that `F^{-1}(u) = x`. Then, create
#    a function `Finv()` that takes an input `u`, `shape`(which is k) and
#    `scale`(which is lamda) and ouput `F^{-1}(u)` from a weibull distribution.
#    Specify the `shape` as 3 and `scale` as 2.
#    For example, Finv(0) = 0,  Finv(0.5, shape = 1, scale = 1) = 0.6931472.
## Do not modify this line!
Finv <- function(u,shape=3,scale=2) scale*(-log(1-u))^(1/shape)
Finv(0.5, shape = 1, scale = 1)

# 2. Load the `tidyverse` package.
#    Use `set.seed()` to set the random seed to 100. Store the seed to `seed1`.
#    Generate 10000 samples from uniform distribution and save them into a
#    tibble `df`.
#    To do this, you use:
#    - `runif()` to generate random samples from uniform distribution and name
#    the column as `u`.
#    - `Finv()` you created to calculate values and name the column as `x`.
#    To check your answer, the `df` prints to:
#    # A tibble: 10,000 x 2
#    u     x
#    <dbl> <dbl>
#    1 0.308  1.43
#    2 0.258  1.34
#    3 0.552  1.86
#    4 0.0564 0.774
#    5 0.469  1.72
#    6 0.484  1.74
#    7 0.812  2.37
#    8 0.370  1.55
#    9 0.547  1.85
#    10 0.170  1.14
#    # … with 9,990 more rows
## Do not modify this line!
set.seed(100)
seed1 <- .Random.seed
df <- tibble(u = runif(10000))%>%
  mutate(x = Finv(u))
df

# 3. Use `set.seed()` to set the random seed to 100. Store the seed to `seed2`.
#    Use `ks.test()` to test if the values of `x` we calculated fit well in
#    weibull distribution. The argument should take in values of `x` from df,
#    `pweibull` and two parameters we specified in the first problem. Extract
#    p-value of the test and store it to `pvalue_result`.
## Do not modify this line!
set.seed(100)
seed2 <- .Random.seed
pvalue_result <- ks.test(df$x,pweibull,shape = 3, scale = 2)$p.value


# 4. Load the `ggplot2` package.
#    Draw a histogram of `x` to compare with the distribution of weibull
#    dsitribution.
#    To do this, you can use:
#    - `ggplot()` to plot `x` from `df`.
#    - `geom_histogram()` to add a hitogram with density scale.
#        - specify `bins = 40`.
#        - specify `y = ..density..`.
#    - `stat_function()` to plot the density of weibull distribution
#        - use `dweibull()` to calculate the density.
#        - specify `color` in `aes()` as `"True value"`.
#    - `scale_colour_manual()` to name the title as `"Legend"` and color the
#      `"True value"` as `"red"`.
#    - `labs()` to format the labels such that:
#        - `title = "The density plot of random generated samples is similar to the true distribution"`.
#        - `x = "Random generated x"`.
#        - `y = "Density"`.
#    Store the returned plot to `g1`.
## Do not modify this line!
library(ggplot2)
g1 <- ggplot(df)+
  geom_histogram(aes(x,y=..density..), bins = 40)+
  stat_function(aes(x, color = "True value"),fun = dweibull, args = list(shape = 3, scale = 2))+
  scale_colour_manual(values = "red",name = "Legend")+
  labs(title = "The density plot of random generated samples is similar to the true distribution",
       x = "Random generated x",
       y = "Density")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
g1

# 5. Draw a qq-plot to plot the estimated distribution vs. true value.
#    To do this, you can use:
#      - `ggplot()` to plot the `x` from `df`.
#      - `geom_qq()` to draw a weibull distribution with parameters
#        of `dparams`.
#      - `stat_qq_line()` to draw a straight line that takes weibull
#        distribution with parameters of `dparams`, make the `color` as red.
#      - `labs()` to name the title as `"The sample seems to distribute along the straight line"`,
#        subtitle as `"The samples fit well in the weibull distribution with shape = 3 and scale = 2"`,
#        x-axis as `"Theoretical values"`,
#        y-axis as `"Actual values"`.
#    Store returned graph to `g2`.
## Do not modify this line!

g2 <- ggplot(df)+
  geom_qq(aes(sample = x),distribution = qweibull ,dparams = list(shape = 3, scale = 2))+
  stat_qq_line(aes(sample = x),distribution = qweibull ,dparams = list(shape = 3, scale = 2),color="red")+
  labs(title = "The sample seems to distribute along the straight line",
       subtitle = "The samples fit well in the weibull distribution with shape = 3 and scale = 2",
       x = "Theoretical values",
       y = "Actual values")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
  
g2

