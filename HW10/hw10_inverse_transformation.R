# HW10: Inverse Transform Method
#
# In this exercise, we will consider the Cauchy random variable
# X[https://en.wikipedia.org/wiki/Cauchy_distribution] and use inverse
# transformation method to sample the random variables.
#
# 1. Let U be a uniform random variable over [0,1]. We want to find a transformation
#    of U that allows us to simulate a Standard Cauchy random variable X from U.
#    By following the steps of inverse transformation method, we want to first
#    obtain the cumulative distribution function `F`. Create a function `F1` that
#    takes an input `x`, `location` and `scale` and output the calculated
#    cumulative density for a cauchy distribution with speficified location and
#    scale. Set the default location into 0 and default scale into 1.
#    For example, F1(0) = 0.5,  F1(1,location=-1,scale=0.5) = 0.922
#    You should use `atan()` to calculate the arc tangent of an input and you should not
#    directly use `pcauchy()` to calculate the cdf.
## Do not modify this line!
F1 <- function(x, location=0, scale=1){
  1/pi*atan((x-location)/scale)+.5
}


# 2. Calculate the inverse of `F1` and create a function `Finv()` that takes an
#    input `u`, `location` and `scale` and ouput `F^{-1}(u)` from a cauchy distribution
#    with specified location and scale. Again, set the default location
#    into 0 and default scale into 1.
#    You should use `tan()` to calculate the tangent of an input and you should not
#    directly use `pcauchy()` to calculate the cdf.
#    For example, Finv(0.5) = 0,  Finv(0.92, location=-1, scale=0.5) = 0.947
## Do not modify this line!
Finv <- function(u, location=0, scale=1) tan(pi*(u-0.5))*scale+location


# 3. Load the `tidyr` package.
#    Use `set.seed()` to set the random seed to 0 and save your seed into `seed` using
#    `.Random.seed`
#    Generate 1000 samples from standard cauchy distribution and save them into a
#    tibble `df`.
#    To do this, you should:
#    - Use `runif()` to generate random samples from uniform distribution.
#    - Use `Finv()` created before.
#    You `df` should look like:
#    A tibble: 1,000 x 2
#    u      x
#    <dbl>  <dbl>
#    0.897  2.97
#    0.266 -0.907
#    0.372 -0.425
#    0.573  0.233
#    0.908  3.37
#    0.202 -1.36
#    0.898  3.03
#    0.945  5.70
#    0.661  0.553
#    0.629  0.429
#    … with 990 more rows.
## Do not modify this line!
library(tidyr)
set.seed(0)
seed <- .Random.seed
df <- tibble(u = runif(1000))%>%
  mutate(x = Finv(u))


# 4. Check your simulated distribution by using a histogram.
#    Recall that cauchy distribution is a heavy tail distribution with more probability
#    on the extreme values, which will make our plot hard to interpret. So before
#    we plot, we want to first filter out the extreme values.
#    To do this, you can:
#    - Load the `ggplot2` and `dplyr` package.
#    - Use `filter()` to keep the samples in (-50,50).
#    - Use `ggplot()` to initialize a ggplot object.
#      Set its arguments `data` and `mapping`.
#    - Use `geom_histogram()` to add a hitogram with density scale.
#        - specify `bins=100`.
#        - specify `y = ..density..`.
#    - Use `stat_function()` to plot the density true standard cauchy distribution
#        - specify `color = "red"`.
#        - use `dcauchy()` to calculate the density.
#    - Use `labs()` to format the labels such that:
#        - `title = "The generated samples have same density as the true distribution"`.
#        - `x = "Simulated x"`.
#        - `y = "Density"`.
#    - Use `theme_light()` to change the theme of plots.
#    - Use `theme()` to change the title and subtitle to the middle of the plot.
#        - Set its argument `plot.title` using `element_text(hjust = 0.5)`.
#    Save your plot into `g1`.
## Do not modify this line!
library(ggplot2)
library(dplyr)
g1 <- df%>%
  filter(between(u,-50,50)&between(x,-50,50))%>%
  ggplot()+
  geom_histogram(aes(x, y=..density..), bins=100)+
  stat_function(aes(x),fun = dcauchy,color = 'red')+
  labs(title = "The generated samples have same density as the true distribution",
       x = "Simulated x",
       y = "Density")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))
g1

# 5. Check your simulated distribution by using a qqplot.
#    To do this, you can:
#    - Use `ggplot()` to initialize a ggplot object.
#      Set its arguments `data` and `mapping`.
#    - Use `geom_qq()` to add a qqplot
#        - specify `distribution = qcauchy`.
#    - Use `stat_qq_line()` to plot the reference line
#        - specify `color = "red"`.
#        - specify `distribution = qcauchy`.
#    - Use `labs()` to format the labels such that:
#        - `title = "The sampling distribution follows the general pattern of a standard cauchy"`.
#        - `subtitle = "Cauchy distribution is more likely to generate extreme variables"`.
#        - `x = "Theoretical"`.
#        - `y = "Sample"`.
#    - Use `theme_light()` to change the theme of plots.
#    - Use `theme()` to change the title and subtitle to the middle of the plot.
#        - Set its argument `plot.title` using `element_text(hjust = 0.5)`.
#        - Set its argument `plot.subtitle` using `element_text(hjust = 0.5)`.
#    Save your plot into `g2`.
## Do not modify this line!
g2 <- ggplot(df)+
  geom_qq(aes(sample = x), distribution = qcauchy)+
  stat_qq_line(aes(sample=x),color="red", distribution = qcauchy)+
  labs(title = "The sampling distribution follows the general pattern of a standard cauchy",
       subtitle = "Cauchy distribution is more likely to generate extreme variables",
       x = "Theoretical",
       y = "Sample")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  theme_light()
g2


