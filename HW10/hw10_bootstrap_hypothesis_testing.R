# HW10: Bootstrap for hypothesis testing
# 
# In 1882 Simon Newcomb performed an experiment to measure the speed of 
# light. The numbers below represent the measured time it took for light 
# to travel from Fort Myer on the west bank of the Potomac River to a fixed 
# mirror at the foot of the Washington monument 3721 meters away.
# In the units in which the data are given, the currently accepted “true” 
# speed of light is 33.02. (To convert these units to time in the millionths 
# of a second, multiply by 10-3 and add 24.8.)
# The recorded values were : 
# 28, -44, 29, 30, 26, 27, 22, 23, 33, 16, 
# 24, 29, 24, 40, 21, 31, 34, -2, 25, 19
# 
# Does the data support the current accepted speed of 33.02?
#
# 1. (1)
#    Create a vector `speed` and save the values 
#    `28, -44, 29, 30, 26, 27, 22, 23, 33, 16, 24, 40, 21, 31, 34, -2, 25, 19`
#    in it (in this order).
#    (2)
#    Draw a histogram using `ggplot()` and save it in `speed_hist`.
#    Note that there are some outliers.
#    You can use:
#    - `ggplot()` to initialize a ggplot object.
#      Set `data` to `tibble(speed = speed)`.
#      Set `mapping` to `aes(speed)`.
#    - `geom_histogram()` to plot the histogram.
#      - Set `bins` to `30`.
#      - Set `color` to `white`.
#    - `labs()`
#      - Set `title` to `"Histogram of speed"`.
#      - Set `subtitle` to `"There are outliers."`.
#      - Set `x` to `Speed`.
#      - Set `y` to `Count`.
#    - `theme_light()` to set the theme to light.
## Do not modify this line!
library(ggplot2)
library(tibble)
speed <- c(28, -44, 29, 30, 26, 27, 22, 23, 33, 16, 24, 29, 24, 40, 21, 31, 34, -2, 25, 19)
speed_hist <- ggplot(tibble(speed=speed))+
  geom_histogram(aes(speed),bins = 30,color='white')+
  labs(title = "Histogram of speed",
       subtitle = "There are outliers.",
       x = "Speed",
       y = "Count")+
  theme_light()


# 2. Draw a qq plot using `ggplot()` and save it in `speed_qq`.
#    Note that the distribution is not normal.
#    You can use:
#    - `ggplot()` to initialize a ggplot object.
#      Set `data` to `tibble(speed = speed)`.
#      Set `mapping` to `aes(speed)`.
#    - `stat_qq()` to plot the dots.
#    - `stat_qq_line` to plot the line.
#    - `labs()`
#      - Set `title` to `"QQ plot of speed"`.
#      - Set `subtitle` to `"The data is not normally distributed."`.
#    - `theme_light()` to set the theme to light.
## Do not modify this line!
speed_qq <- ggplot(tibble(speed = speed),aes(sample = speed))+
  stat_qq()+
  stat_qq_line()+
  labs(title = "QQ plot of speed",
       subtitle = "The data is not normally distributed.")+
  theme_light()


# 3. When performing a t-test, we assume that the population of 
#    measurements is normally distributed. 
#    Since this is not the case here, our tests will at best be approximations. 
#    But with this small sample size, and with such a severe departure from
#    normality, we can’t guarantee a good approximation.
#    The bootstrap offers one approach.
#    The null and alternative hypotheses are :
#    `H0: mean = 33.02`
#    `Ha: mean is not equal to 33.02`
#    The significant level is 5%.
#    (Note: The significance level is the probability of rejecting the null
#    hypothesis when it is true)
#    We now need the p-value, but to do this we need to know the sampling 
#    distribution of our test statistic when the null hypothesis is true. 
#    Our approach is to perform a simulation under 
#    conditions in which we know the null hypothesis is true.
#    (1)
#    What we’ll do is use our data to represent the population, but first 
#    we shift it over so that the mean really is 33.02.
#    Write a function factory `boot_null(x, mu0)` to do this.
#    The input `x` is a vector.
#    The input `mu0` is the mean under null hypothesis.
#    This function shifts the mean of input `x` to `mu0` and returns a function that
#    samples from the shifted `x` with replacement.
#    For example, for seed `5206`, `boot_null(speed, 0)()` would return
#     [1] -23.75   2.25   7.25   7.25 -65.75   2.25   7.25   7.25   4.25   5.25  
#     [11] -5.75   4.25 -65.75  -5.75 -65.75   8.25   7.25  18.25   0.25  18.25
#    (2)
#    Set `mu0` to `33.02`.
#    Use `boot_null()` to create a function `boot_speed_null()` that has no input, 
#    shift the mean of `speed` to `mu0` and sample from it with replacement.
#    (3)
#    Bootstrap using `boot_speed_null()` 1000 times.
#    Set the random seed to zero and save the random seed vector to `seed`. 
#    (hint: use the command `seed <- .Random.seed`).
#    Save the bootstrap samples in tibble `bstrap` with 3 columns `b`, `sample_null`, 
#    and `muhat_null`.
#    - `b` indicates the row number, from 1 to 1000.
#    - `sample_null` contains one bootstrap sample in each row.
#    - `muhat_null` is the mean of the bootstrap sample.
#    You can use `tibble()` to create a tibble.
#    - Set column `b` to 1 to 1000.
#    - Use column `b` and `map()` to compute column `sample_null`.
#    - Use column `sample_null` and `map_dbl()` to compute column `muhat_null`.
#    Tibble `bstrap` should print to:
#    # A tibble: 1,000 x 3
#          b sample_null muhat_null
#      <int> <list>           <dbl>
#    1     1 <dbl [20]>        34.5
#    2     2 <dbl [20]>        31.5
#    3     3 <dbl [20]>        32.7
#    4     4 <dbl [20]>        37.7
#    5     5 <dbl [20]>        32.6
#    6     6 <dbl [20]>        37.4
#    7     7 <dbl [20]>        24.0
#    8     8 <dbl [20]>        36.7
#    9     9 <dbl [20]>        35.7
#   10    10 <dbl [20]>        32.8
#   # … with 990 more rows
#    `bstrap[1, 2][[1]]` should print to:
#     [1]  51.27  41.27  33.27  39.27 -32.73  35.27  51.27   9.27  36.27  39.27  
#     [11] 27.27  51.27  27.27  33.27 44.27  32.27  37.27  44.27  51.27  37.27
## Do not modify this line!
boot_null <- function(x, mu0){
  x <- x-(mean(x)-mu0)
  function () sample(x, length(x), replace = T)
}
mu0 <- 33.02
boot_speed_null <- boot_null(speed, mu0)

set.seed(0)
seed <- .Random.seed
bstrap <- tibble(b=1:1000)%>%
  mutate(sample_null = map(b, ~boot_speed_null()),
         muhat_null = map_dbl(sample_null,mean))
bstrap
bstrap[1,2][[1]]


# 4. Draw a histogram for column `muhat_null` in `bstrap` and save it in 
#    `bstrap_hist`. You can use:
#    - `ggplot()` to initialize a ggplot object.
#      Set `data` to `bstrap`.
#      Set `mapping` to `aes(muhat_null)`.
#    - `geom_histogram()` to plot the histogram.
#      - Set `bins` to `30`.
#      - Set `color` to `white`.
#    - `labs()`
#      - Set `title` to `"Histogram of bootstrap samples"`.
#      - Set `subtitle` to `"The distribution is not normal."`.
#      - Set `x` to `Samples`.
#      - Set `y` to `Count`.
#    - `theme_light()` to set the theme to light.
## Do not modify this line!
bstrap_hist <- ggplot(bstrap)+
  geom_histogram(aes(muhat_null), bins=30, color = 'white')+
  labs(title = "Histogram of bootstrap samples",
       subtitle = "The distribution is not normal.",
       x = "Samples",
       y = "Count")+
  theme_light()


# 5. (1)
#    Write a function `boot_pval(muhat, mu0, muhat_null)` to compute p value.
#    The input `muhat` is the mean of the original samples.
#    The input `mu0` is the mean under null hypothesis.
#    The input `muhat_null` is a vector contains the mean of bootstrap samples.
#    The output is the two sided p value.
#    Note the p value is the proportion of samples that has mean as extreme as or 
#    more extreme as the mean of original sample.
#    For example, `boot_pval(30, 31, speed)` should be `0.9`.
#    (2)
#    Compute the p value for our case and save it into `p_value`.
#    That is `muhat` is the mean of `speed`, `mu0` is `33.02`, and `muhat_null` 
#    is the column `muhat_null` in `bstrap`.
## Do not modify this line!
boot_pval<-function(muhat, mu0, muhat_null){
  (sum(muhat_null < mu0-abs(mu0-muhat)) + sum(muhat_null > mu0+abs(mu0-muhat)))/length(muhat_null)
}
p_value<-boot_pval(mean(speed),33.02,bstrap$muhat_null)


# 6. (1)
#    Note that there were two extreme observations: -44 and -2. 
#    The t-test is notoriously susceptible to being influenced 
#    by extreme observations because it depends on the sample average. 
#    Let’s take those two values out and save the vectors of 
#    the values we keep into `newspeed`. (It should be equal to 
#    `28, 29, 30, 26, 27, 22, 23, 33, 16, 24, 29, 24, 40, 21, 31, 34, 25, 19`)
#    (2)
#    Draw a qq plot for `newspeed` and save it in `newspeed_qq`.
#    Note that the distribution is not normal.
#    You can use:
#    - `ggplot()` to initialize a ggplot object.
#      Set `data` to `tibble(speed = newspeed)`.
#      Set `mapping` to `aes(speed)`.
#    - `stat_qq()` to plot the dots.
#    - `stat_qq_line` to plot the line.
#    - `labs()`
#      - Set `title` to `"QQ plot of newspeed"`.
#      - Set `subtitle` to `"The data is now normally distributed."`.
#    - `theme_light()` to set the theme to light.
## Do not modify this line!
newspeed <- c(28, 29, 30, 26, 27, 22, 23, 33, 16, 24, 29, 24, 40, 21, 31, 34, 25, 19)
newspeed_qq <- ggplot(tibble(speed = newspeed),aes(sample=speed))+
  stat_qq()+
  stat_qq_line()+
  labs(title = "QQ plot of newspeed",
       subtitle = "The data is now normally distributed.")+
  theme_light()


# 7. Use `t.test()` to conduct a two-sided t test for `newspeed` to
#    test whether the mean of `newspeed` equals to `33.02`.
#    Save the result into `ttest`. It should be the output of `t.test()`.
#    We can see that the null hypothesis is rejected.
## Do not modify this line!

ttest<-t.test(newspeed,mu=33.02,alternative = "two.sided")

# 8. Now we will use bootstrap again.
#    (1)
#    Use `boot_null()` to create a function `boot_speed_null2()` that has no input, 
#    shift the mean of `newspeed` to `33.02` and sample from it with replacement.
#    (2)
#    Bootstrap using `boot_speed_null2()` 1000 times.
#    Set the random seed to zero and save the random seed vector to `seed`. 
#    (hint: use the command `seed <- .Random.seed`).
#    Save the bootstrap samples in tibble `bstrap2` with 3 columns `b`, `sample_null`, 
#    and `muhat_null`.
#    - `b` indicates the row number, from 1 to 1000.
#    - `sample_null` contains one bootstrap sample in each row.
#    - `muhat_null` is the mean of the bootstrap sample.
#    You can use `tibble()` to create a tibble.
#    - Set column `b` to 1 to 1000.
#    - Use column `b` and `map()` to compute column `sample_null`.
#    - Use column `sample_null` and `map_dbl()` to compute column `muhat_null`.
#    Tibble `bstrap2` should print to:
#    # A tibble: 1,000 x 3
#          b sample_null muhat_null
#      <int> <list>           <dbl>
#    1     1 <dbl [18]>        30.0
#    2     2 <dbl [18]>        32.3
#    3     3 <dbl [18]>        31.9
#    4     4 <dbl [18]>        34.7
#    5     5 <dbl [18]>        33.2
#    6     6 <dbl [18]>        30.5
#    7     7 <dbl [18]>        35.0
#    8     8 <dbl [18]>        33.0
#    9     9 <dbl [18]>        30.0
#   10    10 <dbl [18]>        32.2
#   # … with 990 more rows
#    `bstrap2[1, 2][[1]]` should print to
#     [1] 27.29778 32.29778 29.29778 34.29778 35.29778 35.29778 27.29778 25.29778 
#     [9] 34.29778 30.29778 27.29778 30.29778 29.29778 22.29778 37.29778 
#     [16] 33.29778 22.29778 27.29778
#    (3)
#    Compute the two sided p value and save it into `p_value2`.
#    We see that this time the p value is much smaller.
## Do not modify this line!
boot_speed_null2 <- boot_null(newspeed,33.02)
set.seed(0)
seed <- .Random.seed
bstrap2 <- tibble(b=1:1000)%>%
  mutate(sample_null = map(b,~boot_speed_null2()),
         muhat_null = map_dbl(sample_null,mean))

p_value2 <- boot_pval(mean(newspeed),33.02,bstrap2$muhat_null)


