# HW10: MC Integration of a power law
#
# This exercise aims at practicing Monte-Carlo Integration and observing the
# benefits of a couple techniques.
#
# 1. In this question:
#    - write a function factory `factory` that takes an input `k` and returns
#      the function `x -> x^k` - don't forget to use `force()` in it,
#    - assign the cubic function to `f_3` by calling `factory(3)`,
#    - assign the value of the integral of `f_3` between 0 and 1 to
#      `integral_f_3` (don't compute it programmatically, you can 
#      calculate it directly!)
#    For example, `factory(2)(3)` should be equal to `9` and `f_3(3)` should be
#    equal to `27`
## Do not modify this line!
factory <- function(k){
  force(k)
  function(x) x^k
}
integral_f_3 <- .25
f_3 <- factory(3)

# 2. Load the `tibble` and `dplyr` packages.
#    Let's sample from a uniform distribution and use the samples to build a
#    MC estimator of the integral. In this question:
#    - assign a sample size of 10,000 to `n`,
#    - call `set.seed(11)`,
#    - assign `.Random.seed` to variable `seed`,
#    - create a tibble `df` using `tibble()`, containing:
#      - a column `s`: a sample of `n` data points from a uniform distribution
#        between 0 and 1 generated using `runif()`,
#      - a column `f_3` obtained by applying `f_3()` to `s`
#    - compute the estimator and confidence interval using `summarize()`
#      from `df` and assign the resulting tibble to `ci_unif`; you will need
#      to:
#      - compute the MC estimation of the integral of `f_3` between 0 and 1
#        using `mean()` and `f_3` and name it `integral`,
#      - compute the MC standard error using `sqrt()`, `var()`, `f_3` and `n`
#        and assign it to `se`,
#      - use `integral` and `se` to compute a 95% confidence interval centered
#        on `integral` using the tabulated value of `1.96` and assign bounds of
#        the interval to `lower` and `upper`
#      The shape of `ci_unif` will be `1x4` with column names
#      `(integral, se, lower, upper)`.
#    `df` should print to:
#    # A tibble: 10,000 x 2
#              s      f_3
#          <dbl>    <dbl>
#     1 0.277    2.13e- 2
#     2 0.000518 1.39e-10
#     3 0.511    1.33e- 1
#     4 0.0140   2.77e- 6
#     5 0.0647   2.71e- 4
#     6 0.955    8.71e- 1
#     7 0.0865   6.47e- 4
#     8 0.290    2.44e- 2
#     9 0.881    6.83e- 1
#    10 0.123    1.87e- 3
#    # … with 9,990 more rows
## Do not modify this line!
library(tibble)
library(dplyr)
n <- 10000
set.seed(11)
seed <- .Random.seed
df <- tibble(s = runif(n))%>%
  mutate(f_3 = f_3(s))
df

ci_unif <- tibble(integral = mean(df$f_3),
                  se = sqrt(var(df$f_3)/n),
                  lower = integral-1.96*se,
                  upper = integral+1.96*se)
  


# 3. Now, let's use antithetic examples to reduce the variance of the
#    estimator; using the fact that if `X ~ unif(0, 1)`, `1-X ~ unif(0, 1)`.
#    In this question:
#    - add the column `f_3_anti` applying `f_3()` to antithetic samples to `df`
#      using `mutate()` and assign the resulting tibble to `df2`,
#    - compute the estimator and and confidence interval using `summarize()`
#      from `df2` and assign the resulting tibble to `ci_anti`; you will need
#      to:
#      - compute the MC estimation of the integral of `f_3` between 0 and 1
#        using `mean()`, `f_3` and `f_3_anti` - name it `integral`,
#      - compute the MC standard error using `sqrt()`, `var()`, `f_3`,
#        `f_3_anti` and `n` and assign it to `se`,
#      - use `integral` and `se` to compute a 95% confidence interval centered
#        on `integral` using the tabulated value of `1.96` and assign bounds of
#        the interval to `lower` and `upper`
#      The shape of `ci_anti` will be `1x4` with column names
#      `(integral, se, lower, upper)`.
#    `df2` should print to:
#    # A tibble: 10,000 x 3
#              s      f_3  f_3_anti
#          <dbl>    <dbl>     <dbl>
#     1 0.277    2.13e- 2 0.378
#     2 0.000518 1.39e-10 0.998
#     3 0.511    1.33e- 1 0.117
#     4 0.0140   2.77e- 6 0.958
#     5 0.0647   2.71e- 4 0.818
#     6 0.955    8.71e- 1 0.0000920
#     7 0.0865   6.47e- 4 0.762
#     8 0.290    2.44e- 2 0.358
#     9 0.881    6.83e- 1 0.00170
#    10 0.123    1.87e- 3 0.674
#    # … with 9,990 more rows
## Do not modify this line!
df2 <- df%>%
  mutate(f_3_anti = f_3(1-s))
ci_anti <- tibble(integral = mean(c(df2$f_3_anti, df2$f_3)),
                  se = sqrt(var((df2$f_3+df2$f_3_anti)/2)/n),
                  lower = integral-1.96*se,
                  upper = integral+1.96*se)

# 4. Now, let's use control variates to reduce the variance of the estimator in
#    question 2; using the known value of the integral of the square function
#    between 0 and 1. In this question:
#    - use `factory()` to assign the square function to `f_2`,
#    - assign the value of the integral of `f_2` between 0 and 1 to
#      `integral_f_2` (don't compute it programmatically!),
#    - add the column `f_2` applying `f_2()` to `s` in `df` using `mutate()`
#      and assign the resulting tibble to `df3`,
#    - compute the estimator and and confidence interval using `summarize()`
#      from `df3` and assign the resulting tibble to `ci_control`; you will
#      need to:
#      - compute the negative ratio of the covariance of `f_3` and `f_2`, and
#        `var(f_2)` - and name it `c`,
#      - compute the MC estimation of the integral of `f_3` between 0 and 1
#        using `mean()`, `f_3` and `c`, `f_2` and `integral_f_2` - and name it
#        `integral`,
#      - compute the MC standard error using `sqrt()`, `var()`, `f_3`,
#        `f_2`, `integral_f_2` and `n` and assign it to `se`,
#      - use `integral` and `se` to compute a 95% confidence interval centered
#        on `integral` using the tabulated value of `1.96` and assign bounds of
#        the interval to `lower` and `upper`
#      The shape of `ci_control` will be `1x5` with column names
#      `(c, integral, se, lower, upper)`.
#    `df3` should print to:
#    # A tibble: 10,000 x 4
#              s      f_3  f_3_anti         f_2
#          <dbl>    <dbl>     <dbl>       <dbl>
#     1 0.277    2.13e- 2 0.378     0.0769
#     2 0.000518 1.39e-10 0.998     0.000000269
#     3 0.511    1.33e- 1 0.117     0.261
#     4 0.0140   2.77e- 6 0.958     0.000197
#     5 0.0647   2.71e- 4 0.818     0.00418
#     6 0.955    8.71e- 1 0.0000920 0.912
#     7 0.0865   6.47e- 4 0.762     0.00748
#     8 0.290    2.44e- 2 0.358     0.0841
#     9 0.881    6.83e- 1 0.00170   0.776
#    10 0.123    1.87e- 3 0.674     0.0152
#    # … with 9,990 more rows
## Do not modify this line!
f_2 <- factory(2)
integral_f_2 <- 1/3
df3 <- df2%>%
  mutate(f_2 = f_2(s))
df3

ci_control <- df3%>%
  summarize(c=-cov(f_3,f_2)/var(f_2),
            integral = mean(f_3+c*(f_2-integral_f_2)),
            se = sqrt(var(f_3+c*(f_2-integral_f_2))/n),
            lower = integral-1.96*se,
            upper = integral+1.96*se)
ci_control
# 5. Load the `tidyr` package.
#    Compute a tibble containing the estimation errors (the difference between
#    the MC estimation and the actual value of the integral) of the three
#    different estimators for each step of the MC sequence generation.
#    For this we first need to compute cumulative statistics of the MC
#    sequences at each step of the generation process.
#    In this question:
#    - compute the `cum_stats` tibble from `df3` using `mutate()` to add the
#      following columns - you can compute all columns only using the existing
#      columns of `df3` and the function `cummean()`:
#      - `cummean_f_3`: the cumulative mean of `f_3`,
#      - `cummean_f_3_anti`: the cumulative mean of `f_3_anti`,
#      - `cummean_f_2`: the cumulative mean of `f_2`,
#      - `cumvar_f_2`: the cumulative variance of `f_2` computed using
#        `cummean(f_2^2)` and `cummean_f_2`,
#      - `cummcov_f_2_f_3`: the cumulative covariance of `f_2` and `f_3`
#        computed using the cumulative mean of `f_2 * f_3` minus the product
#        ot the cumulative means of each,
#      - `cum_c`: the cumulative value of `c` (using only already generated
#        points to compute `c` at each step) - you can use `cumvar_f_2` and
#        `cumcov_f_2_x_f_3` to compute `cum_c`,
#      - filter out rows in which `cum_c` is `NaN` - this happens if the
#        sample variance of `f_2` is `0` (ie. for step 1 when only one point is
#        generated)
#   - compute the `errors` tibble containing the estimation errors from
#     `cum_stats`:
#     - use `mutate()` to construct the following columns:
#       - `sequence` contains all integers from `1` to `n - 1`,
#       - `unif` contains the difference of the MC estimation of the vanilla
#         uniform samples and the actual integral at each step (you can compute
#         it using `cummean_f_3` and `integral_f_3`),
#       - `antithetic` contains the difference of the MC estimation of the
#         antithetic samples and the actual integral at each step (you can
#         compute it using `cummean_f_3, cummean_f_3_anti` and `integral_f_3`),
#       - `control` contains the difference of the MC estimation using control
#         variates and the actual integral at each step (you can compute it
#         using `cummean_f_3`, `cum_c`, `cummean_f_2`, `integral_f_2` and
#         `integral_f_3`)
#     - keep only the columns you just added using `select()`,
#     - use `pivot_longer()` to tidy the data by placing the different types of
#       estimation in one column and the error values in another as in the
#       following
#   `cum_stats %>% select(-c(s, f_3, f_3_anti, f_2))` should print to:
#   # A tibble: 9,999 x 6
#     cummean_f_3 cummean_f_3_anti cummean_f_2 cumvar_f_2 cumcov_f_2_x_f_3  cum_c
#           <dbl>            <dbl>       <dbl>      <dbl>            <dbl>  <dbl>
#   1      0.0107            0.688      0.0384    0.00148         0.000410 -0.277
#   2      0.0515            0.498      0.113     0.0120          0.00632  -0.528
#   3      0.0386            0.613      0.0844    0.0113          0.00583  -0.514
#   4      0.0309            0.654      0.0684    0.0101          0.00515  -0.510
#   5      0.171             0.545      0.209     0.107           0.103    -0.957
#   6      0.147             0.576      0.180     0.0969          0.0922   -0.952
#   7      0.131             0.549      0.168     0.0858          0.0819   -0.955
#   8      0.193             0.488      0.236     0.113           0.106    -0.940
#   9      0.174             0.507      0.214     0.106           0.0991   -0.937
#  10      0.158             0.512      0.197     0.0989          0.0927   -0.937
#  # … with 9,989 more rows
#   `errors` should print to:
#    # A tibble: 29,997 x 3
#       sequence sample_type   value
#          <int> <chr>         <dbl>
#     1        1 unif        -0.239
#     2        1 antithetic   0.0993
#     3        1 control     -0.158
#     4        2 unif        -0.199
#     5        2 antithetic   0.0246
#     6        2 control     -0.0818
#     7        3 unif        -0.211
#     8        3 antithetic   0.0758
#     9        3 control     -0.0835
#    10        4 unif        -0.219
#    # … with 29,987 more rows
## Do not modify this line!
library(tidyr)
cum_stats <- df3%>%
  mutate(cummean_f_3 = cummean(f_3), cummean_f_3_anti = cummean(f_3_anti),
         cummean_f_2 = cummean(f_2), cumvar_f_2 = (cummean(f_2^2)-(cummean(f_2))^2),
         cumcov_f_2_x_f_3 = (cummean(f_2*f_3)-cummean(f_2)*cummean(f_3)),
         cum_c = -cumcov_f_2_x_f_3/cumvar_f_2)%>%
  filter(!is.na(cum_c))
  
errors <- cum_stats%>%
  mutate(sequence=1:(n-1), unif = cummean_f_3-integral_f_3,
         antithetic = (cummean_f_3_anti+cummean_f_3)/2-integral_f_3,
         control=(cummean_f_3+cum_c*(cummean_f_2-integral_f_2))-integral_f_3)%>%
  dplyr::select(c(sequence, unif, antithetic, control))%>%
  pivot_longer(cols = -sequence, names_to = "sample_type", values_to = "value")
errors 
# 6. Load the `ggplot2` package.
#    Generate an error plot `error_plot` from `errors` showing the error for
#    each type of estimation at each step.
#    To do so, you can:
#    - call `ggplot()` from `errors` with the aesthetic `x = sequence`,
#      `y = value` and `colour = sample_type`,
#    - add `geom_line()`,
#    - restrict the shown sequence values up to the 1500th step included using
#      `coord_cartesian(xlim = c(0, 1500))`,
#    - feed the tibble to `ggplot()`,
#    - use `labs()` for the following labels:
#      - `y = "MC error"`,
#      - `colour = "Sample type"`,
#      - `title = "Using antithetic samples and control variates accelerates convergence and reduces variance"`
#    - add `theme_light()`
#
## Do not modify this line!
library(ggplot2)

error_plot <- ggplot(errors)+
  geom_line(aes(sequence, value, color = sample_type))+
  coord_cartesian(xlim=c(0,1500))+
  labs(y = "MC error",
       colour = "Sample type",
       title = "Using antithetic samples and control variates accelerates convergence and reduces variance")+
  theme_light()


