# HW10: Bootstrap confidence interval
#
# In this exercise, we will use bootstrap to estimate the confidence interval for
# regression coefficients. We will fit linear regression on bootstrap samples of
# `mtcars` dataset and estimate the confidence intervals.
#
# 1. Let's first calculate the estimated intercept and slope on the full dataset
#    without resampling.
#    Load the `tidyverse` and `broom` library.
#    Use `set.seed()` to set the random seed to 0 and save your seed into `seed` using
#    `.Random.seed`.
#    Create a function `model_mtcar` which takes an input `df` and output a linear model
#    of `mpg` regress on `wt` on the `df` dataset.
#    For example, the output of `model_mtcars(mtcars[1:20,]) %>% tidy()` shoule be:
#    # A tibble: 2 x 5
#    term        estimate std.error statistic  p.value
#    <chr>          <dbl>     <dbl>     <dbl>    <dbl>
#    (Intercept)    38.5      2.36      16.3  3.07e-12
#    wt             -5.41     0.664     -8.15 1.88e- 7
#    Implement your function on `mtcars` (the original whole dataset) and save the
#    output linear model into `fit_mtcars`.
#    Then get the confidence interval of this model by using `confint()`, save your
#    confidence interval into `ci_normal`.
## Do not modify this line!
library(tidyverse)
library(broom)
set.seed(0)
seed <- .Random.seed
model_mtcars <- function(df) lm(mpg~wt, data = df)
fit_mtcars <-model_mtcars(mtcars)
ci_normal <- confint(fit_mtcars)

# 2. Now let's get the bootstrap index for permutations.
#    Create a function factory `boot_permute_factory` that takes an input `df`, calculate
#    the number of rows of `df` and returns a function to resample from `df` with
#    replacement. For example, `boot_permute_factory(mtcars)` returns a function that
#    takes no input, but returns the resampled data from `df`.
#    Note that for bootstrap resample, your resampled tibble should have the same
#    row numbers with your original tibble.
#    For instance, if you runs:
#    `myboot <- boot_permute_factory(tibble(x = 1:20)); set.seed(0); myboot()`,
#    then the first few rows of output will be
#    # A tibble: 20 x 1
#    x
#    <int>
#      18
#      6
#      8
#      12
#      19
#      5
#    Now implement your `boot_permute_factory()` on `mtcars` and save the output into
#    `boot_permute_mtcars`.
#    Choose number of bootstrap as 200 and save it to `B`.
#    Use `set.seed()` to set the random seed to 0 and save your seed into `seed`
#    using `.Random.seed`.
#    Generate a tibble `result_boot`, storing all information about the linear model for
#    the bootstrap resampled tibble, to do this, you can use:
#    - `tibble()` to create a tibble including
#       column `b` indicates the row number, from 1 to `B`
#       column `permute` indicates the information of linear regressionin each bootstrap
#    - `map()` to broadcast your operations.
#    - `boot_permute_mtcars()` to get permutations of dataset for each `b`
#    - `model_mtcars()` to fit a linear model on each permutation
#    - `tidy()` to extract the information in the linear model
#    Your first few rows of `result_boot` should look like this:
#    A tibble: 200 x 2
#      b   permute
#    <int> <list>
#      1 <tibble [2 × 5]>
#      2 <tibble [2 × 5]>
#      3 <tibble [2 × 5]>
#      4 <tibble [2 × 5]>
#      5 <tibble [2 × 5]>
#      6 <tibble [2 × 5]>
#      7 <tibble [2 × 5]>
#      8 <tibble [2 × 5]>
#      9 <tibble [2 × 5]>
#     10 <tibble [2 × 5]>
#     # … with 190 more rows
#     and Your first tibble in permute should be :
#     # A tibble: 2 x 5
#     term        estimate std.error statistic  p.value
#     <chr>          <dbl>     <dbl>     <dbl>    <dbl>
#     (Intercept)    34.1      1.77      19.3  1.89e-18
#     wt             -4.71     0.515     -9.16 3.40e-10
## Do not modify this line!
boot_permute_factory <- function(df){
  n <- nrow(df)
  function() sample_n(df, n, replace = T)
}
myboot <- boot_permute_factory(tibble(x = 1:20)); set.seed(0); myboot()
boot_permute_mtcars <- boot_permute_factory(mtcars)
B <- 200
set.seed(0)
seed <- .Random.seed
result_boot <- tibble(b = 1:B)%>%
  mutate(permute = map(b, function(x) tidy(model_mtcars(boot_permute_mtcars()))))
# 3. Now let's change our strategy and use another bootstrap way: the parametric
#    bootstrap[https://en.wikipedia.org/wiki/Bootstrapping_%28statistics%29#Parametric_bootstrap]
#    Create a function factory `boot_parametric_factory` that takes two inputs `df` and
#    `model`, then fit the `model` on `df`, save the predicted values and residuals
#    using `fitted()` and `resid()`, and in the end returns a function to resample
#    from residuals with replacement, and to return the values of prediction plus the
#    resampled residuals.
#    For example, `boot_parametric_factory(mtcars, model_mtcars)` returns a function that
#    takes no input, but returns the sum of predicted values and resampled residuals.
#    If you run `set.seed(0); myboot <- boot_parametric_factory(tibble(wt = rnorm(10), mpg = wt + rnorm(10)), model_mtcars); myboot()`,
#    the output should be:
#    # A tibble: 10 x 2
#    wt    mpg
#    <dbl>  <dbl>
#     1.26     0.846
#    -0.326    0.154
#     1.33     1.27
#     1.27     0.164
#     0.415   -0.267
#    -1.54    -1.95
#    -0.929   -1.73
#    -0.295    0.684
#    -0.00577 -0.247
#     2.40     1.14
#    Use `set.seed()` to set the random seed to 0 and save your seed into `seed`
#    using `.Random.seed`.
#    Now implement your `boot_permute_factory()` on `mtcars` and `model_mtcars` and save
#    the output into `boot_parametric_mtcars`.
#    Now add a new column `parametric` to `result_boot`, storing all information about
#    linear model for each parametric bootstrap sample.
#    To do this, you can use:
#    - `mutate()` to add the column `parametric`
#    - `map()` to braodcast your operations.
#    - `boot_parametric_mtcars()` to get parametric bootstrap samples
#    - `model_mtcars()` to fit a linear model on each parametric permutation
#    - `tidy()` to extract the information of linear regression
#    Your first few rows of `result_boot` should now look like this:
#    # A tibble: 200 x 3
#    b permute          parametric
#    <int> <list>           <list>
#       1 <tibble [2 × 5]> <tibble [2 × 5]>
#       2 <tibble [2 × 5]> <tibble [2 × 5]>
#       3 <tibble [2 × 5]> <tibble [2 × 5]>
#       4 <tibble [2 × 5]> <tibble [2 × 5]>
#       5 <tibble [2 × 5]> <tibble [2 × 5]>
#       6 <tibble [2 × 5]> <tibble [2 × 5]>
#       7 <tibble [2 × 5]> <tibble [2 × 5]>
#       8 <tibble [2 × 5]> <tibble [2 × 5]>
#       9 <tibble [2 × 5]> <tibble [2 × 5]>
#      10 <tibble [2 × 5]> <tibble [2 × 5]>
#       … with 190 more rows
#    The first tibble in the `parametric` column should be:
#    # A tibble: 2 x 5
#    term        estimate std.error statistic  p.value
#    <chr>          <dbl>     <dbl>     <dbl>    <dbl>
#    (Intercept)    36.1      1.47       24.6 1.82e-21
#    wt             -5.30     0.436     -12.2 4.05e-13
## Do not modify this line!
boot_parametric_factory <- function(df, model){
  function(){
    preds <- fitted(model(df))
    resids <- resid(model(df))
    df$mpg <- preds + sample(resids, length(resids), replace = T)
    df
  }
}
set.seed(0)
seed <- .Random.seed
boot_parametric_mtcars <- boot_parametric_factory(mtcars, model_mtcars)

result_boot <- result_boot %>%
  mutate(parametric=map(b, ~tidy(model_mtcars(boot_parametric_mtcars()))))

result_boot
# 4. Let's extract the information from `result_boot`, create a dataframe `result_boot_tidy`
#    by following these steps:
#    - Use `pivot_longer()` to change the shape of tibble:
#      - Specify `cols = -b` to keep column `b` and transforme on the other columns
#      - Specify `names_to = boot` to differentiate between two bootstrap methods
#    - Use `unnest()` to extract the `term`, `estimate`, `std.error`, `statistic` and
#      `p.value` from each linear model
#    - Use `mutate()` and `ifelse()` to add a column `full` in `result_boot_tidy` which
#      indicate the estimated value of intercept or slope on the whole dataset. Recall
#      that we've fit a model on the original dataset and saved the model into
#      `fit_mtcars`.
#    Your first few rows of `result_boot_tidy` should look like:
#    # A tibble: 800 x 8
#    b     boot       term        estimate std.error statistic  p.value  full
#    <int> <chr>      <chr>          <dbl>     <dbl>     <dbl>    <dbl> <dbl>
#       1 permute    (Intercept)    34.1      1.77      19.3  1.89e-18 37.3
#       1 permute    wt             -4.71     0.515     -9.16 3.40e-10 -5.34
#       1 parametric (Intercept)    39.2      2.04      19.2  2.09e-18 37.3
#       1 parametric wt             -5.76     0.607     -9.48 1.56e-10 -5.34
#       2 permute    (Intercept)    36.3      1.59      22.9  1.56e-20 37.3
#       2 permute    wt             -4.94     0.439    -11.3  2.70e-12 -5.34
#       2 parametric (Intercept)    38.5      1.55      24.9  1.43e-21 37.3
#       2 parametric wt             -5.75     0.462    -12.4  2.23e-13 -5.34
#       3 permute    (Intercept)    33.7      2.08      16.3  2.05e-16 37.3
#       3 permute    wt             -4.44     0.596     -7.45 2.68e- 8 -5.34
#       … with 790 more rows
## Do not modify this line!
result_boot_tidy <- result_boot%>%
  pivot_longer(cols=-b, names_to = 'boot')%>%
  unnest(value)%>%
  mutate(full = ifelse(term!="wt",fit_mtcars$coefficients[[1]],fit_mtcars$coefficients[[2]]))
result_boot_tidy

# 5. Now let's calculate the bootstrap confidence intervals.
#    Save your confidence intervals in a tibble `ci_bootstrap`. The format should look
#    like this:
#    # A tibble: 8 x 5
#    Groups:   boot [2]
#    boot       term        name       lower upper
#    <chr>      <chr>       <chr>      <dbl> <dbl>
#    parametric (Intercept) basic      34.1  41.6
#    parametric (Intercept) percentile 33.0  40.4
#    parametric wt          basic      ...   ...
#    parametric wt          percentile ...   ...
#    permute    (Intercept) basic      ...   ...
#    permute    (Intercept) percentile ...   ...
#    permute    wt          basic      ...   ...
#    permute    wt          percentile ...   ...
#    To do this, you can use:
#    - `group_by()` to group the `result_boot_tidy` by `boot` and `term`
#    - `summarize()` to add summay columns
#      - `basic` will be the summary that stores the bootstrap confidence intervals using
#        standard bootstrap interval.
#        You can refer to the function `ci_basic_bootstrp()` on page 40 of
#        the lecture slides to get the basic confidence interval
#      - `percentile` will be the summary that stores the bootstrap confidence
#        intervals using the percentile method.
#        You can refer to the function `ci_percentile_bootstrp()` on page 40 of
#        the lecture slides to get the basic confidence interval
#    - `pivot_longer` to save one confidence interval in one row.
#      - Specify `cols = c("basic,"percentile")`
#    - `unnest()` to get the lower and upper bound.
#
#
#
## Do not modify this line!
ci_basic_bootstrap <- function(thetab, thetahat, alpha=.95){
  qb <- quantile(thetab, c((1-alpha)/2,(1+alpha)/2))
  tibble(lower=2*thetahat-qb[2], upper = 2*thetahat-qb[1])
}

ci_percentile_bootstrap <- function(thetab, thetahat, alpha=.95){
  qb <- quantile(thetab, c((1-alpha)/2,(1+alpha)/2))
  tibble(lower= qb[1],upper=qb[2])
}
ci_basic_bootstrap(39.2,2.04)

ci_bootstrap <- result_boot_tidy%>%
  group_by(boot, term)%>%
  summarize(basic=map(full[1], ~ci_basic_bootstrap(estimate,.x)), 
            percentile = map(full[1], ~ci_percentile_bootstrap(estimate,.x)))%>%
  pivot_longer(cols=c("basic", "percentile"))%>%
  unnest(value)
ci_bootstrap
