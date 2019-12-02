# HW10: Inverse Transform Sampling
#
# In this exercise, we will learn to use the bootstrap for bias correction.
# The inspiration is from this link and on p126 of the linked book:
# [https://books.google.com/books?id=MWC1DwAAQBAJ&pg=PA139&lpg=PA139&dq=Efron+and+Tibshirani+chapter+10.4&source=bl&ots=_bEGQKn74v&sig=ACfU3U0IFgsMnr3bzhNQNruroDsrwBQLww&hl=en&sa=X&ved=2ahUKEwiu577p4_nlAhVLq1kKHelhAnMQ6AEwAnoECAcQAQ#v=onepage&q&f=false]
# The book talks about two methods to use bootstrap to find the bias of
# estimator. We are going to implement the two methods and draw plots to
# compare the two methods here.
#
# Throughout the exercise:
#    - Do NOT use `for`, `while` or `repeat` loops.
#    - Use `%>%` to structure your operations.
#    - Use `theme_light()` for the plots.
#    - For graphs with titles, use `theme()` to set
#      `plot.title = element_text(hjust = 0.5)` and
#      `plot.subtitle = element_text(hjust = 0.5)`.
#
# 1. Load the `tidyverse` package. Use `read_csv()` to read in `"patch.csv"`
#    from the `data` folder. Create a new tibble with two columns `z` and `y`.
#    The formulas are `z = oldpatch - placebo` and `y = newpatch - oldpatch`.
#    To do this, you can use:
#      - `transmute()` to create the new columns.
#    Store returned tibble to `df`.
#    To check your answer, `df` prints to:
#    # A tibble: 8 x 2
#    z     y
#    <dbl> <dbl>
#    1  8406 -1200
#    2  2342  2601
#    3  8187 -2705
#    # … with 5 more rows
#
#    Let‘s try the first so-called normal method.
#
## Do not modify this line!

library(tidyverse)
df <- read_csv("data/patch.csv")%>%
  transmute(z = oldpatch-placebo, y = newpatch-oldpatch)
df

# 2. Create a function called `ratio()` that takes in `df` as argument. The
#    function calculates the ratio of mean of `y` and mean of `z`. Then,
#    apply `ratio()` to `df` and store the returned value to `theta_hat`.
#    (e.g. Let `df` be `c(y = c(7.06, 4.06), z = c(1.48, 6.18))`,
#    `ratio(df)` prints to:
#    [1] 1.452135)
#
## Do not modify this line!
ratio <- function(df) mean(df$y)/mean(df$z)
theta_hat <- ratio(df)


# 3. Create a function called `boot_df()` that takes in `df` as argument and
#    samples all the columns of `df` with replacement.
#    To do this, you can use:
#      - `sample_frac()` to sample every column with `replace` as `TRUE`.
#    (e.g. set seed as 1, let `df` be `c(y = c(7.06, 4.06), z = c(1.48, 6.18))`,
#    `boot_df(df)` prints to:
#    # A tibble: 2 x 2
#    y     z
#    <dbl> <dbl>
#    1  4.06  6.18
#    2  7.06  1.48
#    )
#
#    Create a function called `boot_ratio()` that takes in `nboot` and `df`
#    as arguments. The function should take `nboot` samples from `df` and
#    apply `ratio()` to every sample. Finally the function returns a vector
#    of returned values of samples.
#    To do this, you can use:
#      - `tibble()` to generate three columns:
#        - The first column is called `b` that stores a vector of integers
#          from 1 to `nboot`.
#        - The second column is called `df_b` that maps `boot_df()` to every
#          integer to `b`. This column should store `nboot` samples.
#          You may use `map()` here.
#        - The third column is called `theta_hat_b`. This column stores
#          ratios of each sample in `df_b`.
#      - `pull()` to pull out `theta_hat_b` from the tibble.
#    (e.g. set seed as 1, let `df` be `c(y = c(7.06, 4.06), z = c(1.48, 6.18))`,
#    `nboot = 2`, `boot_ratio(nboot, df)` prints to:
#    [1] 0.6567138 1.4521353
#    )
#
## Do not modify this line!
boot_df <- function(df) sample_frac(df,replace = T)
boot_ratio <- function(nboot, df){
  unlist(pull(tibble(b = 1:nboot, df_b = map(b,~boot_df(df)), theta_hat_b = map(df_b,ratio)),theta_hat_b))
}

# 4. Use `set.seed()` to set seed to 1 and store it to `seed1`.
#    Here, it's for the bootstrap for different bootstrap lengths!
#    Create a tibble that has 5 columns:
#      - The first column is called `nboot` and it stores a vector of
#        integers `50, 100, 200, 500, 1000`.
#      - The second column is called `theta_hat_b`. Each entry of the column
#        takes a list of ratios of `nboot` samples. (e.g. In the first row,
#        `nboot` is 50, then the first entry of `theta_hat_b` should be a list
#        of ratios with length 50.)
#        (hint: you may use `map()` here and it takes three arguments)
#      - The third column is called `bias`. It takes mean of each every of
#        `theta_hat_b` and then it substract `theta_hat`.
#      - The fourth column is called `theta_hat_corrected` and it is
#        `theta_hat` minus `bias`.
#      - The fifth column is called `type` and the values of all entries are
#        `"normal"`.
#    Then, drop the `theta_hat_b` column. Store the return tibble to
#    `results`.
#    To check your answer, `results` prints to:
#    # A tibble: 6 x 4
#    nboot    bias theta_hat_corrected type
#    <dbl>   <dbl>               <dbl> <chr>
#    1    50 0.0260              -0.0973 normal
#    2   100 0.0118              -0.0831 normal
#    3   200 0.00755             -0.0789 normal
#    # … with 3 more rows
#
#    Now, let's implement the so-called improved method.
## Do not modify this line!
set.seed(1)
seed1 <- .Random.seed
results <- tibble(nboot = c(50, 100, 200, 500, 1000),
                  theta_hat_b = map(nboot,boot_ratio, df=df))%>%
  mutate(bias = unlist(map(theta_hat_b,mean))-theta_hat,
         theta_hat_corrected = theta_hat-bias,
         type = "normal")%>%
  dplyr::select(-theta_hat_b)
results


# 5. Create a function called `rmean()` that takes in a vector of `p` and a
#    vector of `x`. The function should calculate the sum of product of `p`
#    and `x` then divided by length of `x`.
#    (e.g. Let `p = c(0.5, 0.5)`, `x = c(1, 2)`,
#    `rmean(p, x)` prints to:
#    [1] 0.75
#    )
#
#    Create a function called `rratio()` that takes in a vector of `p` and
#    `df`.
#    The function should apply `rmean()` to `y` and `z` columns in `df` and
#    divide the first manipulation by the second manipulation.
#    (e.g. Let `p = c(0.5, 0.5)`,
#    `df` be `c(y = c(7.06, 4.06), z = c(1.48, 6.18))`,
#    `rratio(p, df)` prints to:
#    [1] 1.452135
#    )
#
## Do not modify this line!
rmean <- function(p, x) sum(p*x)/length(x)
rratio <- function(p, df) rmean(p,df$y)/rmean(p,df$z)


# 6. Create a function called `boot_p_ratio()` that takes in `df` and it
#    should return a tibble with a calculated `theta` and a list of
#    calculated `p`.
#    Here, we only have one sample.
#    To do this, you can do as following:
#      - Use `nrow()` to count number of rows of `df` and store it to `n`.
#      - Use `sample()` to generate samples of size `n` with `replace` as
#        `TRUE`.
#      - Use `tabulate()` to count how many times each number between 1 and
#        `n` appears in the sample, then divide all of them by `n`. This is the
#        proportion of each number and store the vector to `p`. Here, the
#        length of `p` should be 8.
#      - Apply `rratio()` to `p` and `df` to calculate the theta
#        and store returned values to `theta`.
#      - Use `tibble()` to create two columns with `theta` and `p`.
#      - Finally return the tibble.
#   (e.g. set seed as 1, let `df` be `c(y = c(7.06, 4.06), z = c(1.48, 6.18))`,
#   `boot_p_ratio(df)` prints to:
#   # A tibble: 1 x 2
#   theta p
#   <dbl> <list>
#   1  4.78 c(0.5, 0.5)
#   )
#
#   Create a function called `mean_p()` that takes in a list of `p` and
#   calculate mean of each index of `p`.
#   To do this, you can use:
#     - `reduce()` to calculate sum of `p` on each index and then divide
#       by the length of `p`.
#   (e.g. let `p` be list(c(0.5, 0.2), c(0.1, 0.3), c(1, 2)),
#   `mean_p(p)` prints to: [1] 0.5333333 0.8333333)
#
## Do not modify this line!
boot_p_ratio <-function(df) {
  n <- nrow(df)
  s <- sample(1:n, n,replace = T)
  p <- tabulate(s, n)/n
  theta <- rratio(p,df)
  return(tibble(theta=theta,p=list(p)))
}
mean_p <- function(p) reduce(p,`+`)/length(p)

# 7. Create a function called `boot_ratio2()` that takes in `nboot` and `df`.
#    The function creates `nboot` samples of `df` and calculates the ratios
#    with the improved method described in the book.
#    Here, we have `nboot` samples.
#    To do this, you can use:
#      - `tibble()` to generate two columns:
#        - The first column is called `b` that stores a vector of integers
#          from 1 to `nboot`.
#        - The second column is called `df_b` that maps `boot_p_df()` to
#          every integer to `b`. This column should store `nboot` samples of
#          `theta` and `nboot` lists of `p`.
#          You may use `map()` here.
#      - `unnest()` to unnest samples in `df_b`.
#      - `summarize()` to summarize the statistics:
#        - The first column is called `theta_hat_b` that stores the mean
#          of `theta`.
#        - The second column is called `p_hat_b` that stores the list of mean
#          of each index `p` from all samples. You may apply `mean_p()` here,
#          and the length of `p_hat_b` should be 8.
#
#     (e.g. set seed as 1, let `df` be `c(y = c(7.06, 4.06), z = c(1.48, 6.18))`,
#     `nboot = 3`, `boot_ratio2(nboot, df)` prints to:
#     # A tibble: 1 x 2
#     theta_hat_b p_hat_b
#     <dbl>       <list>
#     1        1.45 c(0.5, 0.5)
#     )
#
## Do not modify this line!
boot_ratio2 <- function(nboot,df){
  tibble(b = 1:nboot, df_b = map(b, ~boot_p_ratio(df)))%>%
    unnest(df_b)%>%
    summarize(theta_hat_b = mean(theta),
              p_hat_b = list(mean_p(p)))
}


#  8. Use `set.seed()` to set seed to 1 and store it to `seed2`.
#     Here, it's for the bootstrap for different bootstrap lengths!
#     Create a tibble that has 6 columns:
#       - The first column is called `nboot` and it stores a vector of
#         integers `50, 100, 200, 500, 1000`.
#       - The second column is called `theta_hat_p_hat`. Each entry of
#         the column has a value of `theta_hat_b` and a list of values of `p`.
#         (hint: you may use `map()` here and it takes three arguments)
#
#       - Then use `unnest()` to unnest `theta_hat_p_hat` into two columns.
#       - The third column is called `theta_hat`. You may use `map_dbl()`
#         combined with `p_hat_b`, `rratio()` and `df` to calculate the
#         estimated `theta_hat`.
#       - The fourth column is called `bias`. It takes mean of each every of
#         `theta_hat_b` and then it substract `theta_hat`.
#       - The fifth column is called `theta_hat_corrected` and it is
#         `theta_hat` minus `bias`.
#       - The sixth column is called `type` and the values of all entries are
#         `"improved"`.
#     To check your answer, `results2` prints to:
#     # A tibble: 6 x 4
#     nboot    bias theta_hat_corrected type
#     <dbl>   <dbl>               <dbl> <chr>
#     1    50 0.00544             -0.0614 improved
#     2   100 0.00676             -0.0734 improved
#     3   200 0.00685             -0.0808 improved
#     # … with 3 more rows
#
## Do not modify this line!
set.seed(1)
seed2 <- .Random.seed
results2 <- tibble(nboot = c(50, 100, 200, 500, 1000),
                   theta_hat_p_hat = map(nboot, boot_ratio2, df=df))%>%
  unnest(theta_hat_p_hat)%>%
  mutate(theta_hat = map_dbl(p_hat_b, rratio, df=df),
         bias = unlist(map(theta_hat_b,mean))-theta_hat,
         theta_hat_corrected = theta_hat-bias,
         type="improved")%>%
  dplyr::select(nboot,bias, theta_hat_corrected, type)
#  9. Draw a line plot to plot the estimated `bias` vs. `nboot`.
#     To do this, you can use:
#       - `bind_rows()` to bind `results2` to `results`
#       - `ggplot()` to plot the `bias` vs. `nboot`, colored by `type`.
#       - `geom_line()` to draw to lines.
#       - `labs()` to name the title as `"The bias of improved method converge faster than that of the normal method"`,
#         x-axis as `"Nboot"`,
#         y-axis as `"Bias"`,
#         color as `Type`.
#     Store returned graph to `g1`.
## Do not modify this line!
g1 <- bind_rows(results,results2)%>%
  ggplot()+
  geom_line(aes(nboot,bias,color=type))+
  labs(title = "The bias of improved method converge faster than that of the normal method",
       x="Nboot",
       y="Bias",
       color="Type")+
  theme_light()+
  theme(plot.title = element_text(hjust = .5))
g1


