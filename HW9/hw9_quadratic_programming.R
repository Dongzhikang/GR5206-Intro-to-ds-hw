# HW9: Quadratic Programming
#
# Suppose we have selected 10 stocks from which to build a portfolio.
# We want to determine how much of each stock to include in our portfolio.
#
# 1. Load the `tidyverse` and `quadprog` packages.
#    Load the monthly stock return data during 2012 and 2013 for stocks
#    `"AAPL"`, `"XOM"`, `"GOOG"`, `"MSFT"`, `"GE"`, `"JNJ"`, `"WMT"`, `"CVX"`,
#    `"PG"`, `"WF"` from path `data/stocks_data.csv` using `read_csv()`.
#    Store the result in tibble `stocks_data`.
#    The first rows of `stocks_data` should be:
#    # A tibble: 24 x 11
#      date           AAPL      XOM     GOOG     MSFT       GE      JNJ
#      <date>        <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#    1 2012-01-31  0.127   -0.0120  -0.102    0.138    0.0447   0.00503
#    2 2012-02-29  0.188    0.0330   0.0657   0.0748   0.0182  -0.0126
#    3 2012-03-30  0.105    0.00266  0.0372   0.0164   0.0535   0.0135
#    4 2012-04-30 -0.0260  -0.00450 -0.0567  -0.00744 -0.0244  -0.0130
#    5 2012-05-31 -0.0107  -0.0893  -0.0397  -0.0884  -0.0250  -0.0410
#    6 2012-06-29  0.0109   0.0883  -0.00136  0.0480   0.0917   0.0822
#    7 2012-07-31  0.0458   0.0150   0.0912  -0.0366  -0.00432  0.0246
#    8 2012-08-31  0.0892   0.00518  0.0823   0.0458  -0.00193 -0.0259
#    9 2012-09-28  0.00280  0.0475   0.101   -0.0344   0.0966   0.0219
#   10 2012-10-31 -0.108   -0.00306 -0.0983  -0.0410  -0.0727   0.0277
#    # … with 14 more rows, and 4 more variables: WMT <dbl>, CVX <dbl>,
#    #   PG <dbl>, WF <dbl>
## Do not modify this line!
library(tidyverse)
library(quadprog)
stocks_data <- read_csv("data/stocks_data.csv")



# 2. Transform `stocks_data` into long form so that it can be
#    used to draw the plot.
#    You can use `pivot_longer()` to transform the data.
#    The new column names are `stock` and `return`.
#    Store the result into tibble `stocks_data_longer`.
#    The first rows of its print should be:
#    # A tibble: 240 x 3
#      date       stock   return
#      <chr>      <chr>    <dbl>
#    1 2012-01-31 AAPL   0.127
#    2 2012-01-31 XOM   -0.0120
#    3 2012-01-31 GOOG  -0.102
#    4 2012-01-31 MSFT   0.138
#    5 2012-01-31 GE     0.0447
#    6 2012-01-31 JNJ    0.00503
#    7 2012-01-31 WMT    0.0268
#    8 2012-01-31 CVX   -0.0308
#    9 2012-01-31 PG    -0.0550
#   10 2012-01-31 WF     0.219
#    # … with 230 more rows
## Do not modify this line!
stocks_data_longer <- stocks_data%>%
  pivot_longer(cols=AAPL:WF,names_to = 'stock',values_to = 'return')


# 3. Draw the monthly stock return data for 10 stocks
#    using `stocks_data_longer` and save it in `stock_return_plot`.
#    Please use:
#    - `ggplot()` to initialize a ggplot object. You can set its arguments
#      `data` and `mapping` to plot the `return` column against `date`.
#      of dataset `stocks_data_longer`, setting `group` to `stock` in `aes()`.
#    - `geom_line()` to draw the lines.
#      - Set `color` to `stock` in `aes()`.
#    - `labs()` to set:
#      - `title` to `"10 stocks monthly return rates"`.
#      - `subtitle` to `"There is significant fluctuation in return rates"`.
#      - `x` to `""`.
#      - `y` to `"Monthly Return Rate"`.
#    - `theme_light()` to set light theme (i.e. a light background).
#    - `theme()` to rotate the element text of x axis by 90 degrees.
## Do not modify this line!
stock_return_plot <- ggplot(stocks_data_longer)+
  geom_line(aes(date,return,group=stock,col=stock))+
  labs(title = "10 stocks monthly return rates",
       subtitle = "There is significant fluctuation in return rates",
       x="",
       y = "Monthly Return Rate")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
stock_return_plot

# 4. To consider the risk of deviations in our portfolio return,
#    define the quadratic form `Q(w) = w^T C w`,
#    where
#    - `C` is the covariance matrix of the returns `r_i`.
#    - `w_i` is the fraction of the portfolio value allocated to asset `i`.
#    To solve the portfolio allocation problem, we’ll try to determine
#    the weights `w = (w_1,…,w_{10})` so that the risk function
#    `Q(w)` is minimized.
#    The constraints are:
#    - Normalized `w` i.e.
#      - The sum of `w` should be 1.
#      - Every element of `w` should be between 0 and 1.
#    - A minimum expected monthly return of 1% i.e.
#      the sum of `w_i * r_i` should be greater or equal than 0.01 with `r_i` the
#      average monthly return rate on asset `i`,
#      computed from the data from `stocks_data`.
#    You can use `solve.QP()` to solve this quadratic programming problem
#    and save the result into `sol`.
#    Save the parameters of `solve.QP()` into matrix/vector `Dmat`, `dvec`, `Amat`,
#    and `bvec`.
#    Note that:
#    - The order of the variable i.e. the order of the columns of `Dmat` and `Amat`
#    is `"AAPL"`, `"XOM"`, `"GOOG"`, `"MSFT"`, `"GE"`, `"JNJ"`, `"WMT"`, `"CVX"`,
#    `"PG"`, `"WF"` which is the original order of the `stocks_data`.
#    - `Dmat` should be a `10 * 10` covariance matrix of `stocks_data$return`.
#    - `dvec` should be a zero vector of length 10.
#    - `Amat` should be a `22 * 10` matrix.
#      - The first row makes sure the sum of the variables equals to one.
#      - The second row makes sure the expected monthly return is greater than or equal to 0.01.
#      - The 3rd to 12th rows make sure each variable is greater than or equal to 0.
#      - The 13th to 22nd rows make sure each variable is less than or equal to 1.
#        Remember that the symbol of the equation should be `greater than or equal to`.
#    - `bvec` should be a vector of length 22. It should be set according to `Amat`.
#    Apply the parameters to `solve.QP()` and set `meq = 1`. Remember to use `t(Amat)`.
## Do not modify this line!
Dmat <- cov(stocks_data[,-1])
dvec <- rep(0,10)
A.Equality <- rep(1,10)
mu <-colMeans(stocks_data[,-1])
Amat <- rbind(A.Equality,mu,diag(10),-diag(10))
bvec <- c(1,.01,rep(0,10),rep(-1,10))
sol <- solve.QP(Dmat, dvec,t(Amat),bvec)
sol

# 5. Create a tibble `portfolio` to store the proportion of the stocks.
#    You can use:
#    - `tibble()` to create the tibble.
#      - The fist column `stock`  stores the names of 10 stocks
#      - The second column `proportion` stores the proportion of the
#        according stocks.
#        This data comes from `sol[[1]]` and should be rounded to
#        3 digits using `round()`.
#        Note: Use `sol[[1]]]` instead of `sol$solution` to aviod being
#        identified as cheating.
#    - `mutate()` and `fct_reorder()` to reorder the `stock` column
#      according to `proportion` column.
#    It should print to :
#    # A tibble: 10 x 2
#      stock proportion
#      <fct>      <dbl>
#    1 AAPL       0.01
#    2 XOM        0.162
#    3 GOOG       0
#    # … with 8 more rows
## Do not modify this line!
portfolio <- tibble(stock = colnames(Amat),
                    proportion = round(sol[[1]],digits = 3))%>%
  mutate(stock = fct_reorder(factor(stock),proportion))
portfolio

# 6. Draw the horizontal bar chart for the proportions and save it
#    into `portfolio_plot` using `portfolio`.
#    Please use:
#    - `ggplot()` to initialize a ggplot object. You can set its arguments
#      `data` and `mapping` to plot the `proportion` column of the dataset.
#      Use `aes()` to set parameters `mapping`.
#    - `geom_bar()` to draw the lines.
#      - Set `stat` to `identity`.
#      - Set `fill` to `blue`.
#    - `labs()` to set:
#      - `title` to `"Portfolio"`.
#      - `subtitle` to `"WMT has the highest proportion"`.
#      - `x` to `""`.
#      - `y` to `"Proportion"`.
#    - `theme_light()` to set light background.
## Do not modify this line!
portfolio_plot <- ggplot(portfolio)+
  geom_bar(aes(stock,proportion),stat = 'identity',fill='blue')+
  coord_flip()+
  labs(title = "Portfolio",
       
       subtitle = "WMT has the highest proportion",
       x="",
       y="Proportion")+
  theme_light()
portfolio_plot


