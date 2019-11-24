# HW9: Spotify song dataset
#
# In this exercise, we will explore the distribution of song attributes
# in different popularity groups.
#
# 1. Load the packages `tidyverse`, `lubridate`.
#    Use `read_csv()` to load the file `spotify.csv`
#    located in the folder `data`.
#    Add popularity label to these songs by doing the following:
#     - use `mutate()` to add column `popularity_label`:
#       - use `cut()` to popularity and set `breaks` to `c(0, 30, 70, 100)`.
#       - use `fct_recode()` to set popularity labels to different intervals:
#          `unpopular = "(0,30]"`,
#          `fair = "(30,70]"`,
#          `popular = "(70,100]"`.
#     - drop `NA` values uysing `drop_na()`.
#    `spotify` should print to:
#    # A tibble: 51,334 x 18
#    artist_name track_id track_name acousticness danceability
#    <chr>       <chr>    <chr>             <dbl>        <dbl>
#    1 Marlon Wil… 48LQlnC… Can I Cal…       0.174         0.523
#    2 Colorblind… 6VPo6yS… Old Cadil…       0.0203        0.811
#    3 Armin van … 6F5JDmp… A State O…       0.152         0.511
#    4 Johann Seb… 6hWhZGZ… Goldberg …       0.977         0.122
#    5 Fr̩d̩ric C…   2CXfRXB… Scherzo N…       0.992         0.318
#    6 Daze        6pss1TM… Oh!              0.0392        0.722
#    7 Kane Brown  3TclTDy… Live Fore…       0.687         0.709
#    8 Daniela Ba… 2lcdI3W… Por el Bo…       0.108         0.912
#    9 Harvey      5VdRbSK… Flow             0.0838        0.233
#    10 The Sheepd… 59jWpoY… You Got t…       0.17          0.642
#    # … with 51,324 more rows, and 13 more variables: duration_ms <dbl>,
#    #   energy <dbl>, instrumentalness <dbl>, key <dbl>, liveness <dbl>,
#    #   loudness <dbl>, mode <dbl>, speechiness <dbl>, tempo <dbl>,
#    #   time_signature <dbl>, valence <dbl>, popularity <dbl>,
#    #   popularity_label <fct>
## Do not modify this line!
library(tidyverse)
data <- read_csv("data/spotify.csv")
spotify <- data%>%
  mutate(popularity_label = cut(popularity,breaks = c(0, 30, 70, 100)),
         popularity_label=fct_recode(popularity_label,unpopular = "(0,30]",
                                     fair = "(30,70]",popular = "(70,100]"))%>%
  drop_na()


spotify


# 2. Load `ggplot2`.
#    Draw histogram of danceability to see the different distributions of
#    danceability among different level of popularity
#    To do that, you can use :
#      - `ggplot()` to initialize plot object.
#      - `geom_histogram()` to add the histogram plot:
#        - set `aes` to `aes(x = danceability, y = ..density..)`
#        - set `binwidth` to 0.04.
#        - set `color` to `black` and `fill` to `orange`.
#      - `geom_density()` to add kernel density estimation fit curve:
#        - set `aes` to `aes(x = danceability)`.
#        - set `kernel` to `gaussian`.
#        - set `bw` to 0.06.
#        - set `color` to black.
#      - facet it by `popularity_label`.
#      - inside `labs`, set `x` to `"Danceability"` and `y` to `"Density"`,
#        `title` to `"Danceability distribution varies with popularity"`, 
#        `subtitle` to `"Popular songs are more skewed to have high danceability"`.
#      - `theme_light()` to use light theme.
#    Save the plot to `danceability_dist`.
## Do not modify this line!
library(ggplot2)
danceability_dist <- ggplot(spotify)+
  geom_histogram(aes(x = danceability, y = ..density..), binwidth = .04,color = 'black',fill='orange')+
  geom_density(aes(x = danceability),kernel = "gaussian",bw=.06,color='black')+
  facet_grid(~popularity_label)+
  labs(title = "Danceability distribution varies with popularity", 
  subtitle="Popular songs are more skewed to have high danceability",
  x = "Danceability",
  y = "Density")+
  theme_light()

danceability_dist
# 3. Filter the songs to keep the ones by artist `BTS` using `filter()` 
#    and save to `spotify_bts`. It should print to:
#    # A tibble: 46 x 18
#    artist_name track_id track_name acousticness danceability
#    <chr>       <chr>    <chr>             <dbl>        <dbl>
#    1 BTS         0YguyyC… Go Go - J…      0.222          0.837
#    2 BTS         6u1ILVD… "\x8f\xc1…      0.0704         0.61 
#    3 BTS         4Bkh5uD… DNA - Jap…      0.00335        0.574
#    4 BTS         4a8guR4… Spring Da…      0.104          0.572
#    5 BTS         3N6te5x… MIC Drop …      0.0136         0.664
#    6 BTS         6aJj9Ol… Not Today…      0.00191        0.598
#    7 BTS         6yGKx4J… Best Of M…      0.0366         0.659
#    8 BTS         6ePyl2n… Intro: Si…      0.554          0.775
#    9 BTS         0qrPZ27… OUTRO : C…      0.864          0.729
#    10 BTS         0usLRFL… "Trivia \…      0.0907         0.723
#    # … with 36 more rows, and 13 more variables: duration_ms <dbl>,
#    #   energy <dbl>, instrumentalness <dbl>, key <dbl>, liveness <dbl>,
#    #   loudness <dbl>, mode <dbl>, speechiness <dbl>, tempo <dbl>,
#    #   time_signature <dbl>, valence <dbl>, popularity <dbl>,
#    #   popularity_label <fct>
## Do not modify this line!
spotify_bts <- spotify%>%
  filter(artist_name=="BTS")


spotify_bts
# 4. We now want to observe the tempo distribution of hiphop artist. We will
#    fit a normal distribution.
#    Define function `calc_normal_param <- function(data)`, in which `data`
#    should be a vector. `calc_normal_param` should return a vector consisting 
#    of the two normal parameters `mean` and `sigma` of `data`:
#     - to get the first parameter mean, use `mean(x)` 
#     - to calculate the estimated variance, use formula `E[x^2] - E[x]^2`.
#       (Note: `E[x^2]`` can be calculated by `mean(data ** 2)`)
#    For example, `calc_normal_param(c(0, 1, 2))` should return `[1] 1 0.6666667`.
#    Calculate the parameters of `tempo` of `spotify_bts` and save the result
#    to `tempo_bts`. 
## Do not modify this line!
calc_normal_param <- function(data){
  c(mean(data),mean(data**2)-mean(data)**2)
}

tempo_bts <- calc_normal_param(spotify_bts$tempo)
# 5. Visualize the performance of our parameterized fit.
#    To do that:
#    - Create a list `dparams_tempo`, with element `mean` set to `tempo_bts[1]`
#      and element `sd` set to `sqrt(tempo_bts[2])`.
#    - Draw qq-plot to see how well the above normal distribution fit to BTS tempo:
#      - `ggplot()` on data `spotify_bts` with `aes(sample = tempo)`.
#      - `geom_qq()` to plot the QQ-plot, with `distribution` set to `qnorm`
#         and `dparams` set to dparams_tempo.
#      - `stat_qq_line()` to plot fitted line with `distribution` set to `qnorm`,
#        `dparams` set to `dparams_tempo` and `color` to `"red"`.
#      - inside `labs()`, set `title` to `"Normal distribution fit for BTS tempo"`,
#        `subtitle` to `"Moment parameter estimation fits tempo well"`, 
#        `x` to `"normal distributed value"` and `y` to `"sample value"`.
#      - `theme_light()` to use light theme.
#    Save the generated plot object to `qq_plot_tempo`.
## Do not modify this line!
dparams_tempo <- list(mean=tempo_bts[1],sd = sqrt(tempo_bts[2]))
qq_plot_tempo <- ggplot(spotify_bts)+
  geom_qq(aes(sample = tempo),distribution = qnorm,dparams = dparams_tempo)+
  stat_qq_line(aes(sample = tempo),distribution = qnorm,dparams = dparams_tempo,color='red')+
  labs(title = "Normal distribution fit for BTS tempo",
       subtitle = "Moment parameter estimation fits tempo well",
       x = "normal distributed value",
       y = "sample value")+
  theme_light()
qq_plot_tempo

# 6. Next, we will fit a gamma distribution to `liveness` of popular songs.
#    First, create the following functions :
#    - Moment parameter estimation function:
#      `calc_gamma_param <- function(data)`, in which `data` is a vector, 
#      and returns the estimated parameters for the gamma distribution 
#      (shape and scale) using the moments from the data.
#      Note that the output should be a vector with names `a` and `s`.
#      For example, `calc_gamma_param(c(1, 2))` should return:
#               a         s  
#       4.5000000 0.3333333 
#      (referenced formula: shape = mean^2 / var, scale = var / mean)
#    - Log likelihood function:
#       `nll_gamma_factory <- function(x)`, in which `x` is a vector, and 
#       returns the sum of log likelihood function of all the gamma-distributed 
#       data point in `x`. The returned likelihood function should take `par` as input,
#       which is a vector of the two parameters for gamma distribution and 
#       return the calculated log likelihood of input data `x`.
#       Inside the function:
#       - declare `a <- par[1]` as the shape parameter.
#       - declare `s <- par[2]` as the scale parameter.
#       - calculate `n` to be length of the input vector using `length()`.
#       - calculate `sx` to be sum of the input vector using `sum()`.
#       - calculate `slx` to be sum of the log of each data point in input vector
#         using `sum()` and `log()`.
#       - declare `function(par)` which calculates the log likelihood of data by
#         the following formula:
#         - `(a-1) * slx + n * lgamma(a) + n * a * log(s) + sx / s`
#    - MLE function:
#       `par_gamma` takes data and return the parameters optimized by maximizing
#       the log likelihood of the data.
#       Inside the function:
#       - initialize the parameter `par0` by calling `calc_gamma_param(x)`.
#       - define likelihood function `nll_gamma_x` by calling `nll_gamma_factory(x)`
#       - use `optim()` to optimize the parameter, and set `par0` as first argument,
#        `nll_gamma_x` as second argument, `lower` to 0 and `method` to ""L-BFGS-B".
#       - return the optimized parameter by `optim(..)$par`.
#        For example, `par_gamma(c(1, 2))` should return:
#               a         s 
#       8.6502954 0.1734083 
## Do not modify this line!
calc_gamma_param <- function(x) return(c(a = (mean(x)**2)/var(x),s = var(x)/mean(x)))
nll_gamma_factory <- function(x){
  force(x)
  function(par){
    a <- par[1]
    s <- par[2]
    n <- length(x)
    sx <- sum(x)
    slx <- sum(log(x))
    output <- (1-a) * slx + n * lgamma(a) + n * a * log(s) + sx / s
    return(output)
  }
}
par_gamma <- function(x) return(optim(calc_gamma_param(x),nll_gamma_factory(x),lower=0,method = "L-BFGS-B")$par)
par_gamma(c(1, 2))
# 7. Now, let's fit the predictions using the above methods.
#    To do that, you can :
#     - filter songs of `spotify` with `popularity_label = "popular"` 
#       and save the resulted tibble to `spotify_pop`.
#     - calculate parameter `liveness_pop1` using `par_gamma` (mle estimate).
#     - calculate parameter `liveness_pop2` using `calc_gamma_param`.
#     - create density function `liveness_pop_density1`using 
#       `partial(dgamma, shape =., scale=.)` setting `shape` to 
#       `liveness_pop1[1]` and `scale` to `liveness_pop1[2]`.
#     - create density function `liveness_pop_density2`using 
#       `partial(dgamma, shape =., scale=.)` setting `shape` to 
#       `liveness_pop2[1]` and `scale` to `liveness_pop2[2]`.
#    `spotify_pop` should print to:
#    # A tibble: 1,758 x 18
#    artist_name track_id track_name acousticness danceability
#    <chr>       <chr>    <chr>             <dbl>        <dbl>
#    1 Zé Neto & … 1fyhyOy… Status Qu…       0.59          0.580
#    2 Drake       3mvYQKm… After Dar…       0.0414        0.686
#    3 Julia Mich… 2OvV4Nj… Jump (fea…       0.25          0.654
#    4 Meghan Tra… 7fCNUWi… No Excuses       0.0224        0.827
#    5 Jorge & Ma… 2U94QDS… Terra Sem…       0.357         0.676
#    6 Rels B      5I2I0xd… Buenos Ge…       0.486         0.804
#    7 Capo Plaza  093RgZ7… Tesla (fe…       0.316         0.854
#    8 Mc Davi     1R1Hmdu… Bonita, L…       0.306         0.712
#    9 Tedua       1Oou7m2… Vertigini        0.164         0.579
#    10 Gemitaiz    1Lq5Apq… Davide (f…       0.136         0.797
#    # … with 1,748 more rows, and 13 more variables: duration_ms <dbl>,
#    #energy <dbl>, instrumentalness <dbl>, key <dbl>, liveness <dbl>,
#    #loudness <dbl>, mode <dbl>, speechiness <dbl>, tempo <dbl>,
#    #time_signature <dbl>, valence <dbl>, popularity <dbl>,
#    #popularity_label <fct>
## Do not modify this line!
spotify_pop <- spotify%>%
  filter(popularity_label =="popular")
liveness_pop1 <- par_gamma(spotify_pop$liveness)
liveness_pop2 <- calc_gamma_param(spotify_pop$liveness)
liveness_pop_density1 <- partial(dgamma, shape =liveness_pop1[1], scale=liveness_pop1[2])
liveness_pop_density2 <- partial(dgamma, shape =liveness_pop2[1], scale=liveness_pop2[2])


# 8. Visualize the fit of gamma distribution to liveness of songs.
#     - `ggplot()` on tibble `spotify_pop`.
#     - `geom_histogram()` to draw liveness distribution:
#        - set `aes` to `aes(x = liveness, y = ..density..)`.
#        - set `binwidth` to 0.02, `color` to "black" and `fill` to "orange".
#     - `geom_density()` to draw fitted kernel density estimation:
#        - set `kernel` to "gaussian".
#        - set `bw` to 0.06.
#        - set `color` to "red" and `fill` to "red".
#        - set `alpha` to 0.5.
#     - Draw two parameterized fitted curve using `stat_function`:
#       - set mle curve with `color` set to `"yellow"`.
#       - set moments curve with `color` set to `"green"`.
#     - `labs()` to set `x` to "Liveness", `y` to `"Density"``,
#        `title` to  `"Liveness distribution and density estimates"` and `subtitle` to
#        `"MLE fits better than moments method"`.
#       - `theme_light()` to set a light background.
#    Save the ggplot object into `liveness_gamma_plot`.
## Do not modify this line!
liveness_gamma_plot <-ggplot(spotify_pop)+
  geom_histogram(aes(x = liveness, y = ..density..),binwidth = .02,color='black',fill='orange')+
  geom_density(aes(x = liveness, y = ..density..),kernel="gaussian",bw=.06,color='red',fill='red',alpha=.5)+
  stat_function(aes(liveness),col='yellow',fun = liveness_pop_density1)+
  stat_function(aes(liveness),col='green',fun = liveness_pop_density2)+
  labs(title = "Liveness distribution and density estimates",
       subtitle = "MLE fits better than moments method",
       x = "Liveness",
       y = "Density")+
  theme_light()
liveness_gamma_plot

# 9. Visualize the performance of our mle fit.
#    To do that:
#    - Create a list `dparams_liveness`, with element `shape` set to
#      `liveness_pop1[1]` and element `scale` set to `liveness_pop1[2]`).
#    - Draw qq-plot to see how well the above normal distribution fit to BTS tempo:
#      - `ggplot()` on tibble `spotify_pop` with variables `aes(sample=liveness)`.
#      - `geom_qq()` to plot the QQ-plot, with `distribution` set to `qgamma`
#         and `dparams` set to dparams_liveness
#      - `stat_qq_line()` to plot fitted line with `distribution` set to `qgamma`,
#        `dparams` set to `dparams_liveness` and `color` to "red".
#      - inside `labs()`, set `title` to "Gamma distribution fit for song liveness",
#        `subtitle` to `"Fits liveness well only for small values"`, 
#        `x` to `"gamma distributed value"` and `y` to`"sample value"`.
#      - `theme_light()` to use light theme.
#    Save the generated plot object to `qq_plot_liveness`.
## Do not modify this line!
dparams_liveness <- list(shape = liveness_pop1[1],
                         scale = liveness_pop1[2])
qq_plot_liveness <- ggplot(spotify_pop)+
  geom_qq(aes(sample=liveness),distribution = qgamma,dparams = dparams_liveness)+
  stat_qq_line(aes(sample=liveness),distribution = qgamma,dparams = dparams_liveness,color='red')+
  labs(title = "Gamma distribution fit for song liveness",
       subtitle = "Fits liveness well only for small values",
       x = "gamma distributed value",
       y = "sample value")+
  theme_light()
qq_plot_liveness


