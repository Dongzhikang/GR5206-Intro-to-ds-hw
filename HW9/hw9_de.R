# HW9: Differential evolution
#
# In this exercise, you will show that for problems with many global optima, optim()
# and other gradient-based methods do not work. In this case, differential evolution
# algo can help. Here, you don't need to actually write an algo from scratch,
# but we hope the exercise can teach something.
#
# Throughout the exercise:
#    - Do NOT use `for`, `while` or `repeat` loops.
#    - Use `%>%` to structure your operations.
#    - Use `theme_light()` for the plots.
#    - For graphs with titles, make them use `theme()` with
#      `plot.title = element_text(hjust = 0.5)` and
#      `plot.subtitle = element_text(hjust = 0.5))`.
#
# 1. Create a function called `f_bowl()` which takes in `x` and `y` and then
#    calculate `((0.5*x)^2 + (0.5*y)^2)`.
#
#    Create a function called `f_ackley()` that takes in `x` and `y` and then
#    calculate the formula in the following link:
#    [https://en.wikipedia.org/wiki/Ackley_function].
#
#    Now, maybe you are curious about what 3D shapes of `bowl` and `ackley`
#    look like, so let's draw them.
#    Following visualizations using the `plotly` package.
#    ```
#    library(plotly)
#    grid <- seq(-4, 4, 1e-1)
#    bowl <- outer(grid, grid, f_bowl)
#    ackley <- outer(grid, grid, f_ackley)
#    p1 <- plot_ly() %>%
#      add_surface(x = ~grid, y = ~grid, z = ~bowl) %>%
#      layout(title = "3D shape of bowl")
#    p2 <- plot_ly() %>%
#      add_surface(x = ~grid, y = ~grid, z = ~ackley) %>%
#      layout(title = "3D shape of ackley")
#    ```
## Do not modify this line!
f_bowl <- function(x, y){
  ((0.5*x)^2 + (0.5*y)^2)
}
f_ackley <- function(x, y){
  -20*exp(-.2*sqrt(.5*(x^2+y^2)))-exp(.5*(cos(2*pi*x)+cos(2*pi*y)))+exp(1)+20
}

# 2. Create a function called `obj_bowl()` that takes in `par` and calculates
#    values of `f_bowl()`. Similarly, create a function called `obj_ackley()`
#    that takes in `par` and calculates values of `f_ackley()`.
## Do not modify this line!
obj_bowl <- function(par) f_bowl(par[1],par[2])
obj_ackley <- function(par) f_ackley(par[1],par[2])


# 3. Let's find the minizers of those functions using Newton's method.
#    Use:
#      - `set.seed()` to set your seed to `100`.
#      -`.Random.seed()` and store it to `seed1`.
#      - `runif()` to generate 2 random numbers and save values to `par0`.
#      - `optim()` with `method = "L-BFGS"` to find a minimizer of `obj_bowl()`
#         (DO NOT specify any other parameters in `optim()`).
#    Store the return output into `newton_bowl`. Then, repeat those steps but
#    store the random seed into `seed2`, use `obj_ackley()` instead of
#    `obj_bowl()` and store the output into `newton_ackley`.
## Do not modify this line!
set.seed(100)
seed1 <- .Random.seed
par0 <- runif(2)
newton_bowl <- optim(par = par0,obj_bowl,method = "BFGS")

set.seed(100)
seed2 <- .Random.seed
par0 <- runif(2)
newton_ackley <- optim(par0,obj_ackley,method = "BFGS")
# 4. Now let's use differential evolution to the minizers of those two
#    functions. Load the `DEoptim` package.
#    Then use:
#      - `set.seed()` to set your seed to `100`.
#      -`.Random.seed()` and store it to `seed3`
#      - `DEoptim()` to find the minimer of `obj_bowl()` in the region
#      `[-4, 4]x[-4, 4]`.
#       (DO NOT specify any other parameters in `DEoptim()`)
#    Store the return output into `de_bowl`. Then, repeat those steps but
#    store the random seed into `seed2`, use `obj_ackley()` instead of
#    `obj_bowl()` and store the output into `de_ackley`.
#    When you look at the results of two methods, you can spot the difference
#    and DE does a better job here!
## Do not modify this line!
library(DEoptim)
set.seed(100)
seed3 <- .Random.seed
de_bowl <- DEoptim(obj_bowl,lower = c(-4,-4), upper = c(4,4))
set.seed(100)
seed4 <- .Random.seed
de_ackley <- DEoptim(obj_ackley,lower = c(-4,-4), upper = c(4,4))
# 5. Load the `tidyverse` package.
#    Create a tibble called `value_of_params` that contains the
#    best set of parameters found at each iteration of the optimization.
#    and also the value of `f_ackley` for those points.
#    To do this, you can use:
#      - Subsetting to get the best member at each iteration from
#      `de_ackley`. Note that it corresponds to the `bestmemit` element from
#      `de_ackley`'s `member`. You can take a peak at the structure of
#      `de_ackley` using `str(de_ackley)`.
#      - `as_tibble()` to transform it into a tibble.
#      - `rename()` to change the names into `x` and `y`.
#      - `mutate()` to generate a new column called `iteration` that record the
#      index of the iteration and another column called `objective` that calculate
#      the value of `f_ackley()`.
#    To check your solution, `value_of_params` prints to:
#    # A tibble: 200 x 4
#    x          y         iteration objective
#    <dbl>      <dbl>     <int>     <dbl>
#    1 -0.767   0.0621         1     2.12
#    2 -1.04    0.0194         2     1.10
#    3 -1.04    0.0194         3     1.10
#    4 -0.952   0.0194         4     0.889
#    5 -0.0431 -0.0330         5    -1.44
#    6 -0.0431 -0.0330         6    -1.44
#    7 -0.0431 -0.0330         7    -1.44
#    8 -0.0431 -0.0330         8    -1.44
#    9 -0.0431 -0.0330         9    -1.44
#    10 -0.0431 -0.0330        10    -1.44
#    # … with 190 more rows
## Do not modify this line!
library(tidyverse)
value_of_params<-as_tibble(de_ackley$member$bestmemit)%>%
  rename('x' = 'par1', 'y' = 'par2')%>%
  mutate(iteration = 1:200, objective = f_ackley(x,y))
value_of_params
# 6. Use `value_of_params` to draw a line plot of `y` vs. `x`.
#    To do this, you can use:
#      - `geom_line()` to draw a line plot.
#      - `scale_color_gradient2()` to scale the colors.
#      - `labs()` to name the title as
#         `"We can see that x and y both converge to 0"`.
#    Store the returned graph to `g1`.
## Do not modify this line!
my_theme<-theme_light()+theme(plot.title = element_text(hjust = 0.5),
                              plot.subtitle = element_text(hjust=.5))
g1 <- ggplot(value_of_params)+
  geom_line(aes(x, y, col=objective))+
  scale_color_gradient2()+
  labs(title ="We can see that x and y both converge to 0" )+
  my_theme
g1

# 7. Use `pivot_longer()` to pivot all columns except `iteration` from
#    `value_of_params`. Store the returned tibble to `value_of_params_pivot`.
#
#    Then, use `value_of_params_pivot` to draw a plot of `value` vs.
#    `iteration`, colored by `name`.
#    To do this, you can use:
#      - `geom_line()` to draw a line plot.
#      - `scale_x_log10()` to scale the x-axis.
#      - `geom_hline()` to draw a horizontal line in which intercept of y should
#      take the best value of `f_ackley`. Let the `linetype` be 2
#      - `labs()` to name the
#         - title as
#           `"Evolution of parameters on 200 iterations"`,
#         - subtitle as
#           `"The algorithm approximates more and more to the best parameters and the best value as more iterations are executed"`,
#         - x-axis as `"Iterations"`,
#         - y-axis as `"Value of parameters"`,
#         - color as `"Name"`.
#    Store the returned graph to `g2`.
#
## Do not modify this line!
value_of_params_pivot <- value_of_params%>%
  pivot_longer(cols = c(x, y, objective))


g2 <- ggplot(value_of_params_pivot)+
  geom_line(aes(iteration, value, col = name))+
  scale_x_log10()+
  geom_hline(yintercept = f_ackley(de_ackley$optim$bestmem[1][[1]],de_ackley$optim$bestmem[2][[1]]),linetype=2)+
  labs(title = "Evolution of parameters on 200 iterations",
       subtitle = "The algorithm approximates more and more to the best parameters and the best value as more iterations are executed",
       x = "Iterations",
       y = "Value of parameters",
       color = "Name")+
  theme_light()+
  my_theme
g2
