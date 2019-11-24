# HW9: Implement Newton-Raphson and observe on a function
#
# In this exercise, you will implement the Newton-Raphson algorithm from
# scratch and use it to find the roots - values for which the function is
# zero - of a real-valued function.
#
# 1. Implement a function that takes in a real number `x` and returns
#    `exp(x*x/(x*x + 2*x + 5))*tanh(x)*sin(x)/(x*(1 + .5/abs(x)))` and call it
#    `f`.
## Do not modify this line!
f <- function(x) exp(x*x/(x*x + 2*x + 5))*tanh(x)*sin(x)/(x*(1 + .5/abs(x)))


# 2. Load the `tibble`, `dplyr` and `ggplot2` packages.
#    Plot the graph of `f` for values between `-30` and `30` (points separated
#    by a step of `0.01`) and assign it to `f_plot`.
#    To do so, you can:
#    - create a tibble with `x` points between `-30` and `30` with a step size
#      of `0.01`, using `tibble()` and `seq()`,
#    - add a column with values `f(x)` using `mutate()` and `f()`,
#    - call `ggplot()` on the tibble,
#    - add `geom_line()`,
#    - add a blue vertical line of equation `x = -3` using `geom_vline()`,
#    - add the following labels (using `labs()`):
#      - `y = "f(x)"`,
#      - `title = "Graph of f"`,
#      - `subtitle = "There are many roots, including one near -3"`
#    - add `theme_light()`
## Do not modify this line!
library(tibble)
library(dplyr)
library(ggplot2)
f_plot <- tibble(x=seq(-30,30,.01))%>%
  mutate(value=f(x))%>%
  ggplot()+
  geom_line(aes(x,value))+
  geom_vline(xintercept = -3,col='blue')+
  labs(y = "f(x)",
       title = "Graph of f",
       subtitle = "There are many roots, including one near -3")+
  theme_light()
f_plot

# 3. The Newton-Raphson method produces successive approximations to the roots
#    of a real-valued function. The most basic version starts with an initial
#    guess `x_0` and generates successive points `x_i` such that
#    `x_(i+1) = x_i - f(x_i) / f'(x_i)` until a convergence criterion has been
#    met or a number of iterations has been reached. We might not have access
#    to the derivative `f'` in some cases, so we will approximate `f'(x)` by
#    `(f(x + h) - f(x)) / h` for small `h`.
#    Do the following:
#    - load the `purrr` package,
#    - assign value `1e-7` to variable `h`.
#    - write one step of the Newton-Raphson algorithm. It should be a function
#      called `one_step` that takes in arguments, `f`, `x` (for `x_i`) and
#      `h` and returns the next value `x_(i+1)` using the recursive formula
#      and the approximation of the derivative above.
#    - write a function that runs several steps and returns a tibble with the
#      values obtained at each step. It should take in arguments `f`, `x`, `h`
#      and `n_iterations` (the number of times to call `one_step()`). Call it
#      `multi_step()`. To write its body, you can do the following:
#      - call `accumulate()`, on `n_iterations` replicas of `h`, function
#        `one_step()`, `f = f` and `init = x`,
#      - pass the result tibble to `enframe()` to output a tibble with two
#        columns (`step` and `x`) as below.
#    the call `multi_step(f, 1, h, 2)` should return:
#    # A tibble: 3 x 2
#       step      x
#      <int>  <dbl>
#    1     1  1
#    2     2 -0.400
#    3     3 -0.130
## Do not modify this line!
library(purrr)
h <- 1e-7
one_step <- function(f,x,h) x-f(x)/((f(x+h)-f(x))/h)
multi_step <- function(f, x, h, n_iterations) {
  enframe(accumulate(rep(h,n_iterations),one_step,f=f,.init = x),name = 'step',value='x')
}
multi_step(f, 1, h, 2)

# 4. Load the `tidyr` package.
#    Create a tibble storing the output of the first 2 iterations of the
#    Newton-Raphson algorithm starting with initial guesses `seq(-7, 7, 2)`,
#    `f` and `h` defined in previous questions. Assign the resulting tibble to
#    `newton_steps_tibble`.
#    To do so, you can:
#    - create a tibble with columns `start = seq(-7, 7, 2)` and `steps`
#      using `tibble()`. `steps` should contain nested tibbles created by
#      `multi_step()` with `f`, `h` and `n_iterations = 1` (you can create the
#      nested tibbles from the `start` column using `map()`),
#    - unnest `steps` using `unnest()`,
#    - use `mutate()` to:
#      - add a column `f` containing the values `f(x)`,
#      - turn `step` and `start` into factors
#    `newton_steps_tibble` should print to:
#    # A tibble: 24 x 4
#       start step      x             f
#       <fct> <fct> <dbl>         <dbl>
#     1 -7    1     -7    -0.298
#     2 -7    2     -6.00  0.151
#     3 -7    3     -6.28  0.00295
#     4 -5    1     -5     0.608
#     5 -5    2     -7.09 -0.325
#     6 -5    3     -5.84  0.232
#     7 -3    1     -3    -0.124
#     8 -3    2     -3.14 -0.000706
#     9 -3    3     -3.14 -0.0000000627
#    10 -1    1     -1    -0.549
#    # … with 14 more rows
## Do not modify this line!
library(tidyr)
newton_steps_tibble <- tibble(start = seq(-7, 7, 2))%>%
  mutate(step=map(start,multi_step,n_iterations=2,h=h,f=f))%>%
  unnest(step)%>%
  mutate(f = f(x),step=factor(step),start = factor(start))
newton_steps_tibble
multi_step(f, -7, h, 3)
# 5. Create a plot showing the first 2 iterations of the Newton-Raphson
#    algorithm starting with initial guesses `seq(-7, 7, pi)`, `f` and `h`
#    defined in previous questions. Points corresponding to each iteration
#    should have a different color. Assign the resulting plot to
#    `newton_steps_plot`.
#    To do so, you can:
#    - call `ggplot()` on `newton_steps_tibble` with aesthetic `(x, f)`,
#    - add `stat_function` with `fun = f`,
#    - add `geom_point()` to show each point of `size = 2` and color them by
#      `step`,
#    - add `geom_hline()` with an `alpha` of `0.2` to show the reference `y=0`
#      line with a bit of transparency,
#    - facet the plot by `start` using `facet_wrap()`
#    - add the following labels (using `labs()`):
#      - `y = "f(x)"`,
#      - `title = "The points get closer to roots of f as steps increase"``
#    - add `theme_light()`
## Do not modify this line!
newton_steps_plot <- ggplot(newton_steps_tibble)+
  stat_function(aes(x,f),fun = f)+
  geom_point(aes(x,f,color = step),size=2)+
  geom_hline(yintercept = 0,alpha=.2)+
  facet_wrap(~start)+
  labs(y = "f(x)",
       title = "The points get closer to roots of f as steps increase")+
  theme_light()
newton_steps_plot


# 6. Write the complete `newton_raphson` function with arguments `f`, `x_0`
#    (float initial guess), `h` and `n_iterations` where `h` is passed default
#    value `1e-7` and `n_iterations` is passed default value `5`.
#    Starting with `i = 1`, while `i <= n_iterations`, the function should call
#    `one_step()` and create the successive guesses, returning the final one.
## Do not modify this line!
newton_raphson <- function(f,x_0,h=1e-7,n_iterations=5){
  i <- 1
  while (i<=n_iterations) {
    x_0 <-one_step(f,x_0,h)
    i<-i+1
  }
  x_0
}


# 7. Use `newton_raphson` on `f` with initial value `-3` and assign the result
#    to `root`.
#    Check that `f(root)` is sufficiently close to zero by checking
#    that the absolute value of `f(root)` is smaller than `1e-8` and assign the
#    result of this evaluation to boolean variable `is_zero` (it should be `TRUE`)
## Do not modify this line!
root <- newton_raphson(f,-3)
is_zero <- f(root)<1e-8

# 8. Add the root to `f_plot` and assign the resulting plot to
#    `plot_with_root`. Modify the scale to visualize it more clearly.
#    To do so, you can:
#    - starting with `f_plot`, add the point corresponding to `root` of using
#      `geom_point()` with `colour = "red"`,
#    - use `coord_cartesian(xlim=...)` to restrict shown x values between -5
#      and 0,
#    - add a blue vertical line of equation `x = -3` using `geom_vline()`,
#    - replace the title from `f_plot` with
#      `"Newton-Raphson finds the closest root to -3 within a few iterations"`
#      and remove the `subtitle`, using `labs()`
#
## Do not modify this line!
plot_with_root <- f_plot+
  geom_point(aes(root,f(root)),color='red')+
  coord_cartesian(xlim=c(-5,0))+
  geom_vline(xintercept = -3,col='blue')+
  labs(title = "Newton-Raphson finds the closest root to -3 within a few iterations",
       subtitle = "")
plot_with_root


