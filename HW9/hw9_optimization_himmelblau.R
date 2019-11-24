# HW9: Optimizing Himmelblau’s function
#
# In this exercise, we will use different optimization methods to optimize
# Himmelblau’s function, which is a benchmark optimization function commonly used.
# Himmelblau’s function is defined as follows:
#    f(x,y) = (x^2 + y −11)^2 + (x + y^2 −7)^2
#
# 1. Create a function `himmelblau_helper <- function(x, y)` that takes as input 
#    two scalars `x` and `y` and returns the calculated function value given by 
#    `f(x,y) = (x^2 + y −11)^2 + (x + y^2 −7)^2`.
#    Example : `himmelblau_helper(3, 4) = 148`. 
#    Then, create Himmelblau’s function `himmelblau <- function(para)` that takes a
#    vector `para` of size 2 as argument and return the calculated function value 
#    given by `himmelblau(para) = himmelblau_helper(para[1], para[2])`.
#    Example : `himmelblau(c(3, 4)) = 148`. 
#    This will help us deal with data frames later.
## Do not modify this line!
himmelblau_helper <- function(x, y){
  (x^2 + y-11)^2 + (x + y^2-7)^2
}
himmelblau <- function(para) himmelblau_helper(para[1], para[2])
# 2. Load packages `lattice` and `tidyverse`.
#    Our goal is to visualize the Himmelblau's function in the 
#    two dimensional domain [-4, 4] x [-4, 4].
#    To do that, create a tibble `xy` using:
#      - `crossing()` to create tibble `xy`, inside which create column
#        `x` using `seq()` to generate a vector from -4 to 4 with `length` 
#        set to `101` and `y` using `seq()` to generate a vector from `-4` to `4`
#        with `length` set to `101` as well.
#      - `mutate()` to create column `fnxy` calculated by `himmelblau_helper`.
#    `xy` should look like:
#    # A tibble: 10,201 x 3
#    x     y  fnxy
#    <dbl> <dbl> <dbl>
#    1    -4 -4    26   
#    2    -4 -3.92 20.2 
#    3    -4 -3.84 15.4 
#    4    -4 -3.76 11.4 
#    5    -4 -3.68  8.21
#    6    -4 -3.6   5.80
#    7    -4 -3.52  4.12
#    8    -4 -3.44  3.13
#    9    -4 -3.36  2.77
#    10    -4 -3.28  3.02
#    # … with 10,191 more rows
## Do not modify this line!
library(lattice)
library(tidyverse)
xy <- crossing(x=seq(-4,4,length.out = 101),y=seq(-4,4,length.out = 101))%>%
  mutate(fnxy=himmelblau_helper(x,y))
xy

# 3. Use `wireframe` to draw 3D plot of the function. You can 
#    set the following arguments :
#      - the first argument should be set to `fnxy ~ x * y`
#      - set `data` to `xy`.
#      - set `shade` to be `True`.
#      - set `scales` to be `list(arrows = FALSE)`, this will allow the axis
#        labels show up.
#      - set `screen` to be `list(z=-300, x=-70, y=0)` to set the plot size.
#      - set `xlab` to 'X', `ylab` to 'Y' and `zlab`='Z'.
#    Save the plot to `himmelblau_plot`.
#    We can see the function is decreasing around the four corners of the 
#    4 x 4 square.
## Do not modify this line!
himmelblau_plot <- wireframe(fnxy~x*y,data = xy,shade=T,scales=list(arrows = FALSE),
                             screen=list(z=-300, x=-70, y=0),xlab='X',ylab="Y",zlab="Z")
himmelblau_plot

# 4. Then, we can draw the contour plot of Himmelblau's function.
#    To do that, you can use:
#      - `ggplot()` to initialize plot object over `xy` and set mapping to be
#        `aes(x = x, y = y, z = fnxy)`
#      - `geom_tile()` with `mapping` set to `aes(fill = fnxy)`.
#      - change the color of the contour by adding `scale_fill_viridis`
#        and set `options` to "plasma".
#      - `stat_contour()` to draw the contour plot, and set `breaks` to be
#        `c(0, 1, 2, 3, 4, 5, 10, 15, 20, 30, 40, 50, 60, 80, 100)`, which is
#         the value of each line.
#      - inside `labs`, set `x` to "X" and `y` to "Y", `fill` to "Z",
#        `title` to `"Contour of Himmelblau function"`.
#      - `theme_light()` for a light background
#    Save the generate plot to `contour`.
## Do not modify this line!
library(viridis)
contour <- ggplot(xy,aes(x=x,y=y,z=fnxy))+
  geom_tile(aes(fill=fnxy))+
  scale_fill_viridis(option = "plasma")+
  stat_contour(breaks=c(0, 1, 2, 3, 4, 5, 10, 15, 20, 30, 40, 50, 60, 80, 100))+
  labs(x="X",
       y = "Y",
       fill="Z",
       title = "Contour of himmelblau function")+
  theme_light()
contour

# 5. Load package `pracma`.
#    We will now use four different optimization methods to find the minima of
#    the Himmelblau's function inside the area [-4, 4] x [-4, 4].
#    Initialize the start point `x0` to be `c(0, 0)`.
#    Firstly, we will try `gaussNewton`. It solves system of equations applying
#    the Gauss-Newton's method. It is especially designed for minimizing a
#    sum-of-squares of functions and can be used to find a common zero of
#    several function. To use `gaussNewton`, simply call `gaussNewton()` and
#    feed `x0` and our objective function `himmelblau` into it. Save the returned
#    element `xs` to `gaussNewton_minima` as our first minima.
## Do not modify this line!
library(pracma)
x0 <- c(0,0)
gaussNewton_minima <- gaussNewton(x0,himmelblau)$xs

# 6. Load package `nloptr`.
#    We will now try `neldermead` method in package `nloptr`. It provides
#    explicit support for bound constraints. Whenever a new point would lie outside
#    the bound constraints the point is moved back exactly onto the constraint.
#    To use Nelder-Mead algorithm, simply call `neldermead()` and set `x0` and
#    `himmelblau` as first two arguments, and set `lower` to be `c(-4, -4)` and
#    `upper` to be `c(4, 4)`. Save the returned element to
#    `neldermead_minima` as our second minima.
## Do not modify this line!
library(nloptr)
neldermead_minima <- neldermead(x0,himmelblau,lower = c(-4,-4),upper = c(4,4))$par


# 7. Next, we will try `softline` method in package `pracma`. It is an inexact 
#    line search algorithm that are used to find the most appropriate step size. 
#    To use `softline()`, we feed `x0` as first argument, and a vector `d0` 
#    representing the direction that we want to minimize. As we start from 
#    original point, we want to explore four directions:
#    `(1, 1)`, `(-1, 1)`, `(-1, -1)` and  `(1, -1)`.
#    Use `softline` to get the optimal solutions for different directions.
#    To do that, you can use:
#      - `crossing()` to generate a tibble consisting of combination of 
#        `x = c(1, -1)` and `y = c(1, -1)`.
#      - `mutate()` to generate column `d0` which represents the direction
#        we want to take, column `a` to represent the calculated gradient,
#        `optimum` to represent the calculated minima and `method` to
#        represent the direction used.
#        To be specific:
#        - use `map2(x, y, c)` to concatenate the column `x` and `y` to create
#          `d0`.
#        - use `map_dbl()` to conduct `softline()` optimization on each
#          `d0` with `x0` over `himmelblau` and create `a`.
#        - use `map2()` to calculate the predicted minima from `d0` and 
#          `a` using the formula: minima = x0 + a * d0 and save it into column
#          `optimum`.
#        - use `paste0s("softline:a=")` to generate method name concatenated with
#          `a` with `round()` to 2 digits.
#    Save the result to `softline_dir` and it should print to:
#    # A tibble: 4 x 6
#    x     y      d0         a    optimum   method          
#    <dbl> <dbl> <list>    <dbl> <list>    <chr>           
#    1    -1    -1 <dbl [2]>  1    <dbl [2]> softline: a=1   
#    2    -1     1 <dbl [2]>  2.97 <dbl [2]> softline: a=2.97
#    3     1    -1 <dbl [2]>  2.84 <dbl [2]> softline: a=2.84
#    4     1     1 <dbl [2]>  2.63 <dbl [2]> softline: a=2.63
#    We should get a sense that this method may not be as powerful as others since
#    it just select the step size of moving direction.
## Do not modify this line!
softline_dir <- crossing(x=c(1,-1),y=c(1,-1))%>%
  mutate(d0 = map2(x, y, c))%>%
  mutate(a = map_dbl(d0,softline,x0=x0,f=himmelblau))%>%
  mutate(optimum = map2(d0,a,function(x,y) x0+x*y),
         method = paste0("softline: a=",round(a,digits = 2)))
softline_dir

# 8. Next we will use `optim()` function with initial parameter `x0` and `method`
#    `L-BFGS-B`. Save the returned element `par` as `optim_minima`.
## Do not modify this line!

optim_minima <- optim(x0,himmelblau,method = "L-BFGS-B")$par

# 9. Summarize the above optimization results into a tibble by doing the following:
#     - use `tribble()`, inside which we set two columns to be `~optimum`, and
#       `~method` and then assign the corresponding minima with its method:
#       `gaussNewton_minima` to `"gaussNewton"`, `neldermead_minima` to `"neldermead"`,
#       and `optim_minima` to `"optim"`.
#     - use `bind_rows()` to concatenate the `method` and `optimum` columns,
#       using `dplyr::select()` to select the two columns from `softline_dir`.
#     - use `mutate()` to convert `optimum` columnn to tibbles using `map()` 
#       to apply `enframe` to all the elements in `optimum`.
#     - separate each element in `optimum` to two columns using `unnest()` 
#       before creating two columns containing `value` for each `name` using `spread()`.
#     - `rename()` column `1` to `x` and column `2` to `y`.
#     - use `mutate()` to calculate himmelblau function value column `fnxy` 
#       using `himmelblau_helper`on `x` and `y`.
#    Save the result to `pts`.
#    `pts` should look like:
#     # A tibble: 7 x 4
#     method          x     y     fnxy
#     <chr>       <dbl> <dbl>    <dbl>
#     1 gaussNewton  3.00  2.00 2.10e- 6
#     2 neldermead   3.00  2.00 1.41e-11
#     3 optim        3.00  2.00 1.46e-12
#     # … with 4 more rows
#     (Notice that here, the first rows of x and y actually have slightly different
#     values but when printed, they are rounded to `2.00` and `3.00`)
## Do not modify this line!
pts <- tribble(~method, ~optimum,
               "gaussNewton", gaussNewton_minima,
               "neldermead", neldermead_minima,
               "optim", optim_minima)%>%
  bind_rows(select(softline_dir,method,optimum))%>%
  mutate(optimum = map(optimum,enframe))%>%
  unnest(optimum)%>%
  spread(name,value)%>%
  rename('x'=`1`,'y'=`2`)%>%
  mutate(fnxy = map2(x,y, function(x,y) himmelblau(c(x,y))))%>%
  unnest(fnxy)
  
pts

# 10. Add the minima found by each method on top of `contour`.
#     To do that, you can use :
#     - `geom_point()` with data set to `pts`, `mapping` as
#     `aes(color=method)` and `size` set to 2.
#     - `labs()`, setting `title` to `"Methods find different local minima"` 
#       and `subtitle` to `"Softline allow to explore different directions"`.
#     - `theme_light()` to set light background.
#     Save the plot to `compare_plot`.
#     We can see that `gaussNewton`, `neldemead` and `optim` can converge to a good
#     local minima in the upper right corner while the `softline` converge worse 
#     compared to them. However, `softline` can be useful if we want to implement
#     gradient descent in helping us find suitable step size in each step, so that 
#     we can search for the best direction while optimizing how much we want to step 
#     in that direction.
## Do not modify this line!
compare_plot <-contour+
  geom_point(data = pts,aes(color=method),size=2)+
  labs(title = "Methods find different local minima",
       subtitle = "Softline allow to explore different directions")+
  theme_light()
compare_plot


