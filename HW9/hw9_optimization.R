# HW9: Optimization
#
# In this exercise, you will use linear programming to solve a problem by
# plotting the feasible region of solution and by using programming tools.
#
# 1. A company makes two products (X and Y) using two machines (A and B).
#    Each unit of X that is produced requires 50 minutes processing time on
#    machine A and 30 minutes processing time on machine B. Each unit of Y that
#    is produced requires 24 minutes processing time on machine A and 33 minutes
#    processing time on machine B. At the start of the current week there are 30
#    units of X and 90 units of Y in stock. Available processing time on machine
#    A is forecast to be 40 hours and on machine B is forecast to be 35 hours.
#    The demand for X in the current week is forecast to be 75 units and for Y
#    is forecast to be 95 units. Company policy is to maximize the combined sum
#    of the units of X and the units of Y in stock at the end of the week.
#    Formulate the problem of deciding how much of each product to make in the
#    current week as a linear program.
#    Let's first solve this linear program graphically by plotting out the
#    feasible region.
#    To do this, you can:
#      - Load `ggplot2` package.
#      - Create a sequence `x` from 30 to 50, with step size 0.1.
#      - Use `as.data.frame()` to change `x` to a data frame.
#      - Use `ggplot()` to initialize a ggplot object.
#        Set its arguments `data` and `mapping`.
#      - Determine the constraints of the maximum running time of machine A and B,
#        represent the constraints by two lines(functions).
#      - Use `stat_function()` to draw the two lines.
#        - Use `annotate()` to add annotation for the line indicating constraint for machine A
#          - specify `x = 35`, `y = 20` and `label = "Constraint of Machine A"`
#        - Use `annotate()` to add annotation for the line indicating constraint for machine B
#          - specify `x = 40`, `y = 32` and `label = "Constraint of Machine B"`
#      - Determine what are the minimum numbers of production for X and Y
#      - Use `geom_hline()` to plot the minimum production for Y
#        - Use `annotate()` to add annotation for the line indicating constraint for Y
#          - specify `x = 40`, `y = 3` and `label = "Constraint of Product Y"`
#      - Use `geom_vline()` to plot the minimum production for X
#        - Use `annotate()` to add annotation for the line indicating constraint for X
#          - specify `x = 48`, `y = 12` and `label = "Constraint of Product X"`
#      - Determine the feasible region and indicate the region by its vertex,
#        use `geom_point()` to point out the vertex in red
#      - `labs()` to format the labels such that:
#        - `title = "Feasible region of Linear Programming"`.
#      - `theme_light()` to change the theme of plots.
#      - `theme()` to change the title and subtitle to the middle of the plot.
## Do not modify this line!

library(ggplot2)
x <-seq(30,50,.1)
g1 <- ggplot(as.data.frame(x))+
  stat_function(aes(x),fun = function(.x) -25/12*.x+100)+
  stat_function(aes(x),fun = function(.x) -10/11*.x+35*20/11)+
  annotate("text", x=35,y=20,label = "Constraint of Machine A")+
  annotate("text", x=40,y=32,label = "Constraint of Machine B")+
  geom_hline(yintercept = 5)+
  annotate("text", x=40,y=3,label = "Constraint of Product Y")+
  geom_vline(xintercept = 45)+
  annotate("text", x=48,y=12,label = "Constraint of Product X")+
  geom_point(aes(x=45,y=5),col='red')+
  geom_point(aes(x=45,y=-25*45/12+100),col='red')+
  geom_point(aes(x = 38*6/5, y=5),col='red')+
  labs(title = "Feasible region of Linear Programming")+
  theme_light()+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))


# 2. The goal is to maximize the combined sum of the units of X and the units of
#    Y in stock at the end of the week, which is the same as to maximize the
#    combined sum of the units produced during this week.
#    For the three vertex in the `g1`, plot three parallel lines with slope -1 by
#    using `stat_function()`.
#    Can you tell what is the optimal point for this linear programming problem?
#    Save your plot into `g2` and save your answer of the optimal `x` and `y` into
#    `x1`, `y1`.
## Do not modify this line!
g2 <-g1+
  stat_function(aes(x), fun=function(.x) -1*.x+50,col='blue')+
  stat_function(aes(x), fun=function(.x) -1*.x+-25*45/12+145,col='blue')+
  stat_function(aes(x), fun=function(.x) -1*(.x-38*6/5)+5,col='blue')
g2
x1 <-45
y1 <- -25*45/12+100


# 3. Now let's load the `lpSolve` package to solve the above problem using the
#    computational tools.
#    To do this, you should follow these steps:
#    - Create a matrix representing the constraints, the dimension should be 4*2.
#      Two rows corresponding to the constraints on machines and two rows corresponding
#      to the constraints on products.
#    - Use `lp` to solve the linear programming.
#      - specify `direction = "max"`
#      - use `>=` for all constraints in `const.dir`
#    Save the output of `lp` into `s1`.
## Do not modify this line!

library(lpSolve)
C <- c(1,1)
A <- matrix(c(-50, -24,
              -30, -33,
              1, 0,
              0, 1), nrow=4, byrow=TRUE)
B <- c(-40*60, -35*60, 45, 5)
s1 <- lp(direction = "max", objective.in = C, const.mat = A, const.dir = c(">=", ">=", ">=", ">="), const.rhs = B)
s1
#s1$solution
# 4. For the above problem, we only consider two variables, thus we can solve it
#    graphically, but if we have more than 3 variables to consider, the
#    graphical way is no longer straightforward.
#    Let's use the `lp` function to solve the following problem:
#    The facility has four machines of type 1, five of type 2, three of type 3
#    and seven of type 4. Each machine operates 40 hours per week. The company
#    has 5 products in total, which generate different profits and have
#    different processing time on different machines. The relationship is
#    shown in the following table:
#    Machine       Quantity   Product1  Product2  Product3  Product4  Product5
#    M1                4         1.2       1.3       0.7        0         0.5
#    M2                5         0.7       2.2       1.6       0.5         1
#    M3                3         0.9       0.7       1.3        1         0.8
#    M4                7         1.4       2.8       0.5       1.2        0.6
#    Unit ptofit $               18        25        10        12         15
#    The units of products can only be integers.The company wants to maximize
#    the profit. Solve this linear programming problem and save the output of
#    `lp` into `s2`.
#    To do this, you should follow these steps:
#    - Create a matrix representing the constraints, the dimension should be 4*5.
#      Each column represents a product and each row represents the constraint on
#      a machine.
#    - Use `lp` to solve the linear programming.
#      - specify `direction = "max"`
#      - use `<=` for all constraints in `const.dir`
#
#
## Do not modify this line!

mat2 <- matrix(c(1.2,1.3,.7,0,.5,
                 .7,2.2,1.6,.5,1,
                 .9,.7,1.3,1,.8,
                 1.4,2.8,.5,1.2,.6),nrow = 4,byrow = T)

s2 <- lp(obj=c(18,25,10,12,15),
         direction = "max",
         const.dir = rep("<=",4),
         const.rhs = c(160,200,120,280),
         const.mat = mat2,
         all.int = T)