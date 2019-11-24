# HW9: Optimization: Logistic Regression with Newton's Method
#
# In this exercise, we will walk you through the process of implementing Newton's
# method to solve the logistic regression problem.
# We suggest the functions you can use to create the tibbles and the plots, but
# you are free to use the methods you are the most comfortable with.
# Make sure that the outputs look exactly like the ones you are supposed to create.
#
# Throughout the exercise:
#    - Use `%>%` to structure your operations.
#    - Do NOT use `for`, `while` or `repeat` loops for parts other than
#      question 5.
#
# 1. Load the packages `tidyverse`, `numDeriv`, `MASS` and `broom`.
#    Use `read_csv()` to read the dataset `admission.csv` (located in the directory
#    `data/`) into a tibble `data`.
#    In this homework, we would like to fit a logistic regression model on `admit`
#    vs. `gre` and `gpa`. Instead of using the in-built optimization method to
#    find out the coefficient estimates, we will write up our own implementation
#    of Newton's Method to do so.
#    To facilitate analysis, create
#    (1) A matrix `X` of size 400 x 2 for `gre` and `gpa` in `data`.
#        To do this, you can use `dplyr::select()` with `as.matrix()`.
#    (2) A vector `y` of length 400 for `admit` in `data`.
#        To do this, you can use `pull()`.
## Do not modify this line!

library(tidyverse)
library(numDeriv)
library(MASS)
library(broom)
data <- read_csv("data/admission.csv")
X <- dplyr::select(data,gre,gpa)
X <- as.matrix(X)
y <- pull(data, admit)

# 2. We will use the formulas for the hypothesis function, the gradient, the
#    Hessian and the cost function at https://stanford.io/36RbPAj to build some
#    helper functions for our algorithm.
#    First, define a function `sigmoid()` that
#        - Takes an input matrix `x`.
#        - Returns a matrix for S(x) = 1 / (1 + e^(-x)).
#    To check your result, `sigmoid(X[1:5,])` prints to:
#             gre       gpa
#        [1,]   1 0.9736607
#        [2,]   1 0.9751565
#        [3,]   1 0.9820138
#        [4,]   1 0.9604562
#        [5,]   1 0.9493097
## Do not modify this line!

sigmoid <- function(x) 1/(1+exp(-x))

# 3. Define a function `find_grad()` that
#        - Takes an input matrix `X`, a vector `y` and a vector `theta`.
#        - Returns a matrix for the gradient defined in the Newton's Method
#          section at https://stanford.io/36RbPAj.
#          - `m` is the length of `y`.
#          - You will need the `sigmoid()` function you defined previously.
#          - You may need to reorder the terms in the formula as we are doing
#            matrix multiplications (`%*%`) here. Also, it is always helpful to
#            print out the dimensions of the components before gluing them up.
#    To check your result, `find_grad(X, y, rep(0, 2))` prints to:
#             [,1]
#    gre 97.350000
#    gpa  0.587125
## Do not modify this line!

find_grad <-function(X,y,theta){
  m <- length(y)
  1/m*t(X)%*%(sigmoid(X%*%theta)-y)
}

# 4. Define a function `find_cost()` that
#        - Takes an input matrix `X`, a vector `y` and a vector `theta`.
#        - Returns a float for the cost defined in the Newton's Method
#          section at https://stanford.io/36RbPAj.
#          - `m` is the length of `y`.
#          - You will need the `sigmoid()` function you defined previously.
#          - Again, pay attention to matrix multiplications.
#    To check your result, `find_cost(X, y, rep(0, 2))` prints to:
#    [1] 0.6931472
## Do not modify this line!
find_cost <- function(X, y, theta){
  m <- length(y)
  sum(-y*log(sigmoid(X%*%theta))-(1-y)*log(1-sigmoid(X%*%theta)))/m
}
find_cost(X, y, rep(0, 2))

# 5. Define a function `newton()` that
#        - Takes an input matrix `X`, a vector `y`, a float `threshold`
#          defaulted to `1e-5`, and an integer `max_t` defaulted to `50`.
#        - Implements the algorithm.
#        - Returns a matrix `theta` for the final estimates (`theta` should
#          contain a bias estimate as well).
#    The skeleton codes for the function follow as below:
#
#    
#      return(theta)newton <- function(X, y, threshold = 1e-5, max_t = 50) {
#      # Append a bias column of ones to the left of X
#      X <- cbind(rep(...), X)
#
#      # Initialize theta to be a vector of zeros with the same length as ncol(X).
#      theta <- rep(...)
#
#      t <- 0
#      while (t < max_t) {
#        t <- t + 1
#
#        # Compute gradients and the Hessian
#        g <- find_grad(...)
#        H <- hessian(find_cost, theta, method = "complex", X = X, y = y)
#
#        # Break the algorithm when gradient change (2-norm of g) < threshold
#        if (... < threshold) {
#          break
#        }
#
#        # Update theta using the update rule in the Newton's Method section
#        # at https://stanford.io/36RbPAj.
#        # Hint: use `ginv()` to find the inverse ...
#      }
#    }of the Hessian.
#        theta <- 
#
#    Apply your function `newton()` to find the coefficient estimates for `y` vs.
#    `X`. Store the result into a variable `estimate_newton`.
#    To check your result, `round(estimate_newton, 2)` prints to:
#          [,1]
#    [1,] -4.95
#    [2,]  0.00
#    [3,]  0.75
## Do not modify this line!
newton <- function(X, y, threshold = 1e-5, max_t = 50){
  X <- cbind(rep(1,nrow(X)),X)
  theta <- rep(0,ncol(X))
  t <-0
  while(t<max_t){
    g <-find_grad(X,y,theta)
    H <- hessian(find_cost, theta, method = "complex", X=X,y=y)
    if(norm(g,type="2")<threshold) break
    theta <- theta-ginv(H)%*%g
  }
  return(theta)
}
estimate_newton <- newton(X,y)
round(estimate_newton, 2)
# 6. Finally, let's check if our algorithm is correct by comparing it against
#    the build-in method.
#    Create an `lm` object `logit` that fits `admit` against `gre` and `gpa`
#    using `glm(family = "binomial")`.
#    Create a data frame `estimates` of size 3 x 3 with columns `term`, `estimate`
#    and `estimate_newton`, where
#       - `term` categorizes the coefficients.
#       - `estimate` are the estimates in `logit`.
#       - `estimate_newton` are the estimates in `estimate_newton`.
#    To do this, you can use:
#       - `broom::tidy()` to collect the coefficients in `logit`.
#       - `dplyr::select()` to select the desired columns.
#       - `cbind()` to bind the result with `estimate_newton`.
#    To check your result, `estimates %>% mutate_at(c(2, 3), round, 2)` prints to:
#             term estimate estimate_newton
#    1 (Intercept)    -4.95           -4.95
#    2         gre     0.00            0.00
#    3         gpa     0.75            0.75
#    We can see that our algorithm is correct.
## Do not modify this line!
train <- cbind(y,X)
colnames(train) <- c("admit", "gre","gpa")
logit <- glm(admit~gre+gpa,family = "binomial",data=as.data.frame(train))
logit
estimates <- broom::tidy(logit)%>%
  dplyr::select(term, estimate)%>%
  cbind(estimate_newton)
estimates
estimates %>% mutate_at(c(2, 3), round, 2)

