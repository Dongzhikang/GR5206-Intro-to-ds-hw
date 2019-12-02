# HW10: Pseudo random generation with Sobol sequences
#
# A low-discrepancy sequence is a sequence with the property that for all
# values of N, its subsequence x1, ..., xN has a low discrepancy.
# Roughly speaking, the discrepancy of a float number sequence is low if the
# proportion of points in the sequence falling into an arbitrary interval I is
# close to proportional to the measure of I (ie. its length), as would happen
# on average (but not for particular samples) in the case of an equidistributed
# sequence.
#
# Such sequences are often used for MC Integration and the method of estimation
# is then called a quasi-Monte Carlo method. The estimation is bounded by both
# a term due to the function to integrate alone and a term related to the
# discrepancy of the sequence used for estimation. Low-discrepancy sequences
# provide both a low bound and a way to avoid re-computation of the whole
# sequence if we decide to augment the estimation with more points and still
# keep low discrepancy.
# Sobol sequences are an example of low-discrepany sequences used for
# quasirandom number generations. We will implement a function to generate
# Sobol sequences and look how the values are distributed across time,
# comparing with other pseudo random distributions.
#
# The sequence consists of values `v_i = m_i / 2^i` where `m_i` are odd
# positive integers smaller than `2^i` (so that the `v_i` are in `[0, 1]`).
# The sequence requires picking an irreducible polynomial with coefficients in
# `{0, 1}` (meaning the polynomial can't be factorized into smaller degree
# polynomials with coefficients in `{0, 1}` as well) defined by:
# `f(x) = z^p + c_1*z^(p-1) + ... + c_(p-1)*z + c_p` with the `c_i` in `{0, 1}`
# Then one has to choose initial values for the sequence.
# The recurrence relation used to generate the `m_i` from the preceding ones is
# `m_i = 2*c_1*m_(i-1) xor 2^2*c_1*m_(i-1) xor ... xor 2^p*c_1*m_(i-1) xor m_(i-p)`
# where `xor` is the bitwise mutually exclusive or (e.g.
# `3 xor 4 = 011 xor 100 = 111 = 7` using their binary representations).
#
# We will use the polynomial `x^4 + x + 1` (ie. the coefficients `0, 0, 1, 1`).
# We will use starting values `m_1 = 1`, `m_2 = 1`, `m_3 = 3` and `m_4 = 13`.
#
# 1. Load the `binaryLogic` package.
#    Assign the coefficients chosen above i.e. `0, 0, 1, 1` to the vector `coeffs`.
#    Compute the value `m5` from the recurrence formula above.
#    To do so you can use `as.binary()` that converts an integer into a vector
#    of its binary representation, `xor()` and `as.numeric()` to get back an
#    integer from a vector of its binary representation.
## Do not modify this line!
library(binaryLogic)
coeffs <- c(0, 0, 1, 1)
co <- c(coeffs,1)
m <- c(13,3,1,1,1)
b <- c(2,2^2,2^3,2^4,1)
m5 <- as.numeric(reduce(as.binary(co*m*b),xor))

# 2. Load the `tibble`, `dplyr` and `purrr` packages.
#    In this question we will compute `next_in_seq`, a function that takes as
#    input the vector of coefficients of the chosen polynomial `coeffs` and
#    the vector of previous values needed to compute the next value `previous`.
#    Elements of `previous` are to be ordered from newest to olders (ie. to
#    compute `m_i`, the first element of `previous` should be `m_(i-1)`, the
#    second `m_(i-2)`, etc.). `coeffs` and `previous` should have the same
#    length.
#    In the body of the function you can:
#    - define a helper function `aux` that takes in two integer arguments `x`
#      and `k` and returns the binary representation of `2^k*x` using
#      `as.binary()`,
#    - compute the element-wise product of `coeffs` and `previous` and assign
#      it to `coeff_x_previous`,
#    - map `aux()` to `coeff_x_previous` and an integer vector of `k` from 1 to
#      the length of `coeffs` using `map2()`. More precisely, you can:
#      - construct a tibble with three columns:
#        - `coeff_x_previous` containing the previously computed vector
#        - `k` containing integers 1, 2, ..., `length(coeffs)` which you can
#          compute using `seq_along()` and `coeff_x_previous`
#        - `tmp` containing the result of mapping `aux()` to the two other
#          columns by calling `map2()` on `coeff_x_previous`, `k` and `aux()`,
#      - `pull()` column `tmp` to obtain a vector
#    - reduce the vector using the `xor()` function and `reduce()`,
#    - add a final `xor()` with the result of the previous step and the binary
#      representation of the last element of `previous` - which you can obtain
#      using `as.binary()`
#    - finally return the result as an integer using `as.numeric()` on the
#      resulting binary representation of the previous step to retrieve the
#      associated integer
#    For example, `next_in_seq(c(0, 1, 1), c(3, 1, 1))` should be equal to `13`
## Do not modify this line!
library(tibble)
library(dplyr)
library(purrr)
aux <- function(x, k) as.binary(2^k*x)
next_in_seq <- function(coeffs, previous){
  tib <- tibble(coeff_x_previous =coeffs*previous,
  k =seq_along(coeff_x_previous))%>%
    mutate(tmp = map2(coeff_x_previous,k,aux))
  tmp <- pull(tib,tmp)
  r <-as.numeric(xor(reduce(tmp,xor),as.binary(previous[length(previous)])))
  return(r)
}
next_in_seq(c(0, 1, 1), c(3, 1, 1))


# 3. Once the `m_i` will all have been computed, we will need to transform them
#    into the `v_i = m_i / 2^i`.
#    Write a function `post_process()` that takes in a generated sequence of
#    `m_i` called `vec` and returns the associated `v_i` vector.
#    For example, `post_process(c(1, 1, 3, 13))` should be equal to
#    `c(0.5000, 0.2500, 0.3750, 0.8125)`
## Do not modify this line!
post_process <- function(vec) {
  for (i in 1:length(vec))
    vec[i] <- vec[i]/2^i
  return(vec)
}
post_process(c(1, 1, 3, 13))

# 4. Now we can compute the `sobol` function, that requires arguments `coeffs`,
#    `n` and `init` and returns the sobol sequence with `n` elements, built
#    using the polynomial with coefficients `coeffs` and starting at `init` (
#    the first values of the sequence). The function should:
#    - initialize the `output` vector to be returned with `init`,
#    - initialize the `previous` vector to be used to compute the next value
#      using `init` (change the order so that the `m_1` is last in `previous`,
#      - while it should be first in `output`),
#    - then call `next_in_seq()` until `output` grows to a size of `n`,
#      updating `previous` after each call
#    - return `post_process(output)`
#    For example, `sobol(c(0, 1, 1), 5, c(1, 1, 3))` should be equal to
#    `c(0.50000 0.25000 0.37500 0.81250 0.15625)`
## Do not modify this line!
sobol<-function(coeffs,n,init){
  output<-c()
  output[1:length(init)]<-init
  previous<-rev(init)
  for(i in (length(init)+1):n){
    mi<-next_in_seq(coeffs,previous)
    output[i]<-mi
    previous<-append(previous,mi,after=0)[1:length(coeffs)]
  }
  post_process(output)
}



# 5. Load the `tibble` and `tidyr` packages.
#    Create the `init` vector `c(1, 1, 3, 13)`.
#    Set the seed by doing:
#    - call `set.seed(11)`,
#    - assign `.Random.seed` to variable `seed`,
#    Construct the `simul_tibble` tibble by:
#    - calling `tibble()` to generate a tibble with 4 columns:
#      - `sequence` containing integers from 1 to 50 - the indices of the
#        sequences' elements,
#      - `sobol` containing the output of `sobol(coeffs, 50, init)`,
#      - `normal` containing a sample of size 50 from the normal distribution
#        with mean `0.5` and standard deviation `0.25`,
#      - `uniform` containing a sample of size 50 from the uniform distribution
#        on `[0, 1]`
#    - calling `pivot_longer()` on all columns except `sequence` to create a
#      tidy tibble.
#    `simul_tibble` should print to:
#    # A tibble: 150 x 3
#       sequence type    value
#          <int> <chr>   <dbl>
#     1        1 sobol   0.5
#     2        1 normal  0.539
#     3        1 uniform 0.921
#     4        2 sobol   0.25
#     5        2 normal  0.328
#     6        2 uniform 0.275
#     7        3 sobol   0.375
#     8        3 normal  0.613
#     9        3 uniform 0.901
#    10        4 sobol   0.812
#    # … with 140 more rows
## Do not modify this line!
library(tibble)
library(tidyr)
set.seed(11)
seed <- .Random.seed
init <- c(1, 1, 3, 13)
simul_tibble <- tibble(sequence = 1:50,
                       sobol = sobol(coeffs, 50, init),
                       normal = rnorm(50,mean = 0.5,sd=.25),
                       uniform = runif(50))%>%
  pivot_longer(-sequence, names_to = 'type', values_to = 'value')
simul_tibble
rnorm(50,.5,.25)
# 6. Load the `ggplot2` package.
#    Plot the sequence values for each type of simulation from `simul_tibble`
#    and assign the plot to `simul_plot`.
#    To do so, you can:
#    - call `ggplot()` on `simul_tibble` with the aesthetic `x = sequence`,
#      `y = value`,
#    - add points for each value using `geom_point()`,
#    - facet the plot by type of simulation using `facet_wrap()` and `type`,
#    - add the following title using `ggtitle()`:
#      `"There is a clear non random pattern in the Sobol sequence generation"`
#    - add `theme_light()`
#
## Do not modify this line!
library(ggplot2)
simul_plot <- ggplot(simul_tibble,aes(x=sequence,y=value))+
  geom_point()+
  facet_wrap(~type)+
  ggtitle("There is a clear non random pattern in the Sobol sequence generation")+
  theme_light()



