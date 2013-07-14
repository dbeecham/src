#!/usr/bin/r

# c = concatenation


# Assignment
assign("x", c(1, 2, 3, 4, 5))
x <- c(1, 2, 3, 4, 5)
c(1, 2, 3, 4, 5) -> x

x = c(1, 2, 3, 4, 5) # works in most cases

# Sequences
seq(1, 5) -> x # == c(1, 2, 3, 4, 5)
1:5 -> x       # same
seq(5, 1) -> x # reverse
5:1 -> x       # reverse
assign("x", seq(from=1, to=5))


seq = rep(seq(1, 5), times=3) # (1 2 3 4 5 1 2 3 4 5 1 2 3 4 5)
seq = rep(seq(1, 5), each=3)  # (1 1 1 2 2 2 3 3 3 4 4 4 5 5 5)


# Logical vectors
assign("logicalvector", seq(1, 10) > 5) # (F, F, F, F, F, T, T, T, T, T)


# NA = Not Available (there are many types of NA)
# NaN = Not a Number
is.na(NA) # true
is.nan(NaN) # true
is.na(NaN) #true
is.nan(NA) #false

is.nan(0/0) #true
is.nan(Inf - Inf) # true

is.na(c(seq(1, 3), NA)) # (F, F, F, T)
is.na(c(1:3, NA)) # same as above

"string"
'string'

c("x", "y", "z") # ("x" "y" "z")
paste("x", "y", "z") # "x y z"
paste("x", "y", "z", sep="") # "xyz"
paste("x", seq(1, 10)) # "x1" "x2" "x3" "x4" "x5" "x6" "x7" "x8" "x9" "x10"


# index vectors
assign("x", c(2, 3, 5, 6, 8, 9))

# logical vector
x[c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)] # (2, 5, 6)
x[x > 3] # (5, 6, 8, 9)
x[!is.na(x)] # select against NA

# integer vector
x[1:6] # (2, 3, 5, 6, 8, 9)
x[c(1, 3, 5)] # (2, 5, 8)
x[6] # (9)

# negative integer vector
# same as above but exclusive instead of inclusive
x[-6] # (2 3 5 6 8)
x[-2:-1] # (5 6 8 9)


# character strings
# object needs a 'names' attribute:
names(x) <- c("fry", "leela", "amy", "zoidberg", "professor farnsworth", "hermes")
x[c("fry", "amy")] # (2, 5)


# It's possible to write to these vectors...
x[c(-2, -4, -5)] <- 0 # x = (2, 0, 5, 0, 0, 9)

# modes:
#   numeric
#   complex
#   logical
#   character
#   raw

mode(c(1, 2, 3)) # "numeric"
length(c(1, 2, 3)) # 3

x <- numeric() # empty object
length(x) # 0
x[3] <- 17 # is now (NA NA 17)
length(x) # 3
length(x) <- 5
length(x) # 5


attributes(x) # useful attributes...

attr(z, "dim") <- c(10, 10) # z is now a 10-by-10 matrix



# classes
#   numeric
#   logical
#   character
#   list
#   matrix
#   array
#   factor
#   data.frame


z <- c(1, 2, 3, 4, 5, 6)
dim(z) <- c(2, 3) # is now a 2 by 3 matrix, indexed by "column major order",
                  # [1 1] [2 1] [1 2] [2 2] [1 3] [2 3]

Z <- c(1, 2, 3, 4, 5, 6)
z <- array(Z,  dim=c(2, 3)) # same as above

z <- c(1, 2, 3, 4, 5)
dim(z) <- c(2, 3) # generates an error, length(z) != 2 * 3
z <- array(z, dim=c(2,3)) # produces a warning, but z is recycled to fill the
                          # 2 * 3 array


x <- c(1.2, 0.9, 0.8, 1.2)
x.mean <- sum(x)/length(x) # = mean(x)
x.var  <- sum((x - x.mean)^2)/(length(x) -1) # = var(x)
