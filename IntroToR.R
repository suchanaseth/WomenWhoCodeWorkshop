# Intro to R
# Data Cleaning
# -------------------------------------------
# Suchana Seth
# December 2014
# For Women Who Code 
# Workshop : Intro to Data Science With R
# -------------------------------------------

# vectors

# vectors have variables of _one_ type
c(1, 2, "three")
# shorter arguments are recycled
(1:3) * 2
# warning! (why?)
(1:4) * (1:3)

# lists
L <- list(x = c(1:5), y = c("a", "b", "c"), z = capColor)
L[[2]]
L$y
L[c(1, 3)]
L[c("x", "y")]
L[["z"]]

# data frames
d <- data.frame(x = 1:10, y = letters[1:10], z = LETTERS[1:10])
d[1]
d[, 1]
d[, "x", drop = FALSE]
d[c("x", "z")]
d[d$x > 3, "y", drop = FALSE]
d[2, ]

# illustrate rep() and seq()


# string basics

# illustrate strsplit()

# illustrate grep()


