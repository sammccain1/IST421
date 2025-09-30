# ---- Week 5: Illustrator & Plots! ----
# Authors: Jason Reilly & Jeff Hemlsey
#
library(ggplot2)

# ---- We're throwing together some very basic plots now ----
n <- 2500
y <- seq(0,30, length.out =  n)
x <- sin(y) + (rnorm(n, mean = 0, sd = .15) * (.2 * y))
my.col <- rainbow(31)

# par(xpd = NA)
plot(x, y, xlim = c(-1.5, 1.5)
     , col.lab = "red", col.axis = "red", fg = "red"
     , col = my.col[round(y, 0)])


n2 <- 100
A <- sample(c("here", "there", "nowhere", "everywhere"), size = n2, prob = c(35, 30, 20, 15), replace = T)
B <- sample(c("now", "later"), size = n2, prob = c(45, 55), replace = T)

barplot(table(B,A), beside = TRUE)


pie(table(A))




plot(x, y
     , col.lab = "red", col.axis = "red", fg = "red"
     , col = my.col[round(y, 0)])
