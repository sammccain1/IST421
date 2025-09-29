#plot 1
pie(c(10,15,7,20), 
  labels = c("a","b","c","d"), 
  main="How many employees per division?", 
  col = c("red", "tan", "brown", "orange"))

#plot2
df_emp_division <- data.frame(division = c("Sales", "Product", "Marketing", "Retail"), cnt_employees = c(10,15,7, 20))

pie(df_emp_division$cnt_employees,
    labels = df_emp_division$division,
    main = "How many employees per Division?",
    col=c("blue", "blue3", "lightblue", "lightblue3"))

#basic plot aka plot #3
plot(c(1,2,4,8,1,4))

plot(c(1,2,4,8,1,4), pch = 8) #changes the icon of dots on the dot plot #makes specific data point standout

plot(c(1,2,4,8,1,4), pch =c(2,4,5,8,20)) #plot 3
plot(1:6, 1:6, pch = 1:6)

#setting dynamic ranges
n <- 20
plot (1:n, 1:n, pch= 1:n)
plot (1:n, 1:n, pch= 1:n, cex = 2, col = 'orange', main = "OMG ITS A PLOT") #plot 4

#variable assignment (install janitor package, use clean names function)
my_bucket <- c(1,3,7,1)
my_bucket <- "Jason" #bad don't do this

#normal distribution rnorm function
my_bucket <- rnorm(n = 10)
#random data is a good way to test hypothesis and code

plot(my_bucket)
plot(my_bucket, type = "l")
plot(my_bucket, type = 'l', lwd = 3, col = "blue", 
     main = "net worth over time", xlab = 'years', ylab = "dollars")
#adding labels to your plot

plot(my_bucket, type="h")
plot(my_bucket, type='h', lwd =2)
plot(my_bucket, type='h', lwd =2, lty =5)
plot(my_bucket, type='h', lwd =6, col=c('red','orange','brown'),
     bty = "n")

#changing the plot window
?par
original_par_value = par()
plot(my_bucket, type = "h", lwd = 6, col=c('red','orange','brown'), bty = "n",
     bg = "gray")

n<-27
my.letters.1 <- sample(letters[1:3], size = n, replace = T)
my.letters.1

my.letters.table <- table(my.letters.1)
my.letters.table
barplot(my.letters.table)

#options we have to edit bars
barplot(my.letters.table, col=c('brown','tan', 'orange'),
        names.arg = c("sales", "ops", "delivery"),
        main = "Employees By Department",
        border = "white",
        horiz = T,
        density = 10,
        ylab = "employees",
        xlab = "department",
        angle = c(22,70,120))


#png vs pdf comparison
x <- 
