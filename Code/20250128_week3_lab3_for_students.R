# ---- Lab 3 ----
# Author: Jason Reilly & Jeff Hemlsey

# ---- Overview of Lab ----
# 1) Installing packages with dependencies
# 2) Finding Data Sets
# 3) Multi-dimensional Plots
#
# In this lab we're going to be working with base R to build multidimensional
# plots. These kind of plots are critical as they allow us to highlight and
# demonstrate different aspects of the data for comparision purposes. I am also
# going to introduce some concepts of tidyverse and tidy principles to you for
# working with data.

# ---- Important Business ----
# Announcements:
# 1. Medallia Experience Conference is from March 24th to 26th at the Wynn, in
#    Las Vegas. My presentation is scheduled for Tuesday morning. I am going
#    to try to find someone able to cover these sections OR we can do them
#    online. We shall have a poll!
# 
# 2. Mental Health is super critical. No self-disclosure in front of class
#    but I want everyone to know that the resource center is available and is
#    ready to help students with any number of learning disabilities or to help
#    find resources for mental health concerns. Remember - your brain is just
#    as much a part of your body as your arm. If your arm is broken are you
#    going to just "soldier through?" NO! Ask for help! If you do not feel
#    like you can talk to me - not a problem! The school has many resources:
#    
#    https://syr-accommodate.symplicity.com/public_accommodation/
#    https://disabilityresources.syr.edu/
#    https://experience.syracuse.edu/bewell/mental-health/counseling/
#
#    No one will care for your brain as much as you do. Please make sure you are
#    advocating for yourself. 

# ---- Critical Functions for Lab ----
# Please review and explore! I have helpfully set up each line to call the help
# function!

## ---- Library Needed ----
library(tidyverse) # You should have installed this on week 1!
library(lattice)
library(vioplot) 

# Remember, in general, it's best practice to use install.packages("") in the
# console instead of a script.

# So what happened when you tried to run vioplot or install it?

## ---- Fun Functions ----

?aggregate() 
?abline()
?rug()
?tapply()
?as.numeric()
?as.Date()
?strptime()
?densityplot()

# ---- Opening Comments ----
# Lots of great material this week, and we're going to start looking at how to
# pick a data set for you to explore. This will be due for discussion next week!
# I'm really looking forward to seeing what you guys choose/find. We'll switch
# to our slides, then come back to discuss the lab.

# ---- Section 1: Explore Your Data ----

# Let's go get our sales data.

df_sales <- read.csv(file=file.choose(),
                  header = TRUE, 
                  stringsAsFactors = FALSE)

# No surprises here! We covered this last week! So let's learn about our data
# set. What does it contain?

str(df_sales)
colnames(df_sales)

library(psych)
df_sales_description <- describe(df_sales)

# ---- Section 2: Relationships Between a Single Variable Type ----
## ---- QUESTION: What is the relationship between expenses and income? ----

# what kind of vars are these?
df_sales$expenses[1:10]
df_sales$income[1:10]

# relationships of continuous by continuous data
plot(df_sales$expenses, df_sales$income, main = "scatter")

# How do we see what kind of relationship it is?
abline(lm(df_sales$income ~ df_sales$expenses), col="#722F37", lwd = 3)

# Trend lines from linear models! I told you you will do a lot of these! What 
# does this linear model look like?

summary(lm(df_sales$income ~ df_sales$expenses))

# Note, this is an example of *nested* coding. What does this tell us?

# What lines can we add? Any line we want!
abline(h = 400, col = "blue")
abline(v = 9, col = "blue")

# Let's add some more information. This gives 
rug(x = df_sales$income, side = 2, col = "orange")
rug(x = df_sales$expenses, side = 1, col = "orange")

# ---- Section 3: Mixing Variable Types ----
# QUESTION: What is the relationship between income and type?

# Continuous and Categorical
df_sales$type[1:10]
# Why is this a categorical?

boxplot(df_sales$expenses ~ df_sales$type)
# what is this tilda thing doing?

# ---- Section 4: Which Region Sells the Most Units ----
# Let's go back to our data and take a look at what we have:
df_sales$units.sold[1:10]
df_sales$rep.region[1:10]

# this isn't the number of units sold by region, but the 
# distribution of units sold for each sale (each row is a sale)
boxplot(df_sales$units.sold ~ df_sales$rep.region, main="WRONG")

# Why does this not answer the question about region sales?

## ---- We have to UNDERSTAND our Data ----
# what is one row?
# WHAT DO you think is in the unit price column?
str(df_sales)

# So we have to first find the sum of units sold within each region
df_sales$units.sold
sum(df_sales$units.sold[df_sales$rep.region == "East"])
sum(df_sales$units.sold[df_sales$rep.region == "West"])

# EEK! This is manual and that's NOT fun.

# So let's use the base R function aggregate to do this for us.
df_units_by_region <- aggregate(df_sales$units.sold, 
                                by = list(df_sales$rep.region), 
                                FUN=sum)

# What did we learn?
view(df_units_by_region)

# Now let's plot that out!
barplot(df_units_by_region[,2], names.arg = df_units_by_region[,1], main = "Not Multidimensionly plot")

# ---- Section 5: Two Dimensional Bar Plots ----
# QUESTION: How do the units sold of red vs white wine differ by region?
# First we have to find the sum for each type (red and white) for each region

df_units_by_reg_type <- aggregate(df_sales$units.sold
                               , by = list(df_sales$rep.region, df_sales$type)
                               , FUN=sum)

view(df_units_by_reg_type)
# But this is sort of hard to work with, as it is in a longer format.

mx_units_by_applied_reg_type <- tapply(df_sales$units.sold, list(df_sales$rep.region, df_sales$type), sum)

# what tapply did is to sum units sold across the other fields, it returns a 
# matrix for us to work with.

class(mx_units_by_applied_reg_type)
colnames(mx_units_by_applied_reg_type)
rownames(mx_units_by_applied_reg_type)

# stacked bar plots are bad. I often do not give credit for them.
barplot(mx_units_by_applied_reg_type)

## ---- An Example of a Multidimensional Plot ----
# So this plot is, actually, a multi-dimensional plot because it shows us 
# frequency or count within two different vars. We can make comparisons within 
# RED or by regions or both. Let's take a look and then ask ourselves - which is
# better?

barplot(mx_units_by_applied_reg_type, beside = T, legend.text = rownames(mx_units_by_applied_reg_type))

# Let's look at something slightly different
mx_new_type_breakout <- tapply(df_sales$units.sold, list(df_sales$type, df_sales$rep.region), sum)
barplot(mx_new_type_breakout, beside = T, legend.text = rownames(mx_new_type_breakout))

# What changed?

# ---- Section 6: Working with Dates ----
# QUESTION: Are incomes growing over time for each region?
colnames(df_sales)

mx_income_by_region <- tapply(df_sales$income, list(df_sales$rep.region, df_sales$year), sum)

plot(mx_income_by_region[1,], type = "l")

vt_years <- as.numeric(colnames(mx_income_by_region))

options(scipen=999)
plot(vt_years, mx_income_by_region[1,], type = "l", col="red", lwd=2, 
     ylab = "Income ($)", 
     xlab = "Year", 
     ylim = c(0,max(mx_income_by_region)), bty = "n")
lines(vt_years, mx_income_by_region[2,], col="blue", lwd=2)
lines(vt_years, mx_income_by_region[3,], col="orange", lwd=2, lty=2)

# Legend Options: "bottomright", "bottom", "bottomleft", "left", "topleft", 
# "top", "topright", "right" and "center"

legend('bottomleft', legend = rownames(mx_income_by_region), lwd=2, 
       lty=1, col=c('red', 'blue', 'orange', 'green', 'brown'), 
       bty='n', cex=.75)

### ---- SAVE THIS PLOT ----
# Okay, add lines to the above plot for the remaining two regions. Make sure
# that your lines match the proper data colors and symbology.

## ---- Dates as a Time Point ----

# Let's look at our Data set again.
colnames(df_sales)

# Sales Date looks promising. Let's dig into it:
df_sales$sale.date

# What does this look like?

# Let's use strptime to turn our dates into characters!
vt_date <- as.Date(strptime(df_sales$sale.date[df_sales$year == 2014], "%m/%d/%Y"))
?strptime() # Learn more!

# Now, let's go through and get our data properly organized.
x <- seq.Date(from = min(vt_date), to = max(vt_date), by = "day")
y <- rep(0, length(x))

tb_date_tab <- table(vt_date)
y[match(as.Date(names(tb_date_tab)), x)] <- as.numeric(tb_date_tab)

# Initial plot to show you the date information organized.
plot(x,y, type = "l", col="red")

# And let's play around with it a bit!
df_2014_data <- aggregate(df_sales$income[df_sales$year == 2014], list(df_sales$sale.date[df_sales$year == 2014]), sum)
df_2014_data$date <- as.Date(strptime(df_2014_data$Group.1, "%m/%d/%Y"))
plot(df_2014_data$date, df_2014_data$x, type = "h")

# ---- Section 7: Working with Lattice ----
# We activated the library at the beginning of lab.

densityplot(~income, group = rep.region, data = df_sales, auto.key = TRUE)
densityplot(~ income | rep.region, data = df_sales)

# How are these two charts different? What looks familiar here?

# ---- Section 8: Combining Multiple Chart Types ----
# Working with multiple views of income! How would you change the titles to 
# make them more useful?

par(bty = "n", mfrow=c(3,2))
hist(df_sales$income, main = "", col = "purple")
mtext(text = "hist(df_sales$income)", side = 3, line = 1, adj = 0, col = "red", cex = 1.2)

boxplot(df_sales$income, col = "purple")
mtext(text = "boxplot(df_sales$incomme)", side = 3, line = 1, adj = 0, col = "red", cex = 1.2)

plot(df_sales$income, pch = 16, cex = .5, col = "purple")
mtext(text = "plot(df_sales$income)", side = 3, line = 1, adj = 0, col = "red", cex = 1.2)

plot(sort(df_sales$income), pch = 16, cex = .5, col = "purple")
mtext(text = "plot(sort(df_sales$income))", side = 3, line = 1, adj = 0, col = "red", cex = 1.2)

d <- density(df_sales$income)
plot(d, main = "")
polygon(d, col="purple", border="blue") 
mtext(text = 'plot(density(df_sales$income), main = "", lwd = 2)', side = 3, line = 1, adj = 0, col = "red", cex = .9)

# ---- Section 9: Violin Plot ----
## ---- SAVE THIS PLOT ----
library(vioplot)
vioplot(df_sales$income, horizontal=TRUE, col="purple")
mtext(text = 'library(vioplot)\nvioplot(sales$total.sale, horizontal=TRUE, col="orange")', side = 3, line = 1, adj = 0, col = "red", cex = .9)
