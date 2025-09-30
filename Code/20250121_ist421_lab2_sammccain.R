# ---- Lab 2 ----
# Author: Jason Reilly & Jeff Hemlsey

# ---- Overview of Lab ----
# 1) Working with files
# 2) Data, Data Types, Data Frames
# 3) Data Interrogation
#
# In this lab we will review working with files - where they are located, how to
# find them, how to upload them - and also with what is in those files. What are
# you getting in terms of data, and how do you parse it?
#
# This lab will cover some basic base R.

# ---- Critical Functions for Lab ----
# Please review and explore! I have helpfully set up each line to call the help
# function!

?getwd()
?setwd()
?file.choose()
?choose.dir() # MacOS users tell me what happens if you try to use this function
?dirname()
?basename()
?list.files()
?read.csv()
?paste()
?colnames()
?fix()
?dim()
?str()
?summary()
?plot()

# ---- Closing Comments ----
# We have a lot going on this week! We're going to explore some basics of file
# importing, and also some starting points for beginning your exploratory data
# analysis. This will be critical for choosing your data for your poster project
# and also is pretty important for your career.

# ---- Section 1: Directory Navigation & Opening Files ----
# Please note, we're going to cover manual directory navigation. However, you
# can get around this by using projects. We'll tackle both approaches. Please
# note that you can do whatever approach makes you happy. This is just to help
# you. Note, path slashes ARE important.

## ---- Working Directory ----
# Find out your working directory!
getwd()

# Paste here: "/Users/sammccain"

# Now we're going to set a new working directory. Please pick one.
setwd("c:/") # Windows
setwd("/Users/sammccain") # MacOS

getwd()
# Paste the output here: "/Users/sammccain"

# What happened?

## ---- Using Manual Options for File Selection ----
# Let's go find a file and manually open it. We're going to do that working with
# mtcars.

# Let's create an example file.
df_mtcars_example <- mtcars
write.csv(df_mtcars_example, "df_mtcars_example.csv")

# Remember read.csv from up above? What do you think write.csv does?

# Go find the file 
vt_fname <- file.choose()
vt_fname

# What does vt_fname show in your own words?
# vt_fname shows my file path to the df_mtcars_example.csv in my MAC.
# [EXTRA LINES!]
# [PLEASE DEMONSTRATE YOUR COMMITMENT TO COMMENTS!]

## ---- Choose.dir to do something similar! ----
# Won't work with MacOS! Neener neener! Yes, I'm very mature. In case it
# doesn't show.

vt_my_dir <- "/Users/sammccain"
vt_my_dir
setwd(vt_my_dir)
getwd()

# What is your new wd? Copy and paste it here: "/Users/sammccain"

## ---- Back to fname ----
# If fname is the name of the file we want to work with, we can use that to
# better understand it's structure.

dirname(vt_fname)
basename(vt_fname)

# What else is in your director with the file we want?
list.files(vt_my_dir)

# Remember! R is case sensitive
List.Files(vt_my_dir) #function doesn't exist cuz L and F are capitalized.
# Read the error message since it gives clues to whats wrong.

## ---- Using Projects ----
# Go to File > New Project > New Directory > New Project > [Name] > Click Create
#
# Sometimes the easiest way!

getwd() #"/Users/sammccain/01232025_ist421_lab2_samccain"

## ---- Reading in Files ----
# Let's look at how to read in files!

# We'll assume that this file is in your downloads for now.
# How do you suggest we find the string for importing the data?

# First attempt
file.choose("/Users/sammccain/Downloads/tips.csv")
df_tips <- read.csv("/Users/sammccain/Downloads/tips.csv", 
                    header=TRUE, 
                    stringsAsFactors = F)

# Now, can you do this as a variable? Save the file location to an object. Fill
# in this section to the best of your ability! How do you think it works?


df_tips_from_vector <- read.csv(file= "" ,
                                header = TRUE,
                                stringsAsFactors = F)

# Finally, let's use the environment pane to call this in as:
# df_tips_from_environment

# Let's review.
# What is going on here? We start by calling a function named read.csv. We know
# it is a function because of the (). Which is how we pass over function
# parameters.
#
# We then use the assignment operator to assign it to a user defined variable.
# We can tell it is user defined, because we call it "df_".  
# we know it is a function because of the (). 
#
# Now, how did we get the file name? Did you just type it in? Did you use the
# environment window? No wrong answers!

# What do our data frames contain? Take a look!

df_tips

## ---- Fixing Errors ----
# Remember, R can be very persnickity about exactness! Function names have to be
# called exactly as they are written or R will not know what you are doing.

### ---- CORRECT: ----
# stringsAsFactors

### ---- INCORRECT: ----
# StringsAsFactors
# stringAsFactor

# POP QUIZ: What is the name convention for both of the incorrect function names
# above?
# A) 
# B) 

# What is wrong with the below code? Try to fix it!
df_tips_errors <- read.csv(file="C:\\Users\\Bob\\Dropbox\IST719\Data/tips.csv"
                 , header=TRUE
                 , StringAsFactors = F)

# look for the error!
# red == bad

# make sure your slashes are right
# my.path <- "C:\Users\jjhemsle\Desktop"
# my.path <- "C:/Users/jjhemsle/Desktop/"

## ---- Let's Get a New File ----

file.choose()
vt_my_path <- "/Users/sammccain/Downloads/Wine (1).txt"

# What is something different about this new file?
# Let's put the file name in as a vector.

vt_sales_fname <- "Wine.txt"
vt_sales_file <- paste(vt_my_path, vt_sales_fname, sep="") 

# And now let's put it in!
df_sales <- read.table(vt_sales_file, 
                       header=TRUE,
                       sep="\t", 
                       stringsAsFactors = F)

## ---- Side Note on Concatinate ----
# Paste is a very useful function:
paste("Jason", "Reilly")
paste("Jason", "Reilly", sep=" ")
paste("Jason", "Reilly", sep="$")

# Paste can also have other functions! What does this create?
paste(sample(letters[1:3], size = 10, replace = TRUE), sample(10:12, size = 10, replace = TRUE), sep = "")

## ---- Loading R, RDA, Data ----
# Only works for rad files that are native R files
file.choose()
load("/Users/sammccain/Downloads/shootings.Rda")
# What did this do and where?

vt_my_junk_data <- list("A", 8, "Now is the time")
vt_my_junk_data #created a new list of data with the 3 values.

save(vt_my_junk_data, file = "Users/sammccain/01232025_ist421_lab2_samccain/tossme.rda")
# What happened here?! Hint! Look in the directory

rm(vt_my_junk_data)

# Now look at your environment! It's gone! If you use the load function on this
# file, it will bring back the vt_my_junk_data! Try it!

find_my_junk_file <- file.choose()
load(myFile)

# When we want to see what we just loaded, look in environment
ls()
save(list = ls(), file = "C:/Users/jason/Documents/20250121_week2_lab/my_current_session.rda")

rm(list = ls())

find_my_session <- file.choose()

load(find_my_session)

## ---- WARNING: Excel Files ----
# Excel files are NOT csv or txt files, which are considered flat files. They
# require their own packages to open and explore.

# ---- Section 2: Exploring Your Data ----
# Let's start basic!
library(XQuartz)
colnames(df_tips)
fix(df_tips)

# must close fix to do anything else

# now you will sometimes need to access parts of the data

# look at first row
# [row, col]
# blank means all
df_tips[1,]
df_tips[1:3, ]


# look at some column
df_tips[, 3]

# lots of data!
# how much is really there?
length(df_tips[, 3])

dim(df_tips)
dim(df_tips)[1]

# shows the types
str(df_tips)

# Notice we only use one number. A data file is like a matrix. It has rows and 
# cols a single vector just has one dimension. You could have a 3D matrix and so 
# you would use 3 values to specify a subset of the data.

library(psych)
df_describe_tips <- describe(df_tips)
df_numeric_tips <- df_tips[,2:3]
pairs(df_numeric_tips)

# Remember: SQUARE BRACKETS ARE FOR SUBSETS


# now there are other ways to get at the data
colnames(df_tips)

# "X" "total_bill" "tip"        "sex"        "smoker"     "day"        "time"       "size" 

df_tips[, 2]
df_tips[, "total_bill"]
df_tips$total_bill
# This is now a vector

df_tips$total_bill[1]
summary(df_tips$total_bill)


## ---- QUIZ talk about D I S T R I B U T I O N S ----
# Distribution is the shape of the data, shows the range and central tendency of
# the data there are often many different ways to look at the same data all the 
# same data, but different views into it.

#   QUESTION: WHAT IS THE DISTRIBUTION OF THIS continuous DATA?
plot(df_tips$total_bill, main = "distribution of total")
plot(sort(df_tips$total_bill), main = "distribution of total")
hist(df_tips$total_bill, main = "distribution of total")
# so what do you know from this plot?
boxplot(df_tips$total_bill, main = "distribution of total")
boxplot(df_tips$tip, main = "tip distribution")

# Let's look at mtcars!
dn_mpg <- density(df_mtcars_example$mpg) # returns the density data
plot(dn_mpg, main = "distribution of total") # plots the results 
polygon(dn_mpg, col="orange", border="blue") 

# What is vt_mpg? It's a new data type.
class(df_tips)
class(dn_mpg)
attributes(dn_mpg) #density ibject

plot(dn_mpg$x, dn_mpg$y, type = "l")

## ---- More about Variable Types ----
# Lets look into another variable.

df_tips$sex
unique(df_tips$sex)
df_tips$sex == "Male"

# must be an exact match
df_tips$total_bill[df_tips$sex == "Male"]
df_tips$total_bill[df_tips$sex == "Female"]

# rows cols
# QUESTION: WHAT ARE THE DISTRIBUTIONS OF TOTAL BILL FOR MALES AND FEMALES?
# first, make two new vectors with just the data we want
vt_male_bills <- df_tips$total_bill[df_tips$sex == "Male"]
vt_male_bills
vt_female_bills <- df_tips$total_bill[df_tips$sex == "Female"]

# ---- TURN THIS IN: Visual 1 ----
par(mfrow = c(1,2))
hist(vt_male_bills, main="dudes")
hist(vt_female_bills, main="gals")


# ---- TURN THIS IN: Visual 2 ----
# now plot the last 6 plots in one view to turn in the with R file.
par(mfrow = c(3,2))
plot(df_tips$total_bill)
plot(sort(df_tips$total_bill))
hist(df_tips$total_bill)
boxplot(df_tips$total_bill)
plot(dn_mpg) # plots the results 
polygon(dn_mpg, col="orange", border="blue") 
plot(dn_mpg$x, dn_mpg$y, type = "l")


# ---- Section 3: Intro dates with sales dataset ----

file.choose()
vt_my_path <- "/Users/sammccain/Downloads/sales.csv"


vt_sales_fname <- "sales.csv"
vt_sales_file <- paste(vt_my_path, vt_sales_fname, sep="") 
df_sales <- data.frame(sales_1_) #this is a much better way ti upload files with MAC. I'm sorry I know I'm in the wrong. 

df_sales$year
df_sales$sale.date




