# ---- Week 7 Lab ----
# Authors: Jason Reilly & Jeff Hemsley 

# ---- Overview of Lab ----
# 1) Layout
# 2) Communication
# 3) Design

# Welcome to week 7 of lab! Let's take a moment to consider what you have been
# able to accomplish so far this Semester: you have gone from being able to 
# create basic plots in R to now knowing how to use more advanced plot features
# found in Base R and GGplots. You have a wide variety of tools and resources
# for you to allow you to make plots you've never encountered, and make them 
# look polished and professional.

# All amazing things.

# But today's class is going to be different. Today we're going to talk about
# how you start to put plots together.

# Note, you can do this in R - we already worked with the par() function that
# allowed us to put plots together in a useful way. Now we're going to look at 
# some different ways of combining plots using both Base R and GGplots. 

# ---- Libraries ----
library(RColorBrewer) # Let's keep this so you can add in your colors
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr) # This is a new one for you!
library(lubridate) # We talked about this week 1 or 2 - time to revisit

# ---- Critical Functions ----
?layout()
?ggarrange()
?strptime()
?difftime()

# ---- Import Data ----
df_sales <- sales #imported dataset in environment from computer

# ---- Base R Using Par ----
# There are a few steps that we have to go through here. Let's walk through them
# together.

# For this we're going to create some fake data in different vectors:
mth_day <- 1:24
dlt_rec <- rnorm(length(mth_day))
num_records <- runif(n = length(mth_day), min = 2, max = 9)
num_records <- num_records + dlt_rec^2
type <- sample(c("EX", "CX"), size = length(mth_day), replace = T)

# Let's walk through each vector and what it created!

# Now let's build a matrix, called M:
M <- matrix(
  c(1,1,3
    ,1,1,3
    ,2,2,3)
  ,nrow = 3, byrow = T
)

## --- Building without Layout ----

par(mar=c(0,4,4,2), bty = "n") # 5,4,4,2 # bottom,left,top,right
plot(mth_day,dlt_rec, type="n", bty="n", xaxt="n", ylab="", xlab="", ylim = c(2 * min(dlt_rec), 2 * max(dlt_rec)))
lines(mth_day,dlt_rec, type="p", pch="M", col="darkblue", lty=1, lwd=2, cex=1.6)
lines(mth_day,dlt_rec, type="l", col="darkblue", lty=1, lwd=.5)
mtext(text = "Delta Change in Records", side = 3, line = 1, cex = 1.3)
mtext("Volume", side = 2, line=2)

par(mar=c(5,4,0,2)) # 5,4,4,2
barplot(num_records, names.arg = 1:length(mth_day), col="darkblue", border=NA)
mtext("freqs.", side = 2, line=2)

par(mar=c(4,4,4,4), bty = "n") # 5,4,4,2 # bottom,left,top,right
boxplot(num_records ~ type, col = "darkblue")

# So what happened here? We built three plots right? Are they together? How do
# we get it so all three plots are together?

# Let's use the layout function! Of the above, what do you think we want
# to pass to layout()? Look at the help file then tell me what to put into line
# 67.

layout(M)

# First let's build a line plot with some fun features.

par(mar=c(0,4,4,2), bty = "n") # 5,4,4,2 # bottom,left,top,right
plot(mth_day,dlt_rec, type="n", bty="n", xaxt="n", ylab="", xlab="", ylim = c(2 * min(dlt_rec), 2 * max(dlt_rec)))
lines(mth_day,dlt_rec, type="p", pch="M", col="darkblue", lty=1, lwd=2, cex=1.6)
lines(mth_day,dlt_rec, type="l", col="darkblue", lty=1, lwd=.5)
mtext(text = "Delta Change in Records", side = 3, line = 1, cex = 1.3)
mtext("Volume", side = 2, line=2)

par(mar=c(5,4,0,2)) # 5,4,4,2
barplot(num_records, names.arg = 1:length(mth_day), col="darkblue", border=NA)
mtext("freqs.", side = 2, line=2)

par(mar=c(4,4,4,4), bty = "n") # 5,4,4,2 # bottom,left,top,right
boxplot(num_records ~ type, col = "darkblue")

# So what happened?

# Essentially, layout takes a matrix and treats the numbers inside of it as a
# categorical for bucket 1, bucket 2, and bucket 3. Those are used to define the
# amount of space you have. Let's look at how we could potentially redistribute
# space.

# We used the par() function to control the lines in the plot from the default
# which I provided as a comment. When you make adjustments to that, your plot
# will shift around a bit.

M2 <- matrix(
  c(1,1,3
    ,1,1,3
    ,2,2,2)
  ,nrow = 3, byrow = T
)

# Now let's use M2 to reorganize our plots.
layout(M2)

# Note: NOTHING ELSE WILL CHANGE!

par(mar=c(0,4,4,2), bty = "n") # 5,4,4,2 # bottom,left,top,right
plot(mth_day,dlt_rec, type="n", bty="n", xaxt="n", ylab="", xlab="", ylim = c(2 * min(dlt_rec), 2 * max(dlt_rec)))
lines(mth_day,dlt_rec, type="p", pch="M", col="darkblue", lty=1, lwd=2, cex=1.6)
lines(mth_day,dlt_rec, type="l", col="darkblue", lty=1, lwd=.5)
mtext(text = "Delta Change in Records", side = 3, line = 1, cex = 1.3)
mtext("Volume", side = 2, line=2)

par(mar=c(5,4,0,2)) # 5,4,4,2
barplot(num_records, names.arg = 1:length(mth_day), col="darkblue", border=NA)
mtext("freqs.", side = 2, line=2)

par(mar=c(4,4,4,4), bty = "n") # 5,4,4,2 # bottom,left,top,right
boxplot(num_records ~ type, col = "darkblue")

# So what changed in our plot? And is it helpful? What else would have to change
# in the plot to make this story make sense?

# ---- Turning in a Plot ----
# Pick the plot made with Matrix M or M2. Save that. Use AI or Inkscape to write
# why you used the one you used. Save and turn that one in.

# ---- GGPlots ----
# Alright, our client ACME Wines & Spirits is back for some analytic support. 

# First, we need to build out the categories. Let's look at how do this in base
# R and then the tidyverse:

## ---- Base R Categorical ----
df_br_sales <- df_sales
df_br_sales$price <- "cheap"
df_br_sales$price[df_br_sales$unit.price > 10] <- "mid"
df_br_sales$price[df_br_sales$unit.price > 14] <- "high"

## ---- Tidyverse Categorical ----
df_tv_sales <- df_sales
df_tv_sales <- df_tv_sales %>%
  mutate(price = ifelse(unit.price >= 14, "high",
                        ifelse(unit.price >= 10, "mid", "low")))

# We're going to use tv_sales moving forward. Why? Let's talk about sentiment
# and word choice when presenting to clients! Cheap is often considered
# generally negative - it implies a product is of poor quality, or that a person
# is stingy/miserly. When you are thinking about your client data, it's
# important to use words that are more neutral/descriptive. While cheap is
# accurate - low price makes more sense. It's specific, without the negative
# association.

# Let's make some plots!

p1 <- ggplot(df_tv_sales) + 
  aes(y = recipt, x = expenses, color = price) + 
  geom_point() + 
  geom_smooth() + 
  theme_minimal() + theme(legend.position = "none")

p2 <- ggplot(df_tv_sales) +
  aes(x = units.sold, fill = price) + 
  geom_histogram() +  
  theme_minimal() + theme(legend.position = "none")

p1
p2

# Let's talk through these! What are we trying to show? Is the information
# being presented in a helpful way - What do you SEE?

## ---- GGarrange ----
ggarrange(p1, p2, nrow = 2)

# What does this do? And do you like it more? 

# GGarrange allows us to have a lot of power over multiple plots we've crated. 
# It is, in my mind, a little easier to use compared to layout because it's
# easier to fine tune the structure of the grid within ggarrange vs within a
# matrix and then the par() values. 

# Let's add a plot:

p3 <- ggplot(df_tv_sales) + 
  aes(recipt) + 
  geom_boxplot() + 
  coord_flip() + 
  theme_minimal() + theme(legend.position = "none")

p3

# And now let's shove it in.

ggarrange(p1, p3, p2, nrow = 2, ncol = 2)

# Okay, that's something but what if we want to make it look more like up above?

ggarrange(p1, p3, p2, heights = c(2, 1), widths = c(2, 1),
          ncol = 2, nrow = 2)

# Now we can adjust heights and widths. 

p4 <- ggplot(df_tv_sales) + 
  aes(x = rep.region, fill = type) + 
  geom_bar(position = "dodge") + 
  theme_minimal() + theme(legend.position = "none")

ggarrange(p1, p3, p2, p4, heights = c(2, 0.7), widths = c(2, .7)
          , ncol = 2, nrow = 2)

# Does this work well? Maybe. Let's try something different. To make this work,
# we're going to go back to three charts: P1, P2, and P3a

p3a <- ggplot(df_tv_sales) + 
  aes(recipt) + 
  geom_boxplot(aes(color = price)) + 
  theme_minimal() + theme(legend.position = "none")

p3a

# This is an interesting way to view the plots using nesting:
ggarrange(p1, ggarrange(p2, p3a, ncol = 2), nrow = 2)

# Let's try something else:
ggarrange(ggarrange(p1, p3a, ncol = 2), p2, nrow = 2)

# Let's make it even better:
ggarrange(ggarrange(p1, p3a, ncol = 2), p2, nrow = 2, common.legend = TRUE, legend = "top")

# ---- Save this Plot ----
# Cool. But let's make it EVEN BETTER!!!!!
plt_combined_plots <- ggarrange(ggarrange(p1, p3a, ncol = 2), p2, nrow = 2, common.legend = TRUE, legend = "top")

annotate_figure(plt_combined_plots,
                top = text_grob("ACME Wine & Spirits Product Performance", color = "#000000", face = "bold", size = 14),
                bottom = text_grob("Data source: \n ACME W&S Sales Data", color = "#000000",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                fig.lab = "Figure 1", fig.lab.face = "bold")

# ---- Working with the Tweets Data Set ----
# I'm breaking the rules on importing data all at the top because this is going 
# to be a different import format.

tweets_file <- file.choose()

df_tweets <- read.delim(tweets_file, quote="\"", header = TRUE, sep=",", stringsAsFactors=F)

# What do we have?
dim(df_tweets)
head(df_tweets)
colnames(df_tweets)

# Let's look at media specifically
table(df_tweets$media)

# So let's change the names of the blank to text. 
vt_text_only <- which(df_tweets$media == "")
vt_media_type <- df_tweets$media
vt_media_type[vt_text_only] <- "text"

table(vt_media_type)

# We can do the following to further clean up the photo entries
# NOTE: You might have to run gsub twice. This is a form of regex!
vt_media_type <- gsub(pattern = "photo\\|photo", "photo", vt_media_type)
tb_media_type <- table(vt_media_type)

# Okay let's build a plot:
barplot(tb_media_type)

## ---- Let's Talk Dates ----
df_tweets$created_at[1:10]

# What the actual is this?! 

# date_object <- "Wed Jun 15 11:51:19 +0000 2016"
# new_date_object <- as.POSIXct(strptime(date_object, "%a %b  %d %H:%M:%S +0000 %Y"))

# First, let's look at date_object just like above. Copy and paste it into a new
# line below:

date_object <- "Wed Jun 15 11:51:19 +0000 2016"
new_date_object <- as.POSIXct(strptime(date_object, "%a %b  %d %H:%M:%S +0000 %Y"))
new_date_object

# What happened to the object above? Let's look at this again:

df_tweets$posted.date <- as.POSIXct(strptime(df_tweets$created_at, "%a %b %d %H:%M:%S +0000 %Y"))

#look what happens when the format is wrong
as.POSIXct(strptime(df_tweets$created_at[1:3], "%a %b %d %H-%M-%S +0000 %Y"))

# semi-sophisticated data check
any(is.na(df_tweets$posted.date))


max(df_tweets$posted.date)
min(df_tweets$posted.date)
difftime(max(df_tweets$posted.date), min(df_tweets$posted.date), units = "weeks")
difftime(max(df_tweets$posted.date), min(df_tweets$posted.date), units = "months")
difftime(max(df_tweets$posted.date), min(df_tweets$posted.date), units = "hours")

# Why did months throw an error?

## ---- Let's Look at the Data with Lubridate ----
# Let's use the wday function to look at dates.
barplot(table(wday(df_tweets$posted.date, label = TRUE, abbr = TRUE)))

# And we can also look at hours
barplot(table(hour(df_tweets$posted.date)))

# What is the rate of daily tweets over the whole set?

# Let's go back to strip time
vt_date_only <- as.POSIXct(strptime(format.Date(df_tweets$posted.date, "%m-%d-%Y"), "%m-%d-%Y"))
vt_date_only[1:3]
tb_dates <- table(vt_date_only)
plot(tb_dates)

# Now we can pull out names (month) for this data.
names(tb_dates)[1:3]
plot(strptime(names(tb_dates), "%Y-%m-%d"), as.numeric(tb_dates), type = "l")


df_retweets <- as.data.frame(table(as.Date(df_tweets$posted.date), df_tweets$is_retweet))
colnames(df_retweets) <- c("date", "retweet", "tweets")
df_retweets$date <- as.Date(df_retweets$date)

ggplot(df_retweets, aes(x = date, y = tweets, color = retweet)) + geom_line() + geom_point()
