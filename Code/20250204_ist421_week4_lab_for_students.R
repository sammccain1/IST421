# ---- Lab 4 ----
# Author: Jason Reilly & Jeff Hemlsey

# ---- Overview of Lab ----
# 1) GGPlots - The greatest, bestest, most amazingest graphics tool in the world
#
# In this lab we finally get to deep dive into the structure of ggplots and how
# the tidyverse and ggplots work together to help you understand and navigate
# your code! I'm on record at work for saying this: "Tidyverse is life". And I'm
# not even joking. When you get used to thinking about your data in a tidy way,
# it makes your data wrangling, cleaning, and organization skills stronger,
# which makes it faster and easier for you to get from "I have data" to "I have
# an effective analysis". 
#
# Our goal in this week - and more broadly, this whole semester - is to help you
# tell more effective stories with your analysis. How do you go from: "Here is
# everything" to "Here is what matters, and let me tell you why."
#
# During this week's lab, we'll be focusing on learning the grammar of graphics
# which is why it's called GGplots. We are going to learn about aesthetics or
# mapping data to the X and Y. We're going to learn about geoms, which are the 
# shapes of our data, and how we can combine them through layers, themes, and
# facets. 

# For more info: https://ggplot2-book.org/ -- A free resource.

# ---- Libraries ----
vt_packages <- c("tidyverse", "ggplot2", "broom", "readr", "janitor", "viridis", "RColorBrewer", "ggExtra", "hexbin")
install.packages(setdiff(vt_packages, rownames(installed.packages())))

library(ggplot2)
lapply(vt_packages, require, character.only = TRUE)

rm(vt_packages)

# ---- Critical Functions for Lab ----
# Please review and explore! I have helpfully set up each line to call the help
# function!

?ggplot() # If you didn't see this coming...
?aes()
?geom_point # There are LOTS of geoms
?ylim

# Note, there are many elements of GG plots. We've worked with some, and we'll
# review. These are examples.

# ---- Opening Comments ----
# We have already walked through a couple of examples of GGPlots that I've
# shared from work. There are benefits and penalties to using GGplots that
# make them powerful, but expensive. However, the ease of use, and their
# flexibility make them worth the price in my mind.
#
# So what are ggplots doing?
#
# GGplots work by building up layers on layers while keeping each layer distinct
# and able to be interacted with. It is easiest to think of ggplots working
# much like a powerpoint presentation. In fact, let's look at doing that now. At
# this point in the lecture, I moved over to PowerPoint and assembled shapes one
# on top of the other. This is an example of "layering" and is a popular and 
# common approach to building visualizations in the graphic design space. You
# start with a base layer, and you use aesthetics (key word!) to map your X and
# Y axises to your data. You then decide on a shape or geom for your chart
# and assign that, making changes to such things as color, borders, transparency
# and more. 
#
# Base R is more simple, focusing on a "pen on paper" approach where what is 
# underneath can't easily be edited without having to rewrite the whole chart.
#
# Now, the flexibility and power of GGplot does come with a cost. It typically
# keeps the entire data frame buried within plot object. This can be useful in
# certain situations, but has the penalty of increasing memory utilization. Be
# aware that being able to know exactly what you want to utilize is critical to
# successfully managing your memory.
#
# Additionally, GGplots very much prefer working with data frames. You can't
# pass over two vectors and make them play nicely, GGPlot needs those two
# vectors to be properly formatted into columns for mapping to X and Y axises. 
# This prevents GGplots from playing nicely with a number of other graphic
# packages, another thing to be aware of.
#
# You will not need them anyway!

# ---- Getting Started ----
# Let's get our data imported
df_sales <- data_frame(sales)

## ---- Old vs New ----
# We should all be familiar with the sales data at this point. Let's walk
# through how to explore the data! Tell me how to pull out column names, summary
# and other aspects of the DF?

### ---- This Section Graded for Lab ---- 
str(df_sales)
colnames(df_sales)
dim(df_sales)
# [CODE GOES HERE!]

### ---- Let's Create a Histogram of a Variable ----
# Old Way
hist(df_sales$units.sold)

# New way
ggplot(df_sales, aes(x = units.sold)) + geom_histogram()

### ---- Now Let's Look at a Scatter Plot ----
# we've seen this plot a few times!
plot(df_sales$expenses, df_sales$recipt)

# Here is how we do it in GGPlots
ggplot(df_sales, aes(x = expenses, y = recipt)) + geom_point()

# Let's start at the most basic level:
plt_exp_and_income <- ggplot()
plt_exp_and_income

# What are we looking at? A blank plotting area.

# And here is how we can start to build layers to add features
plt_exp_and_income <- df_sales %>%
  ggplot(aes(x = expenses, y = recipt))

plt_exp_and_income

# What do you see? You should see a blank chart area with Income and Expenses.
# This is created by the aes function, or aesthetic mapping. We want to draw
# x and y. Think of aesthetics about what you want to share or show. It is the
# nature of your chart.

plt_exp_and_income <- plt_exp_and_income + geom_point()
plt_exp_and_income

# Now what has happened? We've added a "shape", in this case points.

plt_exp_and_income <- plt_exp_and_income + geom_smooth(method = "lm", se = FALSE)
plt_exp_and_income

# Pretty neat right? We've added a nice little line to provide some more data!

plt_exp_and_income <- plt_exp_and_income + labs(title = "A Tidy Plot!")
plt_exp_and_income

# ---- What are we looking at? ----
# GGplots is designed to build plots up in layers. As we've discussed. It is
# very good at making complex and unique plots driven by your visualization
# needs. Reminder: It has limitations. Let's look at them.
#
#   * takes much more code for simple plots than base R
#   * MUST have a df (can't just plot a vector)
#   * takes over graphic system, so doesn't play well with other packages
#        e.g. can't use par, layout, text, mtext, points, lines....
#   * requires learning a new language (gg) on top of R
#   * tends to use more memory than base plot (stores df in p)

# What do we mean by the last point?

class(plt_exp_and_income)
attributes(plt_exp_and_income)
plt_exp_and_income$data
plt_exp_and_income$layers
plt_exp_and_income$scales
summary(plt_exp_and_income)

# ---- Flexibility is a Principle of GGPlots ----

# Let's look at a plot!
ggplot(df_sales, aes(x = expenses, y = recipt)) +
  geom_point()

# But what if we did it this way?
p <- ggplot(df_sales)
p + aes(x = expenses, y = recipt) + 
  geom_point() + labs(title = "Try two")

# And another way!
ggplot(df_sales) + 
  aes(x = expenses, y = recipt) + 
  geom_point() + labs(title = "All Broken Out")

# ---- Building all kinds of Plot! ----
# now to "set" a color we do this:
ggplot(df_sales) + aes(x = expenses, y = recipt) +
  geom_point(color = "#4050c6")

# Now let's split colors based on type
ggplot(df_sales) + aes(x = expenses, y = recipt, color = type) + 
  geom_point()

# This is going to be a crazy busy plot. Let's build it out step by step, then
# we can save and this will be a part of what you merge.

ggplot(df_sales) + aes(x = expenses, 
                       y = recipt, 
                       size = units.sold, 
                       shape = type, 
                       alpha = rep.region, 
                       color = year) + 
  geom_point()

# We can pass logical tests over too!

p <- ggplot(df_sales) + aes(x = expenses, 
                         y = recipt, 
                         color = unit.price > 14) + 
  geom_point()
p

# What is this chart showing us?

p + scale_colour_manual(values = c("red", "purple"))
p + geom_rug()
# What happened here? Why did we lose the red/purple coloration?

# Let's build a smooth another way!
income.pred <- predict(lm(df_sales$recipt~df_sales$expenses))

ggplot(df_sales) + aes(y = recipt, x = expenses) + 
  geom_point() +
  geom_line(aes(y = income.pred)
            , color = "red", lwd = 3)

# So geom_smooth will absolutely work with your X and Y, but chances are your lm
# object might have a lot of other factors included. 

ggplot(df_sales) + aes(y = recipt, x = expenses) + 
  geom_point() +
  geom_line(aes(y = income.pred)
            , color = "white", lwd = 5) +
  geom_smooth(method = "lm", se = FALSE)

# Both lines are the same!

# Let's keep going
ggplot(df_sales) + 
  aes(y = recipt, x = expenses) + 
  geom_point() + 
  geom_smooth()

# note the default smooth line here is intended to smooth out the cluster of
# points. It is not going to show you a lm methodology by default. How do you
# find out your options?

# Here is another way of looking at this data:

p <- ggplot(df_sales, aes(x = expenses, y = recipt)) +
  geom_hex() +
  theme_bw()

p

# Now let's talk a bit about scaled fills.
library(ggplot2)
p + scale_fill_viridis()
p + scale_fill_viridis(option = "cividis")
p + scale_fill_viridis(option = "plasma")
p + scale_fill_viridis(option = "inferno")
display.brewer.all()
p + scale_fill_gradientn(colors = brewer.pal(3,"Greens"))
p + scale_fill_gradientn(colors = brewer.pal(6,"YlOrRd"))
p + scale_fill_gradientn(colors = brewer.pal(3,"Set3"))

# ---- Putting it all Together ----
## ---- SAVE THIS CHART! ----
library(RColorBrewer)
plt_exp_income_hex <- ggplot(df_sales, aes(x = expenses, y = recipt)) + 
  geom_point(alpha = 0) +
  geom_hex() +
  scale_fill_gradientn(colors = rev(brewer.pal(6,"Greens"))) +
  theme_classic()

plt_exp_income_hex

# ---- Pulling in Library GGExtra ----
library(dplyr)
df_sales %>%
  filter( unit.price>10 ) %>%
  ggplot( aes(x=price)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

# Let's add distribution data to our plot
library(ggExtra)
ggMarginal(plt_exp_income_hex, type = "histogram", fill = brewer.pal(6,"Greens")[5], color = "white")

# Let's create a new column of data!
df_sales$price <- ifelse(df_sales$unit.price > 14
                , "expensive", "moderate")
df_sales$price[df_sales$unit.price < 9] <- "cheep"

ggplot(df_sales) + 
  aes(y = recipt, x = expenses, color = price) + 
  geom_bin2d(bins = 50)

ggplot(df_sales) + 
  aes(x = expenses, y = recipt) + 
  geom_point() + 
  facet_grid(wine ~ .)

ggplot(df_sales) + 
  aes(x = expenses, y = recipt) + 
  geom_point() + 
  facet_grid(. ~ wine)

ggplot(sales) + 
  aes(x = expenses, y = recipt, color = rep.region) + 
  geom_point() + 
  facet_grid(wine ~ rep.region)

ggplot(sales) + 
  aes(x = recipt) + 
  geom_histogram()

ggplot(sales) +
  aes(x = recipt, fill = wine) +
  geom_histogram(binwidth = 10, alpha = .5) +
  geom_density(alpha=.95, fill="#FF6666") +
  theme_minimal()


ggplot(sales) + 
  aes(x = rep.region, y = recipt) + 
  geom_boxplot()

# talk about difference between "color" and "fill"
ggplot(sales) +
  aes(x = rep.region, y = recipt
      , fill = rep.region
      , color = rep.region) +
  geom_violin() + 
  stat_summary(fun=mean, geom="point", shape=8, size=4, col = "black") +
  scale_color_brewer(palette="Dark2") +
  scale_fill_brewer(palette="Dark2") +
  theme_minimal() +
  theme(legend.position="bottom") +
  ggtitle(label = "Regional Sales", subtitle = "Some story text")


df.year <- aggregate(sales$units.sold
                     , list(year = sales$year)
                     , sum)
ggplot(df.year) + 
  aes(x = year, y = x) + 
  geom_line() + 
  ylim(c(0, 40000))


df2 <- aggregate(sales$units.sold
                 , list(year = sales$year, region = sales$rep.region)
                 , sum)
df2
ggplot(df2) + 
  aes(x = year, y = x, color = region) + 
  geom_line() + 
  ylim(c(0, 10000))

world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")
state<-map_data("state") 


ggplot(sales) + 
  aes(x = rep.region) + 
  geom_bar(fill = "orange", width = .5) +
  ggtitle("Number of sales by region")

# default is stacked
ggplot(sales) + aes(x = rep.region, fill = type) + 
  geom_bar()

ggplot(sales) + aes(x = rep.region, fill = type) + 
  geom_bar(position = "dodge")


reg.by.type <- aggregate(sales$units.sold
                         , list(region = sales$rep.region
                                , type = sales$type)
                         , sum)
colnames(reg.by.type)[3] <- "sales"

ggplot(reg.by.type) +
  aes(x = region, y = sales, fill = type) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#B03060", "#FFEC8B")) +
  theme_classic() +
  ggtitle("Setting identity: bars = units sold")





df <- aggregate(sales$units.sold
                , list(region = sales$rep.region)
                , sum)
colnames(df)[2] <- "sales"

# our data is already aggragated so we
# MUST use identity
ggplot(df) + 
  aes(x = region, y = sales, fill = region) + 
  geom_bar(stat = "identity")

# and so here is how you make a pie chart
ggplot(df) + 
  aes(x = "", y = sales, fill = region) +
  geom_bar(width = .3, stat = "identity") +
  coord_polar("y", start = 45)

o <- order(df$sales, decreasing = FALSE)
df <- df[o,]
df$region <- factor(df$region, levels = df$region)

my.pal.f <- colorRampPalette(c("skyblue", "darkslateblue"))

ggplot(df) +
  aes(x = region, y = sales, fill = region) +
  geom_bar(width = 0.95, stat = "identity") +
  coord_polar(theta = "y") +
  ylim(c(0, 47000)) +
  xlab("") + ylab("") +
  geom_text(data = df, hjust = 1.1, size = 7, 
            aes(x = region, y = 0, label = region)) +
  theme(legend.position = "none"
        , axis.text.y = element_blank()
        , axis.ticks = element_blank()
        , panel.background = element_blank()) +
  scale_fill_manual(values = my.pal.f(5)) +
  ggtitle("Curved Bar Plot: useful?")