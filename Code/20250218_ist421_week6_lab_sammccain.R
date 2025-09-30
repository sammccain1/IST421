# ---- Week 6 Lab for Students ----
# Authors: Jason Reilly & Jeff Hemsley

# ---- Overview of Lab ----
# 1) Color Brewer
# 2) Working with colors and plotting
# 3) Color theory within graphs
# 4) Playing with pngs and including them in plots!

# This week we will be discussing color and color theory. This specific topic is
# one of the reasons we have wider discussions around "liberal arts education"
# as a lot of the principles we discuss here do not come from data science, but
# come to us from the arts. A lot of the critical decision making that goes
# visual design and brand colors can be learned at VPA, and I strongly encourage
# students to take at least one or two basic art classes as it will serve you
# well throughout your career.

# This week is about making color choices that make sense. We'll also look at
# how we can use other elements to make compelling visuals.

# ---- RGB vs RYB ---
# You'll see a lot of references to RGB as hexidecimal codes in this class. Why?
# In primary school you probably learned about RYB - Red, Yellow, Blue - and how
# they are primary colors you can mix. This is true for pigments as they are 
# mixed in real mediums. When you're mixing paints or pigments, you need to
# think through reflection. With RGB you're thinking about the pixels in your
# monitor or traditional computer displays (which are red, green, blue) and how 
# much light you need to emit from that display.

# ---- Okay why CMYK? ----
# CMYK - Cyan, Magenta, Yellow, blacK - is for industrial printing. We shouldn't
# have to deal with this in this class, but you should be aware that this is
# something you could deal with depending on your work. What you see with RGB
# on your screen might not match what you get delivered to you by a print shop
# if they're working with certain hardware. Many print shops will have people
# who can fine tune your RGB references to CMYK, and Illustrator and Inkscape
# both include CMYK codes.

# ---- Libraries ----
library(RColorBrewer)
library(png)
library(readr)
library(tidyverse)
library(ggplot2)

# ---- Critical Functions ----
?display.brewer.all()
?colorRampPalette()

## ---- Fun Facts about Color ----
vt_base_colors <- c("#ffffff", "#000000", "#0000FF", "#FF0000", "#00FF00")

# Hexidecimal works on a series of codes from 0 to F, giving you 15 values you
# can store in a single memory location. This allows for a max value of 225
# represented by the two positions. 

# This is a form of assembly: FF is the max value for the location, and zero is
# the min value. So FFFFFF is white, 000000 is black, and the use of FF in any
# location maximizes the color value in that location

# You can read this as:
# [RR][GG][BB]

# Each double set allows you to control the amount of voltage going to that
# particular set of pixels in the screen for that object.

# Here is an example of a monitor diode: 
# https://mcuoneclipse.com/wp-content/uploads/2014/07/ws2812-led-with-red-green-and-blue.png

# Learn more about how computers work with RGB and how the calculations work:
# https://www.rapidtables.com/web/color/RGB_Color.html

# ---- Import Data ----
df_sales <- sales

# We've used this data set a few times, and we'll keep on using it! 

# ---- Working with ColorBrewer ----
display.brewer.all()
# Let's take a look at our options for display.brewer.all()
# There are a lot of color sets here that run through a lot of the yellow/red.
# We can use this to automatically fill our plots:

# Let's just create some random data.
df_rand_data <- replicate(16,rnorm(35,35,sd=1.5))

# And now, create boxplots using some of the titles you see from the above line
boxplot(df_rand_data,col=brewer.pal(8,"Accent"))

# Put names into quotes!

# ---- Let's make our own ----
vt_num_colors <- 16 
# First we need to make a vector to outline how many colors we want.
# we can start with eight.

fn_color_boundaries <- colorRampPalette(c("blue", "red"))
# This creates a function for us that basically will allow us to use these two 
# primary colors.

vt_my_cols <- fn_color_boundaries(vt_num_colors)
# Notice that the function gives us a list of hexidecimal codes between the two
# extremes of blue and red - and note that that they are 0000FF and FF0000, why
# do you think that is?

# So what does it look like?
boxplot(df_rand_data,col=vt_my_cols)

# ---- TURN IN THE NEXT PLOT ----
# Okay repeat the above, but choose two colors of your own! Put them into a
# boxplot, and then replace the numbers on the axis with the hexidecimal codes!

df_sam_rand_data <- replicate(10,rnorm(35,35,sd=1.5))
vt_num_colors_final <- 10
my_color_boundaries <- colorRampPalette(c("#33ff49","#ffaf33"))
vt_sam_colors <- my_color_boundaries(vt_num_colors_final)
boxplot(df_sam_rand_data, col=vt_sam_colors, names = vt_sam_colors, main = "Lab #6 Plot1", xlab= "color code")


# ---- Using Color Constructively ----

# remember this plot?
plot(df_sales$expenses, df_sales$recipt, pch=16, cex=1)


# So let's quickly go through and quickly assign colors through the use of a 
# vector. In reality we probably would never do this, but it allows us to 
# demonstrate how we can call and use rgb color values and assign them to 
# categorical vectors within the data set.

# Let's set a color
vt_col <- rep(rgb(30, 144, 255, maxColorValue = 255), dim(df_sales)[1])

# Now let's make sure red gets it's own color!
vt_col[df_sales$type == "red"] <- rgb(255, 64, 64, maxColorValue = 255)

# What happened?
plot(df_sales$expenses, df_sales$recipt, pch=16, cex=1, col = vt_col)

# ---- SAVE THE NEXT PLOT FOR TURNING IN ----
# Notice here we can even set up a logical check. What we are doing here is 
# using the same principle above - logical checks to change a color after
# setting the overall color - so that we can show the difference in data points

col.vec <- rep(rgb(30, 144, 255, maxColorValue = 255), dim(df_sales)[1])
col.vec[df_sales$unit.price > 14] <- rgb(255, 64, 64, maxColorValue = 255)
plot(df_sales$expenses, df_sales$recipt, pch=16, cex=1, col = col.vec)

# You can do something similar with GGplot

df_sales_ggplot <- df_sales %>% mutate(color_break = ifelse(unit.price > 14, "H", "L"))

cols <- c("H" = "darkgreen", "L" = "lightgrey")

sales_plot <- df_sales_ggplot %>%
  ggplot(aes(x = expenses, y = recipt, color = color_break)) +
  geom_point() +
  scale_color_manual(values = cols) +
  labs(title = "Scatter Plot with Conditional Coloring",
       x = "",
       y = "Y-axis Label",
       color = "Condition") +
  theme_minimal()

sales_plot


# ---- Overplotting & Transparency ----
# When we plot, we can sometimes run into situations where we are plotting and
# we are going to have difficulty with colors overlapping (see above)

# Let's start again.
plot(df_sales$expenses, df_sales$recipt, pch=16, cex=1.5)

# let's set our colors and plot our data.
over.plotting.cols <- rgb(.8, .15, .15)
plot(df_sales$expenses, df_sales$recipt, pch=16, cex=1.5
     , col = over.plotting.cols)

# Let's use "Alpha" to show transparency
over.plotting.cols <- rgb(.8, .15, .15, alpha = .3)
plot(df_sales$expenses, df_sales$recipt, pch=16, cex=1.5
     , col = over.plotting.cols)

## ---- Critical Notes ----
# Alpha is how you set your transparency
# The RGB function takes values between 0 and 1, unless you use the 
# maxColorValue parameter, which then converts to a numeric.

over.plotting.cols <- rgb(24, 116, 205, alpha = 75, maxColorValue = 255)
plot(df_sales$expenses, df_sales$recipt, pch=16, cex=.5
     , col = over.plotting.cols)

# ---- Building the Wine Plot ---- 
# Now we're going to build the plot from the slide deck.

# Let's start with a basic bar plot. This isn't ordered or anything.
barplot(table(df_sales$wine))

# Not particularly useful or instructive either.

# Reminder:
# Aggregate and tapply are different. Aggregate returns a dataframe
# where tapply returns a matrix. Here we want a df, so we use aggregate

# Base R
df_agg_data <- aggregate(df_sales$units.sold
                      , by = list(type = df_sales$type, wine = df_sales$wine)
                      , FUN=sum)

# Tidyverse
df_tidy_agg_data <- df_sales %>% 
  group_by(type, wine) %>% 
  summarise(sum_units = sum(units.sold)) %>%
  ungroup()

# New Plot
barplot(df_tidy_agg_data$sum_units, names.arg = df_tidy_agg_data$wine)

# Still not that useful but you can see we have our data.

# Now let's select wine colors using RGB functions
wine.colors <- c(rgb(255, 240, 150, maxColorValue = 255)
                 , rgb(160, 30, 65, maxColorValue = 255))

# THIS IS ONLY SO YOU CAN SEE THE COLORS!! 
pie(c(10,10), col = wine.colors)


# here we are going to 
bar.colors <- rep("white", nrow(df_tidy_agg_data))
bar.colors[df_tidy_agg_data$type == "white"] <- wine.colors[1]
bar.colors[df_tidy_agg_data$type == "red"] <- wine.colors[2]

# NOTE: The use of white/red here is from the names within the data set, not 
# colors we're trying to use.

barplot(df_tidy_agg_data$sum_units, names.arg = df_tidy_agg_data$type, col=bar.colors)
barplot(df_tidy_agg_data$sum_units, names.arg = df_tidy_agg_data$wine
        , col=bar.colors, border = NA
        , main="units sold")

# Now you can see that the layering of color has provided some interesting
# dimension for us. WE can see red vs white a lot more clearly. 

# Let's look at income.

df_income_data <- aggregate(df_sales$recipt
                              , by = list(type = df_sales$type, wine = df_sales$wine)
                              , FUN=sum)

# We're merging data together very losey goosy. This is usually best done as a
# left join or something similar.

colnames(df_income_data) <- c("type", "wine", "income")
df_agg_data$income <- df_income_data$income

# Usually this is at the top
options(scipen = 999)

ima <- readPNG("bottles.png")
r1 <- readPNG("R1.png")
w1 <- readPNG("w1.png")

pch <- rep("W", 7)
pch[df_agg_data$type == "red"] <- "R"

plot(df_agg_data$x, df_agg_data$income, bty = "n"
     , pch = 15, cex = 2, col = bar.colors
     , xlim = c(0, 1.25 * max(df_agg_data$x))
     , ylim = c(0, 1.25 * max(df_agg_data$income))
     , xlab = "Units sold", ylab = "Recipts"
     , main = "IST421 Simulated Wine Sales Dataset"
)  

# Isn't that nice?

# Let's keep going
lim <- par()
rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
rect(lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4], col = rgb(1,1,1,.85), border = "white")

r1.x1 <- df_agg_data$x[df_agg_data$type == "red"]
r1.x2 <- r1.x1 + 3000
r1.y1 <- df_agg_data$income[df_agg_data$type == "red"]
r1.y2 <- r1.y1 + 65000

rasterImage(r1, r1.x1, r1.y1, r1.x2, r1.y2)


w1.x1 <- df_agg_data$x[df_agg_data$type == "white"]
w1.x2 <- w1.x1 +3000
w1.y1 <- df_agg_data$income[df_agg_data$type == "white"]
w1.y2 <- w1.y1 + 65000

rasterImage(w1, w1.x1, w1.y1, w1.x2, w1.y2)

text(df_agg_data$units + 2000, df_agg_data$income, labels = df_agg_data$wine, adj=0, cex = 1.2)
