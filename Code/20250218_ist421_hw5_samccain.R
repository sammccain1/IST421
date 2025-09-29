#loading packages

library(ggplot2)
library(dplyr)
library(ggthemes)

#fig 4-5
hotdogs <-
  read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv",
           sep=",", header=TRUE)
fill_colors <- c()
for ( i in 1:length(hotdogs$Country) ) {
  if (hotdogs$Country[i] == "United States") {
    fill_colors <- c(fill_colors, "#821122")
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}
barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year, col=fill_colors,
        border=NA, xlab="Year", ylab="Hot dogs and buns (HDB) eaten")
barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year, col=fill_colors,
        border=NA, space=0.3, xlab="Year", ylab="Hot dogs and buns (HDB) 
eaten")

#4-21 stacked bar chart
hot_dog_places <-
  read.csv("http://datasets.flowingdata.com/hot-dog-places.csv",
            sep=",", header=TRUE)
names(hot_dog_places) <- c("2000", "2001", "2002", "2003", "2004",
                           "2005", "2006", "2007", "2008", "2009", "2010")
hot_dog_matrix <- as.matrix(hot_dog_places)
barplot(hot_dog_matrix, border=NA, space=0.25, ylim=c(0, 200),
        xlab="Year", ylab="Hot dogs and buns (HDBs) eaten",
        main="Hot Dog Eating Contest Results, 1980-2010")

#4-25 scatter plot
subscribers <-
  read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv",
           sep=",", header=TRUE)
plot(subscribers$Subscribers)
plot(subscribers$Subscribers, type="p", ylim=c(0, 30000))


#fig 4-40: time series

#fig 4-42
postage<-read.csv("http://datasets.flowingdata.com/us-postage.csv",sep=",",header=TRUE)
plot(postage$Year,postage$Price,type="s")
plot(postage$Year, postage$Price, type="s",
     main="US Postage Rates for Letters, First Ounce, 1991-2010",
     xlab="Year", ylab="Postage Rate (Dollars)")

#fig 4-47
unemployment<-
  read.csv(
    "http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv",
    sep=",")

scatter.smooth(x=1:length(unemployment$Value),
               y=unemployment$Value,ylim=c(0,11),degree=2,col="#CCCCCC",span=0.5)