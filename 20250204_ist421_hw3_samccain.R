# Load necessary libraries
library(ggplot2)
library(dplyr)

# Loaded in CSV file to environment from computer and now converting it into a dataframe.
art_df <- data.frame(art)

# Set up a 2x2 plotting space
par(mfrow = c(2, 2))

# 1. Relationship between unit price and units sold
plot(art_df$unit.price, art_df$units.sold, 
     main="Unit Price vs. Units Sold", 
     xlab="Unit Price ($)", ylab="Units Sold", 
     col="blue", pch=16)

# 2. Comparing total units sold: Drawing vs. Watercolor
paper_sales <- aggregate(units.sold ~ paper, data = art_df, sum)
barplot(paper_sales$units.sold, names.arg = paper_sales$paper, 
        main="Total Units Sold: Drawing vs. Watercolor", 
        xlab="Paper Type", ylab="Units Sold", 
        col=c("red", "green"))

# 3. Drawing vs. Watercolor Revenue Comparisons
revenue_sales <- aggregate(total.sale ~ paper, data = art_df, sum)
barplot(revenue_sales$total.sale, names.arg = revenue_sales$paper, 
        main="Total Revenue: Drawing vs. Watercolor", 
        xlab="Paper Type", ylab="Total Revenue ($)", 
        col=c("orange", "purple"))

# 4. Drawing paper subtypes comparison
drawing_paper <- subset(art_df, paper == "drawing") %>% 
  group_by(paper.type) %>% 
  summarise(units.sold = sum(units.sold))
barplot(drawing_paper$units.sold, names.arg = drawing_paper$paper.type, 
        main="Units Sold by Drawing Paper Subtype", 
        xlab="Subtype", ylab="Units Sold", col="blue", las=2)


# 5. Revenue over time comparison
yearly_revenue <- aggregate(total.sale ~ year + paper, data = art_df, sum)
ggplot(yearly_revenue, aes(x=year, y=total.sale, color=paper, group=paper)) +
  geom_line() + geom_point() +
  labs(title="Yearly Revenue: Drawing vs. Watercolor", x="Year", y="Total Revenue ($)") +
  theme_minimal()
