#downloading Required Packages 
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
library(dplyr)
#Understanding the dataset 
beans_df<- read.csv(file = file.choose(), header = TRUE, stringsAsFactors = FALSE)
str(beans_df)
summary(beans_df)
str(beans_df)

# Cleaning the data
beans_df$date <- as.Date(beans_df$date)
colSums(is.na(beans_df))

#Which Employees sell the most 
rep_sales <- beans_df %>%
  group_by(rep) %>%
  summarize(total_units = sum(units.sold), .groups = "drop") %>%
  arrange(desc(total_units)) %>%
  slice_head(n = 10)

#plot
ggplot(rep_sales, aes(x = reorder(rep, total_units), y = total_units)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Sales Reps by Units Sold",
    x = "Sales Rep",
    y = "Total Units Sold",
    caption = "Data grouped by rep and filtered to the top 10 by total units sold (2019–2021)"
  ) +
  theme_minimal()

#Average Profit by Region
region_profit <- beans_df %>%
group_by(region) %>%
  summarize(avg_profit = mean(sale.profit), .groups = "drop")
# plot2 
ggplot(region_profit, aes(x = reorder(region, avg_profit), y = avg_profit)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Average Profit by Region",
       x = "Region",
       y = "Average Profit",
       caption = "Data grouped by region and averaged using sale.profit") +
  theme_minimal()

#sales vs profit by rep 
rep_summary_top10 <- beans_df %>%
  group_by(rep) %>%
  summarize(
    units_sold = sum(units.sold),
    total_profit = sum(sale.profit),
    .groups = "drop"
  ) %>%
  arrange(desc(units_sold)) %>%
  slice_head(n = 10)

#plot 3 
ggplot(rep_summary_top10, aes(x = units_sold, y = total_profit, label = rep)) +
  geom_point(size = 4, color = "#1b9e77") +
  geom_text(vjust = 1.5, hjust = 1.1, size = 3.5, check_overlap = FALSE) +  # better position
  labs(
    title = "Top 10 Sales Reps: Units Sold vs. Profit",
    x = "Total Units Sold",
    y = "Total Profit",
    caption = "Top 10 reps by units sold; total units and profit aggregated per person"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 9)
  )

#Product Type Trends Over Time

product_trends <- beans_df %>%
  group_by(date, type) %>%
  summarize(units = sum(units.sold), .groups = "drop")
#plot4
ggplot(product_trends, aes(x = as.Date(date), y = units, color = type)) +
  geom_line(size = 1) +
  labs(title = "Units Sold Over Time by Product Type",
       x = "Date",
       y = "Units Sold",
       color = "Product Type",
       caption = "Grouped by product type and date; total units sold") +
  theme_minimal()


