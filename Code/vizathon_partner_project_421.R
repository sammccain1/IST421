#downloading Required Packages 
library(tidyverse)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(maps)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(scales)
library(forcats)

#Understanding the dataset 
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

ggplot(region_profit, aes(x = reorder(region, avg_profit), y = avg_profit, fill = avg_profit)) +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "steelblue", name = "Avg Profit") +
  labs(
    title = "Average Profit by Region",
    x = "Region",
    y = "Average Profit",
    caption = "Data grouped by region and averaged using sale.profit"
  ) +
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


state_profit <- beans_df %>%
  mutate(state = tolower(state)) %>%
  group_by(state) %>%
  summarize(avg_profit = mean(sale.profit), .groups = "drop")

us_states <- map_data("state")

map_df <- left_join(us_states, state_profit, by = c("region" = "state"))

ggplot(map_df, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = avg_profit), color = "white") +
  scale_fill_gradient(
    low = "#a8e6cf",  
    high = "#374785",  
    na.value = "gray90"
  ) +
  coord_fixed(1.3) +
  labs(
    title = "Average Profit by State (2019–2021)",
    fill = "Avg Profit",
    caption = "Custom hex colors: mint green (low) to indigo (high)"
  ) +
  theme_minimal()


state_sales <- beans_df %>% group_by(state) %>%
  summarise(total_units = sum(units.sold, na.rm = TRUE))

map_df <- left_join(us_states, state_sales, by = c("region" = "state"))

ggplot(map_df, aes(x = long, y= lat, group = group)) +
  geom_polygon(aes(fill = total_units), color = "white") +
  scale_fill_gradient(
    low = "#a8e6cf",
    high = "#374785",
    na.value = "gray90"
  ) +
  coord_fixed(1.3) +
  labs(title = "Total Units Sold by State (2019–2021)",
       fill = "Units Sold",
       caption = "Custom hex colors: mint green (low) to indigo (high)"
  ) +
  theme_minimal()
    

south_dakota_sales_df <- beans_df %>% filter(state == "south dakota")
oklahoma_sales_df <- beans_df %>% filter(state == "oklahoma")
colorado_sales_df <- beans_df %>% filter(state == "colorado")
new_york_sales_df <- beans_df %>% filter(state == "new york")
pennsylvania_sales_df <- beans_df %>% filter(state == "pennsylvania")

state_sales <- state_sales %>% arrange(desc(total_units))
state_profit <- state_profit %>% arrange(desc(avg_profit))

south_dakota_sales_df %>%
  filter(type == "coffee") %>%
  summarise(mean_value = mean(sale.profit))

south_dakota_sales_df %>%
  filter(type == "tea") %>%
  summarise(mean_value = mean(sale.profit))

new_york_sales_df %>%
  filter(type == "coffee") %>%
  summarise(mean_value = mean(sale.profit))

new_york_sales_df %>%
  filter(type == "tea") %>%
  summarise(mean_value = mean(sale.profit))

south_dakota_sales_percent <- south_dakota_sales_df %>%
  rowwise() %>%
  mutate(
    total = nrow(south_dakota_sales_df),
    tea_pct = sum(south_dakota_sales_df$type == "tea") / total * 100,
    coffee_pct = sum(south_dakota_sales_df$type == "coffee") / total * 100
  ) %>%
  select(state, tea_pct, coffee_pct)

new_york_sales_percent <- new_york_sales_df %>%
  rowwise() %>%
  mutate(
    total = nrow(new_york_sales_df),
    tea_pct = sum(new_york_sales_df$type == "tea") / total * 100,
    coffee_pct = sum(new_york_sales_df$type == "coffee") / total * 100
  ) %>%
  select(state, tea_pct, coffee_pct)

colorado_sales_percent <- colorado_sales_df %>%
  rowwise() %>%
  mutate(
    total = nrow(colorado_sales_df),
    tea_pct = sum(colorado_sales_df$type == "tea") / total * 100,
    coffee_pct = sum(colorado_sales_df$type == "coffee") / total * 100
  ) %>%
  select(state, tea_pct, coffee_pct)

pennsylvania_sales_percent <- pennsylvania_sales_df %>%
  rowwise() %>%
  mutate(
    total = nrow(pennsylvania_sales_df),
    tea_pct = sum(pennsylvania_sales_df$type == "tea") / total * 100,
    coffee_pct = sum(pennsylvania_sales_df$type == "coffee") / total * 100
  ) %>%
  select(state, tea_pct, coffee_pct)

oklahoma_sales_percent <- oklahoma_sales_df %>%
  rowwise() %>%
  mutate(
    total = nrow(oklahoma_sales_df),
    tea_pct = sum(oklahoma_sales_df$type == "tea") / total * 100,
    coffee_pct = sum(oklahoma_sales_df$type == "coffee") / total * 100
  ) %>%
  select(state, tea_pct, coffee_pct)

state_sales_percent <- oklahoma_sales_percent[1,]
state_sales_percent <- rbind(state_sales_percent, south_dakota_sales_percent[2,])
state_sales_percent <- rbind(state_sales_percent, pennsylvania_sales_percent[3,])
state_sales_percent <- rbind(state_sales_percent, new_york_sales_percent[4,])
state_sales_percent <- rbind(state_sales_percent, colorado_sales_percent[5,])

state_sales_long <- state_sales_percent %>%
  pivot_longer(cols = c(tea_pct, coffee_pct),
               names_to = "drink",
               values_to = "percent")



# Create gradient (light to dark blue)
blue_gradient<- colorRampPalette(c("#deebf7", "#08519c"))


# Map each value to a color
mapped_colors <- colors[ceiling(scaled_values * 99) + 1]  # +1 to avoid index 0

# Result: each value now has a color between light and dark blue
data.frame(values, scaled_values, mapped_colors)

ggplot(state_sales_long, aes(x = drink, y = fct_rev(state), fill = percent)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), color = "black", size = 4) +
  scale_fill_gradient2(low = "#deebf7",      # light blue
    mid = "#4292c6",      # medium blue
    high = "#08519c",     
    midpoint = 50,      
    limits = c(38, 62), 
    oob = scales::squish,
  labels = function(x) sprintf("%.0f%%", x * 100)) +
  labs(
    title = "Tea vs Coffee Sales (%) by State",
    x = "Drink",
    y = "State",
    fill = "Percentage"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

colorado_sales_percent <- colorado_sales_df %>%
  rowwise() %>%
  mutate(
    total = nrow(colorado_sales_df),
    tea_pct = sum(colorado_sales_df$type == "tea") / total * 100,
    coffee_pct = sum(colorado_sales_df$type == "coffee") / total * 100
  ) %>%
  select(state, tea_pct, coffee_pct)

new_jersey_sales_df <- beans_df %>% filter(state == "new jersey")
massachusetts_sales_df <- beans_df %>% filter(state == "massachusetts")
connecticut_sales_df <- beans_df %>% filter(state == "connecticut")

new_jersey_sales_percent <- new_jersey_sales_df %>%
  rowwise() %>%
  mutate(
    total = nrow(new_jersey_sales_df),
    tea_pct = sum(new_jersey_sales_df$type == "tea") / total * 100,
    coffee_pct = sum(new_jersey_sales_df$type == "coffee") / total * 100
  ) %>%
  select(state, tea_pct, coffee_pct)

massachusetts_sales_percent <- massachusetts_sales_df %>%
  rowwise() %>%
  mutate(
    total = nrow(massachusetts_sales_df),
    tea_pct = sum(massachusetts_sales_df$type == "tea") / total * 100,
    coffee_pct = sum(massachusetts_sales_df$type == "coffee") / total * 100
  ) %>%
  select(state, tea_pct, coffee_pct)

connecticut_sales_percent <- connecticut_sales_df %>%
  rowwise() %>%
  mutate(
    total = nrow(connecticut_sales_df),
    tea_pct = sum(connecticut_sales_df$type == "tea") / total * 100,
    coffee_pct = sum(connecticut_sales_df$type == "coffee") / total * 100
  ) %>%
  select(state, tea_pct, coffee_pct)

low_profit_states <- new_jersey_sales_percent[1,]
low_profit_states <- rbind(low_profit_states, massachusetts_sales_percent[2,])
low_profit_states <- rbind(low_profit_states, connecticut_sales_percent[3,])

low_profit_states_long <- low_profit_states %>%
  pivot_longer(cols = c(tea_pct, coffee_pct),
               names_to = "drink",
               values_to = "percent")

ggplot(low_profit_states_long, aes(x = drink, y = fct_rev(state), fill = percent)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), color = "black", size = 4) +
  scale_fill_gradient2(
    low = "#deebf7",      # light blue
    mid = "#4292c6",      # medium blue
    high = "#08519c",     
    midpoint = 50,      
    limits = c(38, 62), 
    oob = scales::squish,
    labels = function(x) sprintf("%.0f%%", x * 100)
  ) +
  labs(
    title = "Tea vs Coffee Sales (%) by State",
    x = "Drink",
    y = "State",
    fill = "Percentage"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())
