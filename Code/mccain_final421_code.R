#imported dataset from computer
base_mm_dataset <- DEV_March_Madness
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(randomForest)
library(patchwork)
final_mm_dataset <- base_mm_dataset %>% filter(`Post-Season Tournament` == "March Madness")

mm_years <- rep(setdiff(2002:2024, 2020), each =8)
elite_eight_teams_df <- data.frame4(
  season = mm_years,
  team = c("Indiana", "Kent State", "Missouri", "Oklahoma", "Maryland", "UConn", "Kansas", "Oregon", 
           "Kentucky", "Marquette", "Arizona", "Kansas", "Michigan State", "Texas", "Oklahoma", "Syracuse", 
           "UConn", "Alabama", "Duke", "Xavier", "Oklahoma State", "Saint Joseph's", "Georgia Tech", "Kansas",
           "Kentucky", "Michigan State", "Wisconsin", "North Carolina", "West Virginia", "Louisville", "Arizona", "Illinois",
           "Florida", "Villanova", "George Mason", "UConn", "UCLA", "Memphis", "Texas", "LSU",
           "Memphis", "Ohio State", "Georgetown", "North Carolina", "UCLA", "Kansas", "Oregon", "Florida",
           "Xavier", "UCLA", "Texas", "Memphis", "Davidson", "Kansas", "Louisville", "North Carolina",
           "Oklahoma", "North Carolina", "Villanova", "Pittsburgh", "Missouri", "UConn", "Michigan State", "Louisville",
           "Baylor", "Duke", "West Virginia", "Kentucky", "Kansas State", "Butler", "Tennessee", "Michigan State",
           "UConn", "Arizona", "North Carolina", "Kentucky", "Kansas", "VCU", "Butler", "Florida",
           "Kentucky", "Baylor", "Louisville", "Florida", "Syracuse", "Ohio State", "North Carolina", "Kansas",
           "Louisville", "Duke", "Wichita State", "Ohio State", "Michigan", "Florida", "Syracuse", "Marquette",
           "Florida", "Dayton", "Michigan State", "UConn", "Arizona", "Wisconsin", "Kentucky", "Michigan",
           "Kentucky", "Notre Dame", "Wisconsin", "Arizona", "Louisville", "Michigan State", "Duke", "Gonzaga",
           "Kansas", "Villanova", "Oregon", "Oklahoma", "North Carolina", "Notre Dame", "Virginia", "Syracuse",
           "Florida", "South Carolina", "Gonzaga", "Xavier", "Kansas", "Oregon", "North Carolina", "Kentucky",
           "Kansas State", "Loyola Chicago", "Florida State", "Michigan", "Villanova", "Texas Tech", "Duke", "Kansas",
           "Duke", "Michigan State", "Gonzaga", "Texas Tech", "Virginia", "Purdue", "Auburn", "Kentucky",
           "Gonzaga", "USC", "Michigan", "UCLA", "Baylor", "Arkansas", "Oregon State", "Houston",
           "Arkansas", "Duke", "North Carolina", "Saint Peter's", "Houston", "Villanova", "Kansas", "Miami",
           "San Diego State", "Creighton", "Florida Atlantic", "Kansas State", "Miami", "Texas", "UConn", "Gonzaga",
           "UConn", "Illinois", "Alabama", "Clemson", "Duke", "NC State", "Purdue", "Tennessee")
)

final_mm_dataset <- final_mm_dataset %>% select(-elite_eight_teams_df)

colnames(final_mm_dataset) <- make.names(colnames(final_mm_dataset))
colnames(elite_eight_teams_df) <- make.names(colnames(elite_eight_teams_df))

# Add a binary column using left_join() for exact matches
final_mm_dataset <- final_mm_dataset %>%
  left_join(elite_eight_teams_df %>% mutate(elite_8 = 1), 
            by = c("Season" = "season", "Mapped.ESPN.Team.Name" = "team")) %>%
  mutate(elite_8 = if_else(is.na(elite_8), 0, elite_8))

final_mm_dataset <- final_mm_dataset %>%
  select(where(~ !any(is.na(.))))

elite_8_df <- final_mm_dataset %>% filter (elite_8 == 1)

mm_teams_2025 <- final_mm_dataset %>% filter (Season == "2025")

mm_teams_2025_adj <- mm_teams_2025 %>%
  select(all_of(names(final_mm_dataset)))

de_rank <- elite_8_df$Adjusted.Defensive.Efficiency.Rank < 40
oe_rank <- elite_8_df$Adjusted.Offensive.Efficiency.Rank < 40

# Decision Tree Model 
final_mm_dataset$elite_8 <- as.factor(final_mm_dataset$elite_8)

rf_model <- randomForest(
  elite_8 ~ ., 
  data = subset(final_mm_dataset, select = -c(Final.Four., Tournament.Championship), 
  ntree = 500, 
  importance = TRUE
)
importance(rf_model)
varImpPlot(rf_model)

#Offensive + Defensive Efficiency Plot
ggplot() +
  # Background (non-Elite 8 teams)
  geom_point(data = subset(final_mm_dataset, elite_8 == 0),
             aes(x = Adjusted.Offensive.Efficiency, y = Adjusted.Defensive.Efficiency),
             color = "gray80", size = 2, alpha = 0.7) +
  # Foreground (Elite 8 teams)
  geom_point(data = subset(final_mm_dataset, elite_8 == 1),
             aes(x = Adjusted.Offensive.Efficiency, y = Adjusted.Defensive.Efficiency),
             color = "darkblue", size = 4, alpha = 0.9, stroke = 1.2, shape = 21, fill = "darkblue") +
  
  labs(title = "Offensive and Defensive Ratings",
       x = "Offensive Rating",
       y = "Defensive Rating") +
  theme_minimal()

#Turnover Plot
ggplot() +
  # Background (non-Elite 8 teams)
  geom_point(data = subset(final_mm_dataset, elite_8 == 0),
             aes(x = StlRate, y = OppStlRate),
             color = "gray80", size = 2, alpha = 0.7) +
  # Foreground (Elite 8 teams)
  geom_point(data = subset(final_mm_dataset, elite_8 == 1),
             aes(x = StlRate, y = OppStlRate),
             color = "darkblue", size = 4, alpha = 0.9, stroke = 1.2, shape = 21, fill = "darkblue") +
  
  labs(title = "Non Adjusted Free Throw and Turnover PCT",
       x = "Force Turnover Percentage",
       y = "Turnover Percentage") +
  theme_minimal()

#ORPct and EFGPCT Plot
ggplot() +
  # Background (non-Elite 8 teams)
  geom_point(data = subset(final_mm_dataset, elite_8 == 0),
             aes(x = ORPct, y = eFGPct),
             color = "gray80", size = 2, alpha = 0.7) +
  geom_point(data = subset(final_mm_dataset, elite_8 == 1),
             aes(x = ORPct, y = eFGPct),
             color = "darkblue", size = 4, alpha = 0.9, stroke = 1.2, shape = 21, fill = "darkblue") +
  
  labs(title = "Elite Shooting & Offensive Rebounding",
       x = "Offensive Rebound Percentage",
       y = "Effective Field Goal Percentage") +
  theme_minimal()

#Trapezoid of excellence plot
ggplot() +
  # Background (non-Elite 8 teams)
  geom_point(data = subset(final_mm_dataset, elite_8 == 0),
             aes(x = AdjTempo, y = AdjEM),
             color = "gray80", size = 2, alpha = 0.7) +
  # Foreground (Elite 8 teams)
  geom_point(data = subset(final_mm_dataset, elite_8 == 1),
             aes(x = AdjTempo, y = AdjEM),
             color = "darkblue", size = 4, alpha = 0.9, stroke = 1.2, shape = 21, fill = "darkblue") +
  
  labs(title = "Trapezoid of Excellence",
       x = "Pace",
       y = "Net Rating") +
  theme_minimal()





predictions <- predict(rf_model, newdata = mm_teams_2025, type = "prob")
predictions <- as.data.frame(predictions)
predictions$teamname <- mm_teams_2025_adj$Mapped.ESPN.Team.Name
mm_teams_2025_adj$elite_8 <- predictions[, "1"]
mm_teams_2025_adj %>%
  arrange(desc(elite_8)) %>%
  select(Mapped.ESPN.Team.Name, elite_8) %>%
  head(68)


ggplot(elite_8_df, aes(x = Adjusted.Offensive.Efficiency.Rank)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
  labs(title = "Adjusted Offensive Efficiency Rank", x = "Rank", y = "Count")

# Offensive Rank Histogram
ggplot(elite_8_df, aes(x = Adjusted.Defensive.Efficiency.Rank)) +
  geom_histogram(binwidth = 10, fill = "tomato", color = "black") +
  labs(title = "Adjusted Defensive Efficiency Rank", x = "Rank", y = "Count")
mm_teams_2025_elite <-  mm_teams_2025 %>% filter(Adjusted.Offensive.Efficiency.Rank < 50 & Adjusted.Defensive.Efficiency.Rank < 50)
mm_teams_2025_elite_2 <- mm_teams_2025_elite %>% fliter(AdjEM > 22)

# Create both plots and assign them to variables
p1 <- ggplot(elite_8_df, aes(x = Adjusted.Offensive.Efficiency.Rank)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
  labs(title = "Adjusted Offensive Efficiency Rank", x = "Rank", y = "Count")

p2 <- ggplot(elite_8_df, aes(x = Adjusted.Defensive.Efficiency.Rank)) +
  geom_histogram(binwidth = 10, fill = "tomato", color = "black") +
  labs(title = "Adjusted Defensive Efficiency Rank", x = "Rank", y = "Count")

# Combine the two plots side-by-side
combined_plot <- p1 + p2

# Print the combined plot
combined_plot

#Confusion Matrix
# Raw count data
conf_data <- data.frame(
  Round = rep(c("Round of 32", "Sweet 16", "Elite 8", "Final Four"), each = 2),
  Outcome = rep(c("Correct", "Incorrect"), times = 4),
  Count = c(27, 5, 11, 5, 7, 1, 3, 1)
)

# Calculate percent of total per round
conf_data <- conf_data %>%
  group_by(Round) %>%
  mutate(Percent = Count / sum(Count))

# Plot with gradient based on Percent
ggplot(conf_data, aes(x = Outcome, y = Round, fill = Percent)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::percent(Percent, accuracy = .1)), color = "black", size = 5) +
  scale_fill_gradient(low = "gray90", high = "steelblue", labels = scales::percent) +
  labs(title = "Confusion Matrix by Tournament Round",
       x = "Prediction Outcome",
       y = "",
       fill = "Percent") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16))

