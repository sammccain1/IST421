library(tidyverse)
library(scales)
library(ggplot2)

contracts_data <- Contracts_PrimeAwardSummaries_2025_02_27_H20M31S44_1 #imported from computer

company_summary <- contracts_data %>%
  group_by(recipient_name) %>%
  summarize(total_obligated_amount = sum(total_obligated_amount, na.rm = TRUE)) %>%
  arrange(desc(total_obligated_amount))

ggplot(contracts_data, aes(x = total_obligated_amount)) +
  geom_histogram(binwidth = 1e6, fill = "blue", color = "black") +
  scale_x_continuous(labels = dollar) +
  labs(
    title = "Distribution of Contract Amounts",
    x = "Contract Amount (USD)",
    y = "Frequency",
    caption = "Source: USAspending.gov"
  ) +
  theme_minimal()

# Plot top 10 companies by total obligation
top_10_companies <- company_summary %>% slice_head(n = 10)

ggplot(top_10_companies, aes(x = reorder(recipient_name, total_obligated_amount), y = total_obligated_amount)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  coord_flip() +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Top 10 Companies by Total Contract Obligations",
    x = "Company",
    y = "Total Obligation (USD)",
    caption = "Source: USAspending.gov"
  ) +
  theme_minimal()

# Summarize total obligations and contract count per company
company_summary <- contracts_data %>%
  group_by(recipient_name) %>%
  summarize(
    total_obligation = sum(total_obligated_amount, na.rm = TRUE),
    contract_count = n()
  ) %>%
  arrange(desc(total_obligation)) %>%
  top_n(20, total_obligation)  # Top 20 companies for better visualization

# Scatter plot
ggplot(company_summary, aes(x = contract_count, y = total_obligation, label = recipient_name)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_text(size = 3, vjust = -1, hjust = 0.5) +  # Add labels for readability
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Total Contracts vs. Total Obligation Amount",
    x = "Number of Contracts",
    y = "Total Contract Value (USD)",
    caption = "Source: USAspending.gov"
  ) +
  theme_minimal()