library(NHANES)
library(knitr)
library(dplyr)
library(ggplot2)
library(mosaic)

# Create a column for hypertension based on measurements being above the medical threshold for systolic and diastolic blood pressure
NHANE <- NHANES %>%
  mutate(hypertension = ifelse(BPSysAve >= 130 | BPDiaAve >= 80, 1, 0))

# Filter out NA values for current smoking status and hypertension
filter_data <- NHANE %>%
  filter(!is.na(SmokeNow))

second_filtered_data <- filter_data %>%
  filter(!is.na(hypertension))

# Select the relevant variables: systolic BP, diastolic BP, current smoking status, and hypertension
needed_variables <- second_filtered_data %>%
  select(BPSysAve, BPDiaAve, SmokeNow, hypertension)

# Create separate groups for smoking and non-smoking observations
smoke_group <- needed_variables %>%
  filter(SmokeNow == "Yes")

non_smoke_group <- needed_variables %>%
  filter(SmokeNow == "No")

# Generate basic summary statistics for the selected variables
summary_table <- needed_variables %>%
  group_by(SmokeNow, hypertension) %>%
  summarise(count = n(), .groups = 'drop')

# Display the summary table in a readable format
kable(summary_table)

# Calculate mean and standard deviation for hypertension in the smoking group
smoke_mean <- mean(smoke_group$hypertension)
smoke_sd <- sd(smoke_group$hypertension)

# Calculate mean and standard deviation for hypertension in the non-smoking group
non_smoke_mean <- mean(non_smoke_group$hypertension)
non_smoke_sd <- sd(non_smoke_group$hypertension)

# Create a summary table for means and standard deviations
summary_table_mean_sd <- data.frame(Group = c("Smoking Group", "Non-smoking Group"), Mean = c(smoke_mean, non_smoke_mean))

# Display the summary table of means and standard deviations
kable(summary_table_mean_sd)

# Generate summary statistics for systolic BP levels between the two groups
sys_stats_table <- favstats(BPSysAve ~ SmokeNow, data = needed_variables)
print(sys_stats_table)

# Generate summary statistics for diastolic BP levels between the two groups
dia_stats_table <- favstats(BPDiaAve ~ SmokeNow, data = needed_variables)
print(dia_stats_table)

# Create a stacked bar chart to compare hypertension counts between smoking and non-smoking groups
ggplot(summary_table, aes(x = SmokeNow, y = count, fill = as.factor(hypertension))) +
  geom_bar(stat = "identity") +
  labs(title = "Comparison of Hypertension by Current Smoking Status", x = "Smoking Status", y = "Count", fill = "Hypertension")

# Create a box plot to compare systolic BP distributions between smoking and non-smoking groups
ggplot(needed_variables, aes(x = factor(SmokeNow), y = BPSysAve)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Not Smoking", "Smoking Now")) +
  labs(title = "Comparison of Average Systolic BP between Currently Smoking and Not Currently Smoking", x = "Smoking Status", y = "Average Systolic BP")

# Create a box plot to compare diastolic BP distributions between smoking and non-smoking groups
ggplot(needed_variables, aes(x = factor(SmokeNow), y = BPDiaAve)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Not Smoking", "Smoking Now")) +
  labs(title = "Comparison of Average Diastolic BP between Currently Smoking and Not Currently Smoking", x = "Smoking Status", y = "Average Diastolic BP")

# Perform Welch two-sample t-test to compare systolic BP between smoking and non-smoking groups
t_test_systolic <- t.test(non_smoke_group$BPSysAve, smoke_group$BPSysAve, var.equal = FALSE)
print(t_test_systolic)

# Perform Welch two-sample t-test to compare diastolic BP between smoking and non-smoking groups
t_test_diastolic <- t.test(non_smoke_group$BPDiaAve, smoke_group$BPDiaAve, var.equal = FALSE)
print(t_test_diastolic)

# Create a contingency table for current smoking status and hypertension
observed <- table(needed_variables$SmokeNow, needed_variables$hypertension)

# Perform Pearson Chi-squared test with Yates' continuity correction to test the association between smoking and hypertension
chisq_test <- chisq.test(observed, correct = TRUE)
print(chisq_test)


