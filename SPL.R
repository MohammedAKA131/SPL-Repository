#                                        1. Data Wrangling 
# -------------------------------------------------------------------------------------------------
# -------------------- 1. Load Libraries --------------------
library(tidyverse)   # For data manipulation and visualization
library(dplyr)       # For data wrangling
library(tidyr)       # For reshaping data
library(lubridate)   # For date formatting
library(readxl)      # For reading Excel files
library(ggplot2)     # For plotting graphs

# -------------------- 2. Load the Dataset --------------------
# Load dataset from the Excel file
#file_path <- "SPL_2014-2024.xlsx"
#spl_data <- read_excel(file_path)

spl_data <- read_excel("SPL 2014-2024.xlsx")
View(spl_data)

# View the first few rows
head(spl_data)

# View the basic structure of the dataset
str(spl_data)

# -------------------- 3. Handle Missing Values --------------------
# Check for missing values in each column
colSums(is.na(spl_data))

# Replace missing numeric values with column median
spl_data <- spl_data %>% 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Replace missing categorical values with "Unknown"
spl_data <- spl_data %>% 
  mutate(across(where(is.character), ~ifelse(is.na(.), "Unknown", .)))

# -------------------- 4. Transform Data Types --------------------
# Convert Date column from text to Date format (robust handling)
spl_data$Date <- parse_date_time(spl_data$Date, orders = c("dmy", "ymd", "mdy"))

# rename colnames
colnames(spl_data)
colnames(spl_data) <- str_trim(colnames(spl_data))  # Trim spaces
colnames(spl_data) <- gsub(" ", "_", colnames(spl_data))  # Replace spaces with underscores
# Convert categorical variables to factors
spl_data$Club_Home <- as.factor(spl_data$Club_Home)
spl_data$Club_Away <- as.factor(spl_data$Club_Away)
spl_data$Round <- factor(spl_data$Round, ordered = TRUE)  # Ensure logical order for rounds


#Why Convert Categorical Variables to Factors in R?
#1) Reduces memory usage by storing categorical data efficiently.
#2) Improves performance when grouping, summarizing, or plotting.
#3) Essential for machine learning models that need categorical encoding.
#4) Prevents accidental errors by ensuring consistency in categorical variables.

# -------------------- 5. Standardizing & Cleaning Text Data --------------------
# Ensure consistency in team names by trimming spaces
spl_data$Club_Home <- str_trim(spl_data$Club_Home)
spl_data$Club_Away <- str_trim(spl_data$Club_Away)

# Convert team names to lowercase for consistency
spl_data$Club_Home <- tolower(spl_data$Club_Home)
spl_data$Club_Away <- tolower(spl_data$Club_Away)



# -------------------- 6. Create a 'Match Outcome' Column --------------------
# Create a new column to classify match results
spl_data <- spl_data %>%
  mutate(Result = case_when(
    Score_Home > Score_Away ~ "Home Win",
    Score_Home < Score_Away ~ "Away Win",
    TRUE ~ "Draw"
  ))

# -------------------- 7. Remove Duplicates & Sort Data --------------------
# Remove duplicate rows if any exist
spl_data <- spl_data %>% distinct()

# Sort the dataset by date
spl_data <- spl_data %>% arrange(Date)

# -------------------- 8. Save the Cleaned Data --------------------
# Save the cleaned dataset for further analysis
write.csv(spl_data, "Cleaned_SPL_Data.csv", row.names = FALSE)

# View the cleaned data
head(spl_data)





#                                        2. Exploratory Data Analysis (EDA) & Visualization  
# -------------------------------------------------------------------------------------------------
#Load Libraries
library(ggplot2)     # For plotting graphs
# -------------------- 1. Generate Summary Statistics --------------------
# View summary statistics for numerical columns
summary(spl_data)

# Check the distribution of home and away scores
summary(spl_data$Score_Home)
summary(spl_data$Score_Away)

# Calculate mean, median, standard deviation for key variables
spl_data %>% 
  summarise(
    Mean_Home_Goals = mean(Score_Home, na.rm = TRUE),
    Median_Home_Goals = median(Score_Home, na.rm = TRUE),
    SD_Home_Goals = sd(Score_Home, na.rm = TRUE),
    Mean_Away_Goals = mean(Score_Away, na.rm = TRUE),
    Median_Away_Goals = median(Score_Away, na.rm = TRUE),
    SD_Away_Goals = sd(Score_Away, na.rm = TRUE)
  )

# -------------------- 2. Visualizing Data Distributions --------------------
# 2.1 Histogram: Distribution of Home & Away Scores
ggplot(spl_data, aes(x = Score_Home)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Home Scores", x = "Goals Scored", y = "Frequency")

ggplot(spl_data, aes(x = Score_Away)) +
  geom_histogram(binwidth = 1, fill = "red", alpha = 0.7) +
  labs(title = "Distribution of Away Scores", x = "Goals Scored", y = "Frequency")

# 2.2 Box Plot: Comparing Home vs. Away Scores
ggplot(spl_data, aes(x = Result, y = Score_Home, fill = Result)) +
  geom_boxplot() +
  labs(title = "Home Score Distribution by Match Outcome", x = "Match Outcome", y = "Home Goals")

ggplot(spl_data, aes(x = Result, y = Score_Away, fill = Result)) +
  geom_boxplot() +
  labs(title = "Away Score Distribution by Match Outcome", x = "Match Outcome", y = "Away Goals")

# 2.3 Scatter Plot: Relationship Between Home & Away Scores
ggplot(spl_data, aes(x = Score_Home, y = Score_Away)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot of Home vs Away Scores", x = "Home Goals", y = "Away Goals")

# 2.4 Top 10 Goal-Scoring Matchups
# Identify the top 10 highest scoring matches
high_scoring_matches <- spl_data %>% 
  mutate(Total_Goals = Score_Home + Score_Away) %>% 
  arrange(desc(Total_Goals)) %>% 
  head(10)

# Bar Chart: Top 10 Goal-Scoring Matches
ggplot(high_scoring_matches, aes(x = reorder(paste(Club_Home, 'vs', Club_Away), -Total_Goals), y = Total_Goals)) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Top 10 Highest Scoring Matches", x = "Matchup", y = "Total Goals") +
  theme_minimal()

# 2.5 Pie Chart: Proportion of Wins, Losses, and Draws
# Count occurrences of each match result
match_results <- spl_data %>%
  group_by(Result) %>%
  summarise(Count = n())

# Pie Chart for Match Results
ggplot(match_results, aes(x = "", y = Count, fill = Result)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Proportion of Wins, Losses, and Draws", x = NULL, y = NULL) +
  theme_void() +
  theme(legend.title = element_blank())

# 2.6 Scatter Plot: Goals Scored vs. Goals Conceded
ggplot(spl_data, aes(x = Score_Home, y = Score_Away)) +
  geom_point(alpha = 0.6, color = "darkred") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot: Goals Scored vs Goals Conceded", x = "Goals Scored (Home)", y = "Goals Conceded (Away)") +
  theme_minimal()

# 2.7 Line Plot: Total Goals Scored per Season
# Aggregate goals per season
seasonal_goals <- spl_data %>%
  mutate(Season = year(Date)) %>%
  group_by(Season) %>%
  summarise(Total_Goals = sum(Score_Home + Score_Away, na.rm = TRUE))

# Line Chart for Total Goals per Season
ggplot(seasonal_goals, aes(x = Season, y = Total_Goals)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Total Goals Scored Per Season", x = "Season", y = "Total Goals") +
  theme_minimal()


# 2.8 Bar Chart: Average Total Goals for Home and Away for Each Top Club
avg_goals_top_clubs <- spl_data %>%
  group_by(Club_Home) %>%
  summarise(Avg_Home_Goals = mean(Score_Home, na.rm = TRUE),
            Avg_Away_Goals = mean(Score_Away, na.rm = TRUE)) %>%
  arrange(desc(Avg_Home_Goals))

ggplot(avg_goals_top_clubs, aes(x = reorder(Club_Home, -Avg_Home_Goals), y = Avg_Home_Goals, fill = "Home")) +
  geom_bar(stat = "identity", alpha = 0.7) +
  coord_flip() +
  labs(title = "Average Goals Scored at Home and Away by Top Clubs", x = "Club", y = "Average Goals") +
  theme_minimal()

# 2.9 Bar Chart: Average Received Goals at Home and Away
avg_received_goals <- spl_data %>%
  group_by(Club_Away) %>%
  summarise(Avg_Received_Home = mean(Score_Away, na.rm = TRUE),
            Avg_Received_Away = mean(Score_Home, na.rm = TRUE)) %>%
  arrange(desc(Avg_Received_Home))

ggplot(avg_received_goals, aes(x = reorder(Club_Away, -Avg_Received_Home), y = Avg_Received_Home, fill = "Received Home")) +
  geom_bar(stat = "identity", alpha = 0.7) +
  coord_flip() +
  labs(title = "Average Goals Received at Home and Away", x = "Club", y = "Average Goals Received") +
  theme_minimal()

# 2.10 Bar Chart: Average Wins Between Specific Clubs
avg_win_clashes <- function(team1, team2) {
  spl_data %>%
    filter((Club_Home == team1 & Club_Away == team2 & Result == "Home Win") |
             (Club_Home == team2 & Club_Away == team1 & Result == "Away Win")) %>%
    summarise(Avg_Wins = n()/nrow(spl_data))
}

matchups <- data.frame(
  Matchup = c("Al-Hilal vs Al-Nassr", "Al-Nassr vs Al-Shabab", "Al-Hilal vs Al-Shabab", 
              "Al-Nassr vs Al-Ittihad FC", "Al Ahli SC vs Al-Ittihad FC", "Al-Nassr vs Al Ahli SC", 
              "Al-Hilal vs Al Ahli SC", "Al-Nassr vs Al-Taawon", "Al Ahli SC vs Al-Taawon", 
              "Al-Nassr vs Al-Fateh"),
  Avg_Wins = c(avg_win_clashes("al-hilal", "al-nassr")$Avg_Wins,
               avg_win_clashes("al-nassr", "al-shabab")$Avg_Wins,
               avg_win_clashes("al-hilal", "al-shabab")$Avg_Wins,
               avg_win_clashes("al-nassr", "al-ittihad fc")$Avg_Wins,
               avg_win_clashes("al ahli sc", "al-ittihad fc")$Avg_Wins,
               avg_win_clashes("al-nassr", "al ahli sc")$Avg_Wins,
               avg_win_clashes("al-hilal", "al ahli sc")$Avg_Wins,
               avg_win_clashes("al-nassr", "al-taawon")$Avg_Wins,
               avg_win_clashes("al ahli sc", "al-taawon")$Avg_Wins,
               avg_win_clashes("al-nassr", "al-fateh")$Avg_Wins))

ggplot(matchups, aes(x = reorder(Matchup, -Avg_Wins), y = Avg_Wins, fill = "Wins")) +
  geom_bar(stat = "identity", alpha = 0.7) +
  coord_flip() +
  labs(title = "Average Wins Between Key Club Matchups", x = "Matchup", y = "Average Wins") +
  theme_minimal()

# 2.11 Bar Chart: Top Teams with the Most Wins
top_teams_wins <- spl_data %>%
  filter(Result == "Home Win" | Result == "Away Win") %>%
  mutate(Winner = ifelse(Result == "Home Win", Club_Home, Club_Away)) %>%
  group_by(Winner) %>%
  summarise(Total_Wins = n()) %>%
  arrange(desc(Total_Wins))

ggplot(top_teams_wins, aes(x = reorder(Winner, -Total_Wins), y = Total_Wins, fill = Winner)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  coord_flip() +
  labs(title = "Teams with the Most Wins", x = "Team", y = "Total Wins") +
  theme_minimal()

# 2.12 Bar Chart: Top 10 Teams with the Highest Goal Tally in Matches
top_scorers <- spl_data %>%
  group_by(Club_Home, Club_Away) %>%
  summarise(Total_Goals_Scored = sum(Score_Home, na.rm = TRUE)) %>%
  arrange(desc(Total_Goals_Scored)) %>%
  head(10)

ggplot(top_scorers, aes(x = reorder(paste(Club_Home, "vs", Club_Away), -Total_Goals_Scored), y = Total_Goals_Scored, fill = Club_Home)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  coord_flip() +
  labs(title = "Top 10 Teams with the Highest Goal Tally in Matches", x = "Matchup", y = "Goals Scored by Home Team") +
  theme_minimal()

# 2.13 Bar Chart: Top 10 Goal-Scoring Matchups
top_goal_matchups <- spl_data %>%
  mutate(Total_Goals = Score_Home + Score_Away) %>%
  group_by(Club_Home, Club_Away) %>%
  summarise(Total_Goals = sum(Total_Goals, na.rm = TRUE)) %>%
  arrange(desc(Total_Goals)) %>%
  head(10)

ggplot(top_goal_matchups, aes(x = reorder(paste(Club_Home, "vs", Club_Away), -Total_Goals), y = Total_Goals, fill = Club_Home)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  coord_flip() +
  labs(title = "Top 10 Goal-Scoring Matchups", x = "Matchup", y = "Total Goals Scored") +
  theme_minimal()


# 2.14 Linear Regression: Total Score for Home and Away for Each Club
# Aggregate total home and away scores per club
total_scores <- spl_data %>%
  group_by(Club_Home) %>%
  summarise(Total_Home_Goals = sum(Score_Home, na.rm = TRUE),
            Total_Away_Goals = sum(Score_Away, na.rm = TRUE))

# Fit linear regression model
lm_total_scores <- lm(Total_Home_Goals ~ Total_Away_Goals, data = total_scores)

# Scatter plot with regression line
ggplot(total_scores, aes(x = Total_Away_Goals, y = Total_Home_Goals)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression: Total Home vs Away Goals for Each Club",
       x = "Total Away Goals", y = "Total Home Goals") +
  theme_minimal()

# -------------------- 3. Analyzing Team Performance --------------------
# Compare average wins and goals between Home and Away
spl_data %>%
  group_by(Result) %>%
  summarise(
    Avg_Home_Goals = mean(Score_Home),
    Avg_Away_Goals = mean(Score_Away)
  )



# Identify Top Teams Based on Wins
# Count total wins for each team
top_teams <- spl_data %>%
  filter(Result == "Home Win" | Result == "Away Win") %>%
  mutate(Winner = ifelse(Result == "Home Win", Club_Home, Club_Away)) %>%
  group_by(Winner) %>%
  summarise(Total_Wins = n()) %>%
  arrange(desc(Total_Wins))

# Display Top Teams by Wins
print(top_teams)


# -------------------- 4. Trends Over Time --------------------
# 4.1 Goals Scored Over Time
ggplot(spl_data, aes(x = Date, y = Score_Home, color = Club_Home)) +
  geom_line() +
  labs(title = "Goals Scored Over Time (Home Teams)", x = "Date", y = "Goals Scored")

ggplot(spl_data, aes(x = Date, y = Score_Away, color = Club_Away)) +
  geom_line() +
  labs(title = "Goals Scored Over Time (Away Teams)", x = "Date", y = "Goals Scored")

# -------------------- 5. Identifying Patterns & Anomalies --------------------
# Identify teams with highest average goals scored
spl_data %>%
  group_by(Club_Home) %>%
  summarise(Avg_Home_Goals = mean(Score_Home, na.rm = TRUE)) %>%
  arrange(desc(Avg_Home_Goals)) %>%
  head(10)

# Identify teams with lowest scoring performance
spl_data %>%
  group_by(Club_Away) %>%
  summarise(Avg_Away_Goals = mean(Score_Away, na.rm = TRUE)) %>%
  arrange(Avg_Away_Goals) %>%
  head(10)

# -------------------- 6. Save Processed Data --------------------
write.csv(spl_data, "EDA_SPL_Data.csv", row.names = FALSE)







#                                        4. Statistical Analysis  
# -------------------------------------------------------------------------------------------------
# -------------------- 1. Perform Statistical Tests --------------------
library(stats)   # For statistical tests

# 4.1 T-Test: Compare Home and Away Goals
# Checking if the average number of goals scored at home is significantly different from away goals
t_test_result <- t.test(spl_data$Score_Home, spl_data$Score_Away, paired = TRUE)
print(t_test_result)
######################################################################################
#################################Warning message:#####################################################
######################################################################################
# 4.2 Chi-Square Test: Checking Relationship Between Match Outcome and Home/Away Teams
# Create a contingency table for match results
match_result_table <- table(spl_data$Result, spl_data$Club_Home)
chi_sq_test <- chisq.test(match_result_table)
print(chi_sq_test)
#--------------
# Collapse teams with fewer than 5 matches into "Other"
spl_data$Club_Home <- fct_lump_min(as.factor(spl_data$Club_Home), min = 5)
# Re-run the Chi-Square test
match_result_table <- table(spl_data$Result, spl_data$Club_Home)
chi_sq_test <- chisq.test(match_result_table)
print(chi_sq_test)
######################################################################################
#################################Warning message:#####################################################
######################################################################################

# -------------------- 2. Regression Analysis --------------------
# 4.3 Linear Regression: Predicting Home Score Based on Season and Opponent Strength
lm_model <- lm(Score_Home ~ Score_Away + as.factor(Round) + as.factor(Club_Away), data = spl_data)
summary(lm_model)

# 4.4 Logistic Regression: Predicting Match Outcome
# Convert Result into a binary variable (Win = 1, Draw/Loss = 0)
spl_data <- spl_data %>%
  mutate(Win = ifelse(Result == "Home Win", 1, 0))

logit_model <- glm(Win ~ Score_Home + Score_Away + as.factor(Club_Home) + as.factor(Club_Away), 
                   data = spl_data, family = binomial)
summary(logit_model)

# -------------------- 3. Interpretation of Results --------------------
# Extract key insights from regression analysis
print("Linear Regression Summary:")
print(summary(lm_model))

print("Logistic Regression Summary:")
print(summary(logit_model))

# 3.1 Identify Top Teams Based on Wins
# Count total wins for each team
top_teams <- spl_data %>%
  filter(Result == "Home Win" | Result == "Away Win") %>%
  mutate(Winner = ifelse(Result == "Home Win", Club_Home, Club_Away)) %>%
  group_by(Winner) %>%
  summarise(Total_Wins = n()) %>%
  arrange(desc(Total_Wins))

# Display Top Teams by Wins
print(top_teams)

# 3.2 Count Most Championships Won Per Team
# Assuming each season has a league winner, let's find the top champions
# We need a dataset with season-wise standings (requires external standings data)

# Example (if available in data): Count the number of times each team was a champion
# championships <- spl_data %>%
#   group_by(Season, Winner) %>%
#   summarise(Championships_Won = n()) %>%
#   arrange(desc(Championships_Won))

# print(championships)

# Note: If championship data is not in the dataset, we may need additional data input.


# Compare average wins and goals between Home and Away
spl_data %>%
  group_by(Result) %>%
  summarise(
    Avg_Home_Goals = mean(Score_Home),
    Avg_Away_Goals = mean(Score_Away)
  )

# -------------------- 4. Trends Over Time --------------------
# 4.1 Goals Scored Over Time
ggplot(spl_data, aes(x = Date, y = Score_Home, color = Club_Home)) +
  geom_line() +
  labs(title = "Goals Scored Over Time (Home Teams)", x = "Date", y = "Goals Scored")

ggplot(spl_data, aes(x = Date, y = Score_Away, color = Club_Away)) +
  geom_line() +
  labs(title = "Goals Scored Over Time (Away Teams)", x = "Date", y = "Goals Scored")

# -------------------- 5. Save Processed Data --------------------
write.csv(spl_data, "EDA_SPL_Data.csv", row.names = FALSE)


# --------------------  Random Forest --------------------
# Load required package
library(caret)

# Create a confusion matrix
conf_matrix <- confusionMatrix(as.factor(rf_predictions), as.factor(test_data$Result_Category))

# Extracting performance metrics
accuracy <- conf_matrix$overall["Accuracy"]
kappa <- conf_matrix$overall["Kappa"]

# Precision, Recall, and F1-score
precision <- conf_matrix$byClass["Pos Pred Value"]  # Precision per class
recall <- conf_matrix$byClass["Sensitivity"]  # Recall per class
f1_score <- 2 * ((precision * recall) / (precision + recall))  # F1 Score

# Display results
performance_metrics <- data.frame(
  Metric = c("Accuracy", "Kappa", "Precision (Win)", "Precision (Draw)", "Precision (Loss)",
             "Recall (Win)", "Recall (Draw)", "Recall (Loss)", 
             "F1 Score (Win)", "F1 Score (Draw)", "F1 Score (Loss)"),
  Value = c(accuracy, kappa, precision[1], precision[2], precision[3], 
            recall[1], recall[2], recall[3], 
            f1_score[1], f1_score[2], f1_score[3])
)

print(performance_metrics)


# --------------------  Confusion Matrix --------------------
# Convert predictions and actual results to Win/Draw/Loss categories
test_data <- test_data %>%
  mutate(
    Result_Category = case_when(
      Result == "Home Win" | Result == "Away Win" ~ "Win",
      Result == "Draw" ~ "Draw",
      TRUE ~ "Loss"
    )
  )

rf_predictions <- as.character(rf_predictions)  # Convert factor to character
rf_predictions <- ifelse(rf_predictions == "Home Win" | rf_predictions == "Away Win", "Win", 
                         ifelse(rf_predictions == "Draw", "Draw", "Loss"))

# Generate new confusion matrix
conf_matrix <- confusionMatrix(as.factor(rf_predictions), as.factor(test_data$Result_Category))

# Print the updated confusion matrix
print(conf_matrix)

# --------------------  Match prediction --------------------
# Combine performance and goal stats
alnassr_overall_performance <- alnassr_performance %>%
  bind_cols(alnassr_goals)

# Save the predictions for Al-Nassr to a CSV
write.csv(alnassr_overall_performance, "alnassr_predicted_performance.csv", row.names = FALSE)

# Print the final performance summary
print(alnassr_overall_performance)



