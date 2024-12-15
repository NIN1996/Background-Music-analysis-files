# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyverse)

# Define a consistent color palette
colors <- list(
  mean = "red",
  median = "blue",
  complex = "#B2DF8A",  # Light green for complex tasks
  simple = "#FDBF6F",   # Light orange for simple tasks
  vocal = "#A6CEE3",    # Soft blue for vocal preference
  upbeat = "#FFB6C1"    # Soft pink for upbeat preference
)

# 1. Age Distribution: Histogram with Mean and Median Lines and a Legend
ggplot(Raw_Data, aes(x = Age)) +
  geom_histogram(binwidth = 5, color = "white", fill = colors$complex) +  # Soft green for histogram
  labs(title = "Age Distribution of Participants", x = "Age", y = "Frequency") +
  geom_vline(aes(xintercept = mean(Age, na.rm = TRUE), color = "Mean"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(Age, na.rm = TRUE), color = "Median"), linetype = "dotted", size = 1) +
  scale_color_manual(name = "Statistics", values = c("Mean" = colors$mean, "Median" = colors$median)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "top"
  )

# 2. Gender Distribution: Bar Chart for Clarity
Raw_Data %>%
  filter(!is.na(Gender)) %>%  # Exclude NA values
  count(Gender) %>%
  ggplot(aes(x = reorder(Gender, -n), y = n, fill = Gender)) +
  geom_bar(stat = "identity", color = "white") +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  geom_text(aes(label = scales::percent(n / sum(n), accuracy = 1)), vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("#FFB6C1", "#A6CEE3", "#B2DF8A", "#FDBF6F", "#CAB2D6", "#FB9A99")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )

# 3a. Extraversion Score: Histogram for Distribution
ggplot(Raw_Data, aes(x = Extraversion_Score)) +
  geom_histogram(binwidth = 0.5, color = "white", fill = colors$complex) +  # Consistent green for histogram
  labs(title = "Distribution of Extroversion Scores", x = "Extroversion Score", y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )

# 3b. Extroversion Score: Minimalist Box Plot using boxplot()
boxplot(Raw_Data$Extraversion_Score,
        main = "Box Plot of Extroversion Scores",
        ylab = "Extroversion Score",
        border = "black",
        col = colors$complex,
        outline = TRUE,    # Show outliers
        whisklty = 1,      # Solid line for whiskers
        staplelty = 1,     # Solid line for staples
        boxlty = 1,        # Solid line for box
        medlty = 1,        # Solid line for median
        whiskcol = "black",
        staplecol = "black",
        boxcol = "black",
        medcol = "black")

#################################################################################################

# Q2: Listening Frequency for Complex vs. Simple Tasks

# 1. Bar Chart of Summary Statistics (Mean and Median)
Q2_summary <- Raw_Data %>%
  summarise(
    Mean_Complex = mean(Q2_Freq_Complex, na.rm = TRUE),
    Median_Complex = median(Q2_Freq_Complex, na.rm = TRUE),
    Mean_Simple = mean(Q2_Freq_Simple, na.rm = TRUE),
    Median_Simple = median(Q2_Freq_Simple, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Task_Statistic", values_to = "Value") %>%
  mutate(Task_Type = ifelse(grepl("Complex", Task_Statistic), "Complex", "Simple"),
         Statistic = ifelse(grepl("Mean", Task_Statistic), "Mean", "Median"))

# Plot bar chart
ggplot(Q2_summary, aes(x = Task_Type, y = Value, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  labs(title = "Listening Frequency Summary Statistics for Complex vs. Simple Tasks", 
       x = "Task Type", y = "Frequency (1-5 Scale)", fill = "Statistic") +
  scale_fill_manual(values = c("Mean" = colors$mean, "Median" = colors$median)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12))

# 2. Box Plot for Listening Frequency using boxplot()

# Combine Q2_Freq_Complex and Q2_Freq_Simple into a single data frame for box plotting
listening_data <- data.frame(
  Frequency = c(Raw_Data$Q2_Freq_Complex, Raw_Data$Q2_Freq_Simple),
  Task_Type = rep(c("Complex Tasks", "Simple Tasks"), each = nrow(Raw_Data))
)

# Box Plot for Listening Frequency by Task Type using boxplot()
boxplot(Frequency ~ Task_Type, data = listening_data,
        main = "Distribution of Listening Frequency by Task Type",
        xlab = "Task Type",
        ylab = "Frequency (1-5 Scale)",
        border = "black",
        col = c(colors$complex, colors$simple),
        outline = TRUE,         # Show outliers
        whisklty = 1,           # Solid line for whiskers
        staplelty = 1,          # Solid line for staples
        boxlty = 1,             # Solid line for box
        medlty = 1,             # Solid line for median
        whiskcol = "black",
        staplecol = "black",
        boxcol = "black",
        medcol = "black")

# H2: Preference for Vocal/Upbeat Music During Simple vs. Complex Tasks

# Create data for stacked bar chart by counting preferences
H2_data <- Raw_Data %>%
  summarise(
    Vocal_Complex = sum(Q4_Complex_Type_Vocal, na.rm = TRUE),
    Vocal_Simple = sum(Q4_Simple_Type_Vocal, na.rm = TRUE),
    Upbeat_Complex = sum(Q4_Complex_Type_Upbeat, na.rm = TRUE),
    Upbeat_Simple = sum(Q4_Simple_Type_Upbeat, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Category_Task", values_to = "Count") %>%
  separate(Category_Task, into = c("Music_Type", "Task_Type"), sep = "_")

# Plot stacked bar chart
ggplot(H2_data, aes(x = Task_Type, y = Count, fill = Music_Type)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  labs(title = "Preference for Vocal/Upbeat Music in Simple vs. Complex Tasks", 
       x = "Task Type", y = "Count", fill = "Music Type") +
  scale_fill_manual(values = c("Vocal" = colors$vocal, "Upbeat" = colors$upbeat)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12))

# Contingency Table for H2 (Supplemental)
H2_contingency_table <- Raw_Data %>%
  summarise(
    Vocal_Complex = sum(Q4_Complex_Type_Vocal, na.rm = TRUE),
    Vocal_Simple = sum(Q4_Simple_Type_Vocal, na.rm = TRUE),
    Upbeat_Complex = sum(Q4_Complex_Type_Upbeat, na.rm = TRUE),
    Upbeat_Simple = sum(Q4_Simple_Type_Upbeat, na.rm = TRUE)
  )
print("Contingency Table for H2:")
print(H2_contingency_table)

################################################################################

# Assuming we have a dataframe with columns: Task_Type, Reason, and Frequency

# Prepare data (example structure)
listening_data <- Raw_Data %>%
  pivot_longer(cols = c(Q6_Complex_Reason, Q6_Simple_Reason), 
               names_to = "Task_Type", 
               values_to = "Reason") %>%
  count(Task_Type, Reason) %>%
  mutate(Task_Type = recode(Task_Type, 
                            "Q6_Complex_Reason" = "Complex", 
                            "Q6_Simple_Reason" = "Simple"))

# Plot grouped bar chart
ggplot(listening_data, aes(x = Reason, y = n, fill = Task_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  labs(title = "Reasons for Listening to Background Music by Task Type",
       x = "", y = "Frequency", fill = "Task Type") +
  scale_fill_manual(values = c("Complex" = colors$complex, "Simple" = colors$simple)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )


# Prepare data for RQ4: Reasons for Avoiding Background Music
# Pivot, filter out NA values, and count responses for Complex and Simple reasons
avoidance_data <- Raw_Data %>%
  pivot_longer(cols = c(Q8_Complex_Avoidance, Q8_Simple_Avoidance),
               names_to = "Task_Type",
               values_to = "Reason") %>%
  filter(!is.na(Reason)) %>%  # Exclude NA values
  count(Task_Type, Reason) %>%
  mutate(Task_Type = recode(Task_Type,
                            "Q8_Complex_Avoidance" = "Complex",
                            "Q8_Simple_Avoidance" = "Simple"))

# Plot grouped bar chart for RQ4
ggplot(avoidance_data, aes(x = Reason, y = n, fill = Task_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  labs(title = "Reasons for Avoiding Background Music by Task Type (RQ4)",
       x = "", y = "Frequency", fill = "Task Type") +
  scale_fill_manual(values = c("Complex" = colors$complex, "Simple" = colors$simple)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

# Prepare data for RQ11: Effects of Background Music Over Time
# Pivot, filter out NA values, and count responses for Complex and Simple effects
effects_data <- Raw_Data %>%
  pivot_longer(cols = c(Q10_Complex_Effect, Q10_Simple_Effect),
               names_to = "Task_Type",
               values_to = "Effect") %>%
  filter(!is.na(Effect)) %>%  # Exclude NA values
  count(Task_Type, Effect) %>%
  mutate(Task_Type = recode(Task_Type,
                            "Q10_Complex_Effect" = "Complex",
                            "Q10_Simple_Effect" = "Simple"))

# Plot grouped bar chart for RQ11
ggplot(effects_data, aes(x = Effect, y = n, fill = Task_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  labs(title = "Effects of Background Music Over Time by Task Type (RQ11)",
       x = "", y = "Frequency", fill = "Task Type") +
  scale_fill_manual(values = c("Complex" = colors$complex, "Simple" = colors$simple)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

#############################################################################################

# H2: Preference for Vocal, Upbeat Music during Simple vs. Complex Tasks
# Summarize data for vocal and upbeat music preference during simple and complex tasks
H2_data <- Raw_Data %>%
  summarise(
    Vocal_Complex = sum(Q4_Complex_Type_Vocal, na.rm = TRUE),
    Vocal_Simple = sum(Q4_Simple_Type_Vocal, na.rm = TRUE),
    Upbeat_Complex = sum(Q4_Complex_Type_Upbeat, na.rm = TRUE),
    Upbeat_Simple = sum(Q4_Simple_Type_Upbeat, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Category_Task", values_to = "Count") %>%
  separate(Category_Task, into = c("Music_Type", "Task_Type"), sep = "_")

ggplot(H2_data, aes(x = Task_Type, y = Count, fill = Music_Type)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = c("Vocal" = "#A6CEE3", "Upbeat" = "#FDBF6F")) +
  labs(title = "Preference for Vocal and Upbeat Music in Simple vs. Complex Tasks",
       x = "Task Type", y = "Count", fill = "Music Type") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12))

# H3: Mean Listening Frequency during Complex Tasks by Personality Type
# Calculate mean and standard error for each group
mean_se_data <- Raw_Data %>%
  group_by(Personality_Type) %>%
  summarise(
    mean_frequency = mean(Q2_Freq_Complex, na.rm = TRUE),
    se_frequency = sd(Q2_Freq_Complex, na.rm = TRUE) / sqrt(n())
  )

# Bar chart with error bars
ggplot(mean_se_data, aes(x = Personality_Type, y = mean_frequency, fill = Personality_Type)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = mean_frequency - se_frequency, ymax = mean_frequency + se_frequency), 
                width = 0.2, color = "black") +
  scale_fill_manual(values = c("Extrovert" = "#A6CEE3", "Introvert" = "#FFB6C1")) +
  labs(title = "Mean Listening Frequency during Complex Tasks by Personality Type",
       x = "Personality Type", y = "Mean Frequency (1-5 Scale)", fill = "Personality Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )

# H4: Preference for Calming Music during Complex Tasks by Personality Type
calming_preference <- Raw_Data %>%
  filter(!is.na(Personality_Type), !is.na(Q4_Complex_Type_Calming)) %>%
  group_by(Personality_Type, Q4_Complex_Type_Calming) %>%
  summarise(count = n()) %>%
  mutate(Preference = ifelse(Q4_Complex_Type_Calming == 1, "Yes", "No"))

ggplot(calming_preference, aes(x = Personality_Type, y = count, fill = Preference)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = c("Yes" = "#A6CEE3", "No" = "#FFB6C1")) +
  labs(title = "Preference for Calming Music during Complex Tasks (Introverts vs. Extraverts)",
       x = "Personality Type", y = "Count", fill = "Preference") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12))

# H5: Preference for Calming Music during Complex Tasks by Gender
gender_calming_preference <- Raw_Data %>%
  filter(!is.na(Gender), !is.na(Q4_Complex_Type_Calming)) %>%
  group_by(Gender, Q4_Complex_Type_Calming) %>%
  summarise(count = n()) %>%
  mutate(Preference = ifelse(Q4_Complex_Type_Calming == 1, "Yes", "No"))

ggplot(gender_calming_preference, aes(x = Gender, y = count, fill = Preference)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = c("Yes" = "#B2DF8A", "No" = "#FDBF6F")) +
  labs(title = "Preference for Calming Music during Complex Tasks (Gender Differences)",
       x = "Gender", y = "Count", fill = "Preference") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12))

# H6: Correlation between Age and Listening Frequency during Complex Tasks
ggplot(Raw_Data %>% filter(!is.na(Age) & !is.na(Q2_Freq_Complex)), aes(x = Age, y = Q2_Freq_Complex)) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +  # Linear trend line
  labs(title = "Correlation between Age and Listening Frequency during Complex Tasks",
       x = "Age", y = "Frequency (1-5 Scale)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )

