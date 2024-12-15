# Load necessary libraries
library(dplyr)

# Function to calculate summary statistics for RQ1
calculate_descriptive_stats <- function(data, cols) {
  data %>%
    summarise(across(all_of(cols), list(
      Mean = ~ mean(.x, na.rm = TRUE),
      SD = ~ sd(.x, na.rm = TRUE),
      Median = ~ median(.x, na.rm = TRUE),
      IQR = ~ IQR(.x, na.rm = TRUE)
    )))
}

# RQ1: Listening Frequency - Descriptive Statistics for Complex and Simple Tasks
rq1_listening_freq_stats <- calculate_descriptive_stats(Raw_Data, c("Q2_Freq_Complex", "Q2_Freq_Simple"))
print("Listening Frequency Descriptive Statistics:")
print(rq1_listening_freq_stats)

# Function to calculate frequency counts for RQ2 music types
calculate_music_type_counts <- function(data, complex_cols, simple_cols) {
  list(
    Complex = data %>% summarise(across(all_of(complex_cols), ~ sum(.x, na.rm = TRUE))),
    Simple = data %>% summarise(across(all_of(simple_cols), ~ sum(.x, na.rm = TRUE)))
  )
}

# RQ2: Music Types Preference - Frequency Counts for Complex and Simple Tasks
rq2_music_type_counts <- calculate_music_type_counts(
  Raw_Data,
  complex_cols = c("Q4_Complex_Type_Vocal", "Q4_Complex_Type_Instrumental", "Q4_Complex_Type_Upbeat", 
                   "Q4_Complex_Type_Calming", "Q4_Complex_Type_Not_Applicable", "Q4_Complex_Type_Other"),
  simple_cols = c("Q4_Simple_Type_Vocal", "Q4_Simple_Type_Instrumental", "Q4_Simple_Type_Upbeat", 
                  "Q4_Simple_Type_Calming", "Q4_Simple_Type_Not_Applicable", "Q4_Simple_Type_Other")
)

print("Music Types Preference (Complex Tasks):")
print(rq2_music_type_counts$Complex)
print("Music Types Preference (Simple Tasks):")
print(rq2_music_type_counts$Simple)

# Calculate gender-based demographic statistics including Age and Extroversion Rate
gender_demographics <- Raw_Data %>%
  group_by(Gender) %>%
  summarise(
    Count = n(),
    Percentage = (n() / nrow(Raw_Data)) * 100,
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    SD_Age = sd(Age, na.rm = TRUE),
    IQR_Age = IQR(Age, na.rm = TRUE),
    Extraversion_Rate = mean(ifelse(Personality_Type == "Extrovert", 1, 0), na.rm = TRUE) * 100
  )

# Add a total row for all participants
total_row <- Raw_Data %>%
  summarise(
    Gender = "Total",
    Count = n(),
    Percentage = 100,
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    SD_Age = sd(Age, na.rm = TRUE),
    IQR_Age = IQR(Age, na.rm = TRUE),
    Extraversion_Rate = mean(ifelse(Personality_Type == "Extrovert", 1, 0), na.rm = TRUE) * 100
  )

# Combine gender statistics with the total row
gender_demographics <- bind_rows(gender_demographics, total_row)

# Display the combined table
print("Gender Demographic Statistics Table")
print(gender_demographics)


################################################################################

# Function to calculate reasons for listening/avoiding background music
calculate_reasons <- function(data, reason_cols) {
  data %>%
    summarise(across(all_of(reason_cols), ~ sum(.x, na.rm = TRUE))) %>%
    mutate(across(everything(), ~ . / nrow(data) * 100))
}

# RQ3: Reasons for Listening - Combined for Complex and Simple Tasks
rq3_complex_reasons <- calculate_reasons(Raw_Data, c("Q6_Complex_Reason_Noise", "Q6_Complex_Reason_Concentrate", 
                                                     "Q6_Complex_Reason_Mood", "Q6_Complex_Reason_Not_Applicable"))
rq3_simple_reasons <- calculate_reasons(Raw_Data, c("Q6_Simple_Reason_Noise", "Q6_Simple_Reason_Concentrate", 
                                                    "Q6_Simple_Reason_Mood", "Q6_Simple_Reason_Not_Applicable"))

# RQ4: Reasons for Avoiding - Combined for Complex and Simple Tasks
rq4_complex_avoid <- calculate_reasons(Raw_Data, c("Q8_Complex_Distracts", "Q8_Complex_Anxious", 
                                                   "Q8_Complex_No_Avoid"))
rq4_simple_avoid <- calculate_reasons(Raw_Data, c("Q8_Simple_Distracts", "Q8_Simple_Anxious", 
                                                  "Q8_Simple_No_Avoid"))

# Display results for RQ3 and RQ4
print("Reasons for Listening (Complex Tasks):")
print(rq3_complex_reasons)
print("Reasons for Listening (Simple Tasks):")
print(rq3_simple_reasons)

print("Reasons for Avoiding (Complex Tasks):")
print(rq4_complex_avoid)
print("Reasons for Avoiding (Simple Tasks):")
print(rq4_simple_avoid)

# Function to calculate text category percentages
calculate_text_categories <- function(data, column) {
  data %>%
    filter(!is.na(.data[[column]])) %>%
    group_by(.data[[column]]) %>%
    summarise(Count = n(), Percentage = (n() / nrow(data)) * 100) %>%
    arrange(desc(Count))
}

# Organize Text Categories (Q3, Q5, Q7, Q9, Q11) into a Consistent Format
q3_categories <- calculate_text_categories(Raw_Data, "Q3_Text_Categories")
q5_categories <- calculate_text_categories(Raw_Data, "Q5_Text_Categories")
q7_categories <- calculate_text_categories(Raw_Data, "Q7_Text_Categories")
q9_categories <- calculate_text_categories(Raw_Data, "Q9_Text_Categories")
q11_categories <- calculate_text_categories(Raw_Data, "Q11_Text_Categories")

# Display results for text categories
print("Q3 Categories:")
print(q3_categories)
print("Q5 Categories:")
print(q5_categories)
print("Q7 Categories:")
print(q7_categories)
print("Q9 Categories:")
print(q9_categories)
print("Q11 Categories:")
print(q11_categories)

