# Ensure 'studentS.csv' is in the current working directory (check with getwd())

# Load student data
students <- read.csv("C:\\Users\\Akhila Ranga\\Desktop\\studentInfo.csv")


# Data preprocessing

# **Check for active data frame:**
# print(Sys.getenv("R_STUDIO_CURRENT_DATA"))  # For RStudio users
# OR
# print(getwd())  # Prints current working directory (data frame location might be implied)

# Transform 'final_result' into a binary variable
students$is_passed <- as.factor(ifelse(students$final_result == "Pass", 1, 0))

# Convert 'credits' into a factor directly (avoiding *tmp*):
students$credits <- factor(students$studied_credits)

# Convert 'imd_band' to a numeric scale based on given categories
imd_scale <- c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
students$imd_numeric <- as.numeric(factor(students$imd_band, levels = imd_scale))

# **Choose one of the following solutions depending on your data and analysis goals:**

# **Solution 1: Adjust Credit Levels in Test Data (if applicable):**

# Identify rows with new credit levels (optional)
# new_credit_rows <- students[students$credits %in% c(85, 175, 235, 250, 310, 315, 325, 345, 390),]$id  # Replace 'id' with your actual ID column name

# Option 1: Correct the credit values (if possible)
# Modify the credit values in 'new_credit_rows' based on your knowledge

# Option 2: Remove rows with new credit levels (if applicable)
# test_data <- test_data[!test_data$id %in% new_credit_rows,]  # Replace 'id' with your actual ID column name

# **Solution 2: Combine Levels (if reasonable):**

# Define new factor levels (adjust ranges as needed)
# new_credit_levels <- c("0-99", "100-199", "200-299", "300+")

# Convert 'credits' to a factor with new levels (if applicable)
# students$credits <- factor(students$studied_credits, levels = new_credit_levels)
# test_data$credits <- factor(test_data$studied_credits, levels = new_credit_levels)  # Ensure consistency

# **Solution 3: Re-train Model with All Data:**

# Combine training and test data (if applicable)
# ... (follow instructions from previous solutions if chosen)

# Creating train and test sets manually
set.seed(20230712)  # Setting seed for reproducibility
sample_count <- floor(0.8 * nrow(students))  # Splitting the data into 80% as train data and 20% as test data.
training <- sample(seq_len(nrow(students)), size = sample_count)

train_data <- students[training, ]
test_data <- students[-training, ]

# Building a logistic regression model with glm (Generalized Linear Model) in base R
logit_model <- glm(is_passed ~ credits + imd_numeric, family = binomial(link = "logit"), data = train_data)

# Model summary display.
summary(logit_model)

# Prediction on test data
test_predictions <- predict(logit_model, test_data, type = "response")
predicted_outcome <- ifelse(test_predictions > 0.5, 1, 0)

# Accuracy calculation
true_outcomes <- as.numeric(test_data$is_passed) - 1  # Adjusting factor levels from 1 to 0 and 1 for comparison
model_accuracy <- mean(predicted_outcome == true_outcomes)
print(paste("Model Accuracy:", model_accuracy))

# Creating a bar chart for predicted vs. actual outcomes
library(ggplot2)  # Load ggplot2 for creating the
