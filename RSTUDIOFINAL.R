read.csv("Prestige_New.csv")

#clean the data set
# Clean the data
newdata <- na.omit(Prestige_New)  # Remove missing values
newdata1 <- unique(newdata)  # Remove duplicates


install.packages("outliers")
library(outliers)

outliers <- outlier(newdata1$income)#remove outliers
newdata2 <- newdata1
View(newdata1)

outliers <- outlier(newdata1$women)#remove outliers
newdata2 <- newdata1
View(newdata1)

outliers <- outlier(newdata1$education)#remove outliers
newdata2 <- newdata1
View(newdata1)

outliers <- outlier(newdata1$prestige)#remove outliers
newdata2 <- newdata1
View(newdata1)

# Add a small constant to income to handle zeros (if needed) and take the log
# You can add a very small value like 1 to avoid log(0) issues
newdata1$log_income <- log(newdata1$income + 1)

# Check the first few rows to ensure it worked
head(newdata1)


# Assuming data already loaded and log_income created
newdata1$log_income <- log(newdata1$income + 1)

# Check for missing values in log_income
summary(newdata1$log_income)



# question 3

# Minimum
min_log_income <- min(newdata1$log_income)
print(paste("Minimum log_income:", min_log_income))

# Maximum
max_log_income <- max(newdata1$log_income)
print(paste("Maximum log_income:", max_log_income))

# Mean
mean_log_income <- mean(newdata1$log_income)
print(paste("Mean log_income:", mean_log_income))

# Median
median_log_income <- median(newdata1$log_income)
print(paste("Median log_income:", median_log_income))

# Mode (Custom function as R doesn't have a built-in mode function)
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mode_log_income <- get_mode(newdata1$log_income)
print(paste("Mode log-income:", mode_log_income))


# Load necessary library
install.packages("dplyr") # If not already installed
library(dplyr)

# question 4

# Summary statistics function
summary_stats <- function(df, col) {
  list(
    Min = min(df[[col]]),
    Max = max(df[[col]]),
    Mean = mean(df[[col]]),
    Median = median(df[[col]]),
    SD = sd(df[[col]])
  )
}

# Compute summary statistics
prestige_stats <- summary_stats(newdata1, "prestige")
education_stats <- summary_stats(newdata1, "education")
log_income_stats <- summary_stats(newdata1, "log_income")

# Print results
print("Summary statistics for Prestige:")
print(prestige_stats)
print("Descriptive Justification:")
cat("Minimum:", prestige_stats$Min, "\n")
cat("Maximum:", prestige_stats$Max, "\n")
cat("Mean:", prestige_stats$Mean, "\n")
cat("Median:", prestige_stats$Median, "\n")
cat("Standard Deviation:", prestige_stats$SD, "\n")

print("Summary statistics for Education:")
print(education_stats)
print("Descriptive Justification:")
cat("Minimum:", education_stats$Min, "\n")
cat("Maximum:", education_stats$Max, "\n")
cat("Mean:", education_stats$Mean, "\n")
cat("Median:", education_stats$Median, "\n")
cat("Standard Deviation:", education_stats$SD, "\n")

print("Summary statistics for log_Income:")
print(income_stats)
print("Descriptive Justification:")
cat("Minimum:", income_stats$Min, "\n")
cat("Maximum:", income_stats$Max, "\n")
cat("Mean:", income_stats$Mean, "\n")
cat("Median:", income_stats$Median, "\n")
cat("Standard Deviation:", income_stats$SD, "\n")


### question 5

library(ggplot2)
library(rlang)  # Needed for tidy evaluation with aes()

# Step 2: Recalculate central tendency and standard deviation for log-income
mean_log_income <- mean(newdata1$log_income, na.rm = TRUE)
median_log_income <- median(newdata1$log_income, na.rm = TRUE)
mode_log_income <- as.numeric(names(sort(-table(newdata1$log_income)))[1])

sd_log_income <- sd(newdata1$log_income, na.rm = TRUE)


# Step 3: Bell curve plot function with updated ggplot2 syntax
plot_bell_curve <- function(data, variable, mean_val, sd_val, color, fill_color, title, x_label) {
  ggplot(data, aes(x = !!sym(variable))) +  # Tidy evaluation for dynamic variable names
    stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), color = color, linewidth = 1) +
    stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), 
                  geom = "area", fill = fill_color, alpha = 0.3) +
    geom_vline(aes(xintercept = mean_val), color = "red", linetype = "dashed") +
    annotate("text", x = mean_val, y = 0.02, label = paste("Mean =", round(mean_val, 2)), color = "red") +
    geom_vline(aes(xintercept = median(data[[variable]], na.rm = TRUE)), color = "black", linetype = "dashed") +
    annotate("text", x = median(data[[variable]], na.rm = TRUE), y = 0.015, label = paste("Median =", round(median(data[[variable]], na.rm = TRUE), 2)), color = "black") +
    labs(title = title, x = x_label, y = "Density") +
    theme_minimal()
}



# Step 4: Plot the bell curve for log-income
bell_curve_log_income <- plot_bell_curve(newdata1, "log_income", mean_log_income, sd_log_income, "yellow", "yellow", "Bell Curve for Log Income", "Log(Income)")

# Step 5: Plot the log-income graph
bell_curve_log_income


install.packages("rlang")

# Step 2: Central Tendency Analysis
mean_prestige <- mean(newdata1$prestige, na.rm = TRUE)
median_prestige <- median(newdata1$prestige, na.rm = TRUE)
mode_prestige <- as.numeric(names(sort(-table(newdata1$prestige)))[1])

mean_education <- mean(newdata1$education, na.rm = TRUE)
median_education <- median(newdata1$education, na.rm = TRUE)
mode_education <- as.numeric(names(sort(-table(newdata1$education)))[1])


# Step 3: Standard deviation calculations
sd_prestige <- sd(newdata1$prestige, na.rm = TRUE)
sd_education <- sd(newdata1$education, na.rm = TRUE)

# Step 4: Bell curve plot function
plot_bell_curve <- function(newdata1, variable, mean_val, sd_val, color, fill_color, title, x_label) {
  ggplot(newdata1, aes(x = !!sym(variable))) +
    stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), color = color, linewidth = 1) +  # Use linewidth instead of size
    geom_area(stat = "function", fun = dnorm, args = list(mean = mean_val, sd = sd_val), fill = fill_color, alpha = 0.3) +
    geom_vline(aes(xintercept = mean_val), color = "red", linetype = "dashed") +
    annotate("text", x = mean_val, y = 0.02, label = paste("Mean =", round(mean_val, 2)), color = "red") +
    geom_vline(aes(xintercept = median(newdata1[[variable]], na.rm = TRUE)), color = "black", linetype = "dashed") +
    annotate("text", x = median(newdata1[[variable]], na.rm = TRUE), y = 0.015, label = paste("Median =", round(median(newdata1[[variable]], na.rm = TRUE), 2))) +
    labs(title = title, x = x_label, y = "Density") +
    theme_minimal()
}



# Step 5: Plot the bell curves
Bell_curve_education <- plot_bell_curve(newdata1, "education", mean_education, sd_education, "pink", "pink", "Bell Curve for Education", "Years of Education")
Bell_curve_prestige <- plot_bell_curve(newdata1, "prestige", mean_prestige, sd_prestige, "blue", "blue", "Bell Curve for Prestige", "Prestige Score")

# Step 6: Plot all graphs
Bell_curve_education
Bell_curve_prestige







# question 6

# Conduct ANOVA to test if there's a significant difference in prestige based on type of occupation
anova_result <- aov(prestige ~ type, data  = newdata1)

# Summary of the ANOVA result
summary(anova_result)

# Post-hoc analysis (if ANOVA is significant)
# Tukey's Honest Significant Difference (HSD) test
post_hoc <- TukeyHSD(anova_result)

# Print the results
print(post_hoc)


ggplot(newdata1, aes(x = type, y = prestige, fill = type)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Prestige by Occupation Type", x = "Occupation Type", y = "Prestige") +
  theme_minimal()


# Correlation test (Pearson's correlation)
cor_test <- cor.test(newdata1$prestige, newdata1$education, method = "pearson")

# Print the correlation test result
print(cor_test)

# Conduct linear regression to model the relationship
lm_model <- lm(prestige ~ education, data = newdata1)

# Summary of the linear regression model
summary(lm_model)

# Correlation test (Pearson's correlation)
cor_test_log_income <- cor.test(newdata1$prestige, newdata1$log_income, method = "pearson")

# Print the correlation test result
print(cor_test_log_income)

# Conduct linear regression to model the relationship between prestige and income
lm_log_income_model <- lm(prestige ~ log_income, data = newdata1)

# Summary of the linear regression model
summary(lm_log_income_model)


# Correlation test (Pearson's correlation)
cor_test_women <- cor.test(newdata1$prestige, newdata1$women, method = "pearson")

# Print the correlation test result
print(cor_test_women)

# Conduct linear regression to model the relationship between prestige and percentage of women
lm_women_model <- lm(prestige ~ women, data = newdata1)

# Summary of the linear regression model
summary(lm_women_model)



#question 7
# Perform Pearson correlation test
cor_test <- cor.test(newdata1$prestige, newdata1$education)

# Print correlation test results
print("Pearson Correlation Test Results:")
print(cor_test)

# Scatter plot with regression line
ggplot(newdata1, aes(x = education, y = prestige)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Prestige vs Education", x = "Education", y = "Prestige") +
  theme_minimal()



# question8
# Perform Pearson correlation test
cor_test <- cor.test(newdata1$prestige, newdata1$log_income)

# Print correlation test results
print("Pearson Correlation Test Results:")
print(cor_test)

# Scatter plot with regression line
ggplot(newdata1, aes(x = log_income, y = prestige)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Prestige vs Income", x = "log_Income", y = "Prestige") +
  theme_minimal()



# question 9
# Perform Pearson correlation test
cor_test <- cor.test(newdata1$prestige, newdata1$women)

# Print correlation test results
print("Pearson Correlation Test Results:")
print(cor_test)

# Scatter plot with regression line
ggplot(newdata1, aes(x = women, y = prestige)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Prestige vs  Women", x = " Women", y = "Prestige") +
  theme_minimal()




