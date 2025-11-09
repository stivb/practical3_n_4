library(readr)
library(readxl)
df <- read_excel("7COM1079_survey.xlsx", col_names = TRUE)

colnames(df)[1] <- "id"
colnames(df)[2] <- "start_time"
colnames(df)[3] <- "end_time"
colnames(df)[4] <- "person"
colnames(df)[5] <- "name"
colnames(df)[6] <- "award"
colnames(df)[7] <- "continent"
colnames(df)[8] <- "gender"
colnames(df)[9] <- "height_cm"
colnames(df)[10] <- "minutes_exercise"
colnames(df)[11] <- "miles_from_campus"
colnames(df)[12] <- "spending_on_transport"
colnames(df)[13] <- "spending_on_rent"
colnames(df)[14] <- "time_to_campus"
colnames(df)[15] <- "spending_on_food"
colnames(df)[16] <- "hometown_population"
colnames(df)[17] <- "ptjobs"


# The as.numeric function in R is used to convert an object to a numeric (double) type.
# This is particularly useful when you have data stored as characters or factors and need to perform mathematical operations.
# For example, if you have a vector of numbers stored as strings, as.numeric will convert them to actual numeric values.
# Note: If the conversion is not possible (e.g., non-numeric characters), R will return NA for those elements.
# Usage example: as.numeric("123") returns 123 as a numeric value.
df$height_cm <- as.numeric(df$height_cm)
df$minutes_exercise <- as.numeric(df$minutes_exercise)
df$miles_from_campus <- as.numeric(df$miles_from_campus)
df$spending_on_transport <- as.numeric(df$spending_on_transport)
df$spending_on_rent <- as.numeric(df$spending_on_rent)
df$time_to_campus <- as.numeric(df$time_to_campus)
df$spending_on_food <- as.numeric(df$spending_on_food)
df$hometown_population <- as.numeric(df$hometown_population)

#PART 1: COMPARISON OF MEANS OF HEIGHT AMONG MEN AND WOMEN

#comparison of means, do men and women differ in height?
df2 <- subset(df, gender == "Man" | gender == "Woman")

hist(df2$height_cm, main = "Histogram of Heights", xlab = "Height (cm)")
shapiro_test_result <- shapiro.test(df2$height_cm)
print(shapiro_test_result)


# Perform t-test
height_t_test_result <- t.test(height_cm ~ gender, data = df2)
print(height_t_test_result)

# Visualize height difference by gender with boxplot
boxplot(height_cm ~ gender, data = df2,
        main = "Height Distribution by Gender",
        xlab = "Gender",
        ylab = "Height (cm)",
        col = c("lightblue", "lightpink"))

# comparison of means, do men and women spend differently on food on food
# Remove outliers

#PART 2: COMPARISON OF MEANS OF WEEKLY FOOD EXPENDITURE BY GENDER
df2 <- subset(df, spending_on_food < 100)
df2 <- subset(df2, gender == "Man" | gender == "Woman")

hist(df2$spending_on_food, main = "Histogram of Spending on Food", xlab = "Spending on Food (£)")
# Check if spending_on_food is normally distributed
shapiro_test_result <- shapiro.test(df2$spending_on_food)
print(shapiro_test_result)
#less than zero so not normal
#do a wilcoxon test instead
food_gender_wilcox_test_result <- wilcox.test(spending_on_food ~ gender, data = df2)
print(food_gender_wilcox_test_result)

# Perform t-test
food_gender_t_test_result <- t.test(spending_on_food ~ gender, data = df2)
print(food_gender_t_test_result)

# visualize spending on food difference by gender with boxplot
boxplot(spending_on_food ~ gender, data = df2,
        main = "Spending on Food by Gender",
        xlab = "Gender  ",
        ylab = "Spending on Food (£)",                  
        col = c("lightblue", "lightpink"))



#PART 2.5: COMPARISON OF MEANS OF hometown_population BY GENDER

df2 <- subset(df, hometown_population < 10000)
df2 <- subset(df2, gender == "Man" | gender == "Woman")
hist(df2$hometown_population, main = "Histogram of Hometown Population", xlab = "Hometown Population")
shapiro_test_result <- shapiro.test(df2$hometown_population)
print(shapiro_test_result)
#less than zero so not normal

gender_hometownpop_wilcox_test_result <- wilcox.test(hometown_population ~ gender, data = df2)
print(gender_hometownpop_wilcox_test_result)


#PART 3: CORRELATION BETWEEN DISTANCE TO CAMPUS AND TRANSPORT COSTS

# is there a correlation between spending on transport and miles from campus?
# Remove unrealistic values
df2 <- subset(df, miles_from_campus < 100 & spending_on_transport < 150)
# Check the data - is it normal or not
hist(df2$spending_on_transport, main = "Histogram of Transport Costs", xlab = "Spending on Transport (£)")
# Here is the test
dist_transport_cor_test_result <- cor.test(df2$miles_from_campus, df2$spending_on_transport, method = "spearman") # not normal so using spearman
print(dist_transport_cor_test_result)
# Plot followed by trend line
plot(df2$miles_from_campus, df2$spending_on_transport,
     xlab = "Miles to Campus",
     ylab = "Weekly Transport Cost (£)",
     main = "Scatterplot of Distance to Campus vs Transport Cost")
abline(lm(df2$spending_on_transport ~ df2$miles_from_campus), col = "red")
# is there a correlation between spending on food and the amount of exercise one does
# Remove anyone who spends more than £150 pw on food, and who exercises more than 500 minutes

#PART 4: CORRELATION BETWEEN FOOD EXPENDITURE AND EXERCISE DURATION
df2 <- subset(df, spending_on_food < 150 & minutes_exercise < 500)
# Is food expenditure normal?
hist(df2$spending_on_food, main = "Histogram of Food Spending", xlab = "Spending on Food (£)")  
# Are minutes in gym normal?
hist(df2$minutes_exercise, main = "Histogram of Exercise Minutes", xlab = "Minutes of Exercise per Week")
# Food expenditure is kind of normal - so I will go with it here    
food_exercise_cor_test_result <- cor.test(df2$minutes_exercise, df2$spending_on_food, method = "pearson")
print(food_exercise_cor_test_result)
# Now plot it
plot(df2$minutes_exercise, df2$spending_on_food,
     xlab = "Minutes of Exercise per Week",
     ylab = "Amount Spent on Food per Week (£)",
     main = "Scatterplot of Exercising Duration vs Food Expenditure")
abline(lm(df2$spending_on_food ~ df2$minutes_exercise), col = "red")



#PART 5: COMPARISON OF PROPORTIONS OF JOB SITUATION BY GENDER

#now do a comparison of proportions like in Comparison_Of_Proportions.R
#removing non-binary and "prefer not to say" because too few items (not because of bigotry!)
#creating a new dataset (df2) after filtering
df2 <- subset(df, gender == "Man" | gender == "Woman")  
# Create contingency table for ptjobs by gender
ptjobsbygender_contingency_table <- table(df2$gender, df2$ptjobs)
print(ptjobsbygender_contingency_table)

# Perform chi-squared test
jobs_gender_chi_test_result <- chisq.test(ptjobsbygender_contingency_table)
print(jobs_gender_chi_test_result)

# Create stacked bar chart
barplot(ptjobsbygender_contingency_table,
        main = "Part-time Jobs by Gender",
        xlab = "Part-time Jobs",
        ylab = "Frequency",
        col = c("lightblue", "lightpink"),
        legend = rownames(ptjobsbygender_contingency_table),
        beside = FALSE)

#PART 6: COMPARISON OF PROPORTIONS OF CITY SIZE BY AWARD

df$citysize <- ifelse(df$hometown_population <= 1000, "small",
                                ifelse(df$hometown_population > 10000, "mega",
                                ifelse(df$hometown_population > 1000 & df$hometown_population <= 10000, "large", NA)))
df$citysize <- factor(df$citysize, levels = c("small","large","mega"))

# Create contingency table for citysize by award
citysize_award_contingency_table <- table(df$citysize, df$award)
print(citysize_award_contingency_table)
# Perform chi-squared test
citysize_award_chi_test_result <- chisq.test(citysize_award_contingency_table)
print(citysize_award_chi_test_result)
# Create stacked bar chart
barplot(citysize_award_contingency_table,
        main = "City Size by Award",
        xlab = "Award",
        ylab = "Frequency",
        col = c("lightgreen", "lightblue", "lightpink"),
        legend = rownames(citysize_award_contingency_table),
        beside = FALSE)
#now running a transpose over the counts table (basically turning rows into columns and columns into rows)
#that is to say changing a 3 bars of citysize
#into three bars (citysize) of 5 awards each
tpercentages <- prop.table(t(citysize_award_contingency_table), margin = 2) * 100
#now running a transpose over the counts table (basically turning rows into columns and columns into rows)
#that is to say changing a 3 bars of citysize
#into three bars (citysize) of 5 awards each
tpercentages <- prop.table(t(citysize_award_contingency_table), margin = 2) * 100      
#now printing out the stacked barchart
barplot(tpercentages, col = c("red", "green", "blue"), xlab = "Awards", ylab = "Percentage",
        main = "Stacked Bar Of Awards by City Size", ylim = c(0, 100),
        legend.text = rownames(tpercentages), args.legend = list(x = "topright"))       


print("Gender Height t-test Result:")
print(height_t_test_result)
meanmen<-mean(df2$height_cm[df2$gender=="Man"], na.rm=TRUE)
meanwomen<-mean(df2$height_cm[df2$gender=="Woman"], na.rm=TRUE)
tvalue<-height_t_test_result$statistic
pvalue<-height_t_test_result$p.value


print("Food Gender Wilcoxon Test Result:")
print(food_gender_wilcox_test_result)
medianmen_food<-median(df2$spending_on_food[df2$gender=="Man"], na.rm=TRUE)
medianwomen_food<-median(df2$spending_on_food[df2$gender=="Woman"], na.rm=TRUE)
wilcoxvalue<-food_gender_wilcox_test_result$statistic
pvalue_food_gender_wilcox<-food_gender_wilcox_test_result$p.value  

print("Food Gender t-test Result:")
print(food_gender_t_test_result)
meanmen_food<-mean(df2$spending_on_food[df2$gender=="Man"], na.rm=TRUE)
meanwomen_food<-mean(df2$spending_on_food[df2$  gender=="Woman"], na.rm=TRUE)
tvalue_food<-food_gender_t_test_result$statistic
pvalue_food<-food_gender_t_test_result$p.value

print("Gender Hometown Population Wilcoxon Test Result:")
print(gender_hometownpop_wilcox_test_result)
medianmen_pop<-median(df2$hometown_population[df2$gender=="Man"], na.rm=TRUE)
medianwomen_pop<-median(df2$hometown_population[df2$gender=="Woman"], na.rm=TRUE)       
wilcoxvalue_pop<-gender_hometownpop_wilcox_test_result$statistic
pvalue_pop<-gender_hometownpop_wilcox_test_result$p.value

print("Distance to Transport Correlation Test Result:")
print(dist_transport_cor_test_result)   
cor_dist_transport<-dist_transport_cor_test_result$estimate
pvalue_dist_transport<-dist_transport_cor_test_result$p.value

print("Food Exercise Correlation Test Result:")
print(food_exercise_cor_test_result)
cor_food_exercise<-food_exercise_cor_test_result$estimate
pvalue_food_exercise<-food_exercise_cor_test_result$p.value

print("Jobs Gender Chi-squared Test Result:")
print(jobs_gender_chi_test_result)
chiValue_jobs<-jobs_gender_chi_test_result$statistic
pvalue_jobs<-jobs_gender_chi_test_result$p.value

print("City Size Award Chi-squared Test Result:") 
chiValue_citysize_award<-citysize_award_chi_test_result$statistic
pvalue_citysize_award<-citysize_award_chi_test_result$p.value      
print(citysize_award_chi_test_result)
print("End of Analysis")

###############################################################
###MAKE QTI QUIZ QUESTIONS BASED ON THE ABOVE RESULTS##########
##based on the https://github.com/gpoore/text2qti library###########
##make numerical questions for the various means, medians, corrs, chisq, ttests etc
# Create text2qti formatted questions (text strings for Python processing)
cat("\n\n### TEXT2QTI QUIZ QUESTIONS ###\n\n")

# Question 1: Mean Height of Men
cat("1. What is the mean height of men (in cm)?\n")
cat("=", round(meanmen, 2), "+-", 0.5, "\n\n")

# Question 2: Mean Height of Women
cat("2. What is the mean height of women (in cm)?\n")
cat("=", round(meanwomen, 2), "+-", 0.5, "\n\n")

# Question 3: T-value from Height T-test
cat("3. What is the t-value from the height t-test?\n")
cat("=", round(as.numeric(tvalue), 2), "+-", 0.05, "\n\n")

# Question 4: P-value from Height T-test
cat("4. What is the p-value from the height t-test?\n")
cat("=", round(as.numeric(pvalue), 4), "+-", 0.0001, "\n\n")

# Question 5: Median Food Spending of Men
cat("5. What is the median food spending of men (in £)?\n")
cat("=", round(medianmen_food, 2), "+-", 0.5, "\n\n")

# Question 6: Median Food Spending of Women
cat("6. What is the median food spending of women (in £)?\n")
cat("=", round(medianwomen_food, 2), "+-", 0.5, "\n\n")

# Question 7: Wilcoxon Statistic from Food Gender Test
cat("7. What is the Wilcoxon statistic from the food gender Wilcoxon test?\n")
cat("=", round(as.numeric(wilcoxvalue), 2), "+-", 1, "\n\n")

# Question 8: Correlation Coefficient Distance to Transport
cat("8. What is the Spearman correlation coefficient between distance to campus and transport costs?\n")
cat("=", round(as.numeric(cor_dist_transport), 3), "+-", 0.01, "\n\n")

# Question 9: P-value from Distance Transport Correlation
cat("9. What is the p-value from the distance to transport correlation test?\n")
cat("=", round(as.numeric(pvalue_dist_transport), 4), "+-", 0.0001, "\n\n")

# Question 10: Correlation Food Exercise
cat("10. What is the Pearson correlation coefficient between food expenditure and exercise duration?\n")
cat("=", round(as.numeric(cor_food_exercise), 3), "+-", 0.01, "\n\n")

# Question 11: P-value Food Exercise Correlation
cat("11. What is the p-value from the food expenditure and exercise correlation test?\n")
cat("=", round(as.numeric(pvalue_food_exercise), 4), "+-", 0.0001, "\n\n")

# Question 12: Chi-squared Statistic Jobs Gender
cat("12. What is the chi-squared statistic from the jobs gender chi-squared test?\n")
cat("=", round(as.numeric(chiValue_jobs), 2), "+-", 0.1, "\n\n")

# Question 13: P-value Jobs Gender Chi-squared
cat("13. What is the p-value from the jobs gender chi-squared test?\n")
cat("=", round(as.numeric(pvalue_jobs), 4), "+-", 0.0001, "\n\n")

# Question 14: Chi-squared Statistic City Size Award
cat("14. What is the chi-squared statistic from the city size award chi-squared test?\n")
cat("=", round(as.numeric(chiValue_citysize_award), 2), "+-", 0.1, "\n\n")

# Question 15: P-value City Size Award Chi-squared
cat("15. What is the p-value from the city size award chi-squared test?\n")
cat("=", round(as.numeric(pvalue_citysize_award), 4), "+-", 0.0001, "\n\n")
# End of practical_3_starter.R
