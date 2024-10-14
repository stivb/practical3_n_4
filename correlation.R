library(readr)
survey_data <- read_csv("survey_data.csv")
df<-survey_data
names(df)[7] <- "award"
names(df)[8] <- "gender"
names(df)[9] <- "height"
names(df)[10] <- "continent"
names(df)[11] <- "pe_minutes"
names(df)[12] <- "miles"
names(df)[13] <- "gbptransport"
names(df)[14] <- "get2campusminutes"
names(df)[15] <- "gbpfood"


#if there a correlation between the amount spent on transport and distance from uni?
#firstly get rid of unrealistiv values
df2<-subset(df,miles<100 & gbptransport<150)

#check the data - is it normal or not
hist(df2$gbptransport, main = "Histogram of Transport Costs")

#here is the test
cor.test(df2$miles, df2$gbptransport, method="spearman")   #not normal so using spearman

# Plot followed by trend line
plot(df2$miles, df2$gbptransport, xlab = "Miles to Campus", ylab = "Weekly Transport Cost £s", main = "Scatterplot of Distance to Campus vs Transport Cost")  
abline(lm(df2$gbptransport ~ df2$miles), col = "red") 
 


#if there a correlation between spending on food and the amount of exercise one does
#get rid of anyone who spends more than £150 pw on food, and who exercses more than 500 minutes
df2<-subset(df,gbpfood<150 & pe_minutes <500)
#is food expenditure normal
hist(df2$gbpfood, main = "Histogram of Food Costs")

#are minutes in gym normal?
hist(df2$pe_minutes)

#food expenditure is kind of normal - so I will go with it here
cor.test(df2$pe_minutes, df2$gbpfood, method="pearson")

#now plot it
plot(df2$pe_minutes, df2$gbpfood, xlab = "Minutes of exercise per week", ylab = "Amount spent on food per week", main = "Scatterplot of Exercising Duration vs Food Expenditure")  
# Add a trend line  
abline(lm(df2$gbpfood ~ df2$pe_minutes), col = "red") 




