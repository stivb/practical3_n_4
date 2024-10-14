#import data
library(readr)
survey_data <- read_csv("survey_data.csv")
#rename dataset to make it easier to reference
df<-survey_data
#rename columns to make them shorter
names(df)[7] <- "award"
names(df)[8] <- "gender"
names(df)[9] <- "height"
names(df)[10] <- "continent"
names(df)[11] <- "pe_minutes"
names(df)[12] <- "miles"
names(df)[13] <- "gbptransport"
names(df)[14] <- "get2campusminutes"
names(df)[15] <- "gbpfood"


#removing non-binary and "prefer not to say" because too few items (not because of bigotry!)
#creating a new dataset (df2) after filtering
df2<-subset(df,gender=="Man" | gender=="Woman")

#creating a table of counts gender/award - type pt into the console to see what it looks like
pt <- table(df2$gender,df2$award)  
chisq.test(pt) 

#renaming the columns of the table (pt) in order that they are shorter for when we do the graph
colnames(pt) = c("Adv CS", "AI", "CyberSec", "Data Sci.", "Soft Eng.")

#converting the counts into percentages of the total counts
percentages <- prop.table(pt, margin=2) * 100  

#now printing out the the proportions of men women inside each award
barplot(percentages, col = c("blue", "pink"), xlab = "Awards", ylab = "Percentage", 
                          main = "Stacked Bar Of Awards by Men v Women", ylim = c(0, 100), 
                          legend.text = c("Man", "Woman"), args.legend = list(x = "topright"))  

#now running a transpose over the counts table and turning that into percentages
tpercentages<-prop.table(t(pt), margin=2) * 100 

#now running a transpose over the counts table (basically turning rows into columns and columns into rows)
#that is to say changing a 5 bars of men/women
#into two bars (men/woman) of 5 awards each
barplot(tpercentages, col = c("red", "green", "yellow", "pink", "blue"), xlab = "Awards", ylab = "Percentage", 
        main = "Stacked Bar Of Awards by Men v Women", ylim = c(0, 100), 
        legend.text = rownames(tpercentages), args.legend = list(x = "topright"))  

#########################################################################################################
##########now seeing if there are differences in travelling time of students in different awards#########

#filtering any result over 155 minutes (seems unrealistic) into new dataset df2
df2<-subset(df,get2campusminutes<155)

#creating a new column called "halfhours" which breaks distances travelled into groups of halfhours
df2$halfhours<-floor(df2$get2campusminutes/30) 

#creating a table of half hours by award
pt <- table(df2$halfhours,df2$award)  
chisq.test(pt)

#making names shorter so they appear in the graph
colnames(pt) = c("Adv CS", "AI", "CyberSec", "Data Sci.", "Soft Eng.")

#if you have lots of items in your bars you can use this to get the colors
random_colors <- sample(colors(), 10)

#converting the counts into percentages
percentages<-prop.table(pt, margin=2) * 100 

#a little routine to convert number of half hours to a more comprehensible range
lbls = paste0(as.character(as.numeric(rownames(pt)) * 30), "m-", as.character((as.numeric(rownames(pt))+1) * 30), "m")

#now printing out the stacked barchart
barplot(percentages, col = random_colors, xlab = "Awards", ylab = "Percentage", 
        main = "Time to Campus (in half hour block)", ylim = c(0, 100), 
        legend.text = lbls, args.legend = list(x = "topright"))  

#are the different gender balances between asian and non-asian students on the course

df2<-subset(df,gender=="Man" | gender=="Woman")
df2$isAsian<-ifelse(df2$continent=="Asia","Asia","Not Asia")

pt <- table(df2$gender,df2$isAsian)  
chisq.test(pt)
percentages<-prop.table(pt, margin=2) * 100 
barplot(percentages, col = random_colors, xlab = "Provenance", ylab = "Percentage", 
        main = "Gender balance in Asian and Non-Asian students", ylim = c(0, 100), 
        legend.text = rownames(pt), args.legend = list(x = "topright"))  


