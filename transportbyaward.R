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

df2<-subset(df,get2campusminutes<155)
df2$halfhours<-floor(df2$get2campusminutes/30)
pt <- table(df2$halfhours,df2$award)  
chisq.test(pt)
colnames(pt) = c("Adv CS", "AI", "CyberSec", "Data Sci.", "Soft Eng.")
random_colors <- sample(colors(), 10)
percentages<-prop.table(pt, margin=2) * 100 
lbls = paste0(as.character(as.numeric(rownames(pt)) * 30), "m-", as.character((as.numeric(rownames(pt))+1) * 30), "m")
barplot(percentages, col = random_colors, xlab = "Awards", ylab = "Percentage", 
        main = "Time to Campus (in half hour block)", ylim = c(0, 100), 
        legend.text = lbls, args.legend = list(x = "topright"))  