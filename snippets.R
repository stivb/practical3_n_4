t.test(dfmw$gbpfood~ dfmw$gender)# for normal data  
wilcox.test(dfmw$gbpfood ~ dfmw$gender)# for non parametric data  
boxplot(dfmw$gbpfood ~ dfmw$gender, data = mtcars, xlab = "Gender", ylab = "food expenditure per week", main = "Money Spent on Food Men vs Women")  


t.test(dfmw$gbptransport~  dfmw$gender)# for normal data  
wilcox.test(dfmw$gbptransport ~ dfmw$gender)# for non parametric data  
boxplot(dfmw$gbptransport ~ dfmw$gender, data = mtcars, xlab = "Gender", ylab = "transport expenditure per week", main = "Money Spent on Transport Men vs Women")  

t.test(dfmw$pe_minutes~  dfmw$gender)# for normal data  
wilcox.test(dfmw$pe_minutes ~ dfmw$gender)# for non parametric data  
boxplot(dfmw$pe_minutes ~ dfmw$gender, data = mtcars, xlab = "Gender", ylab = "Time in Gym Minutes", main = "Physical Exercise Men vs Women")  

df<-subset(df,pe_minutes < 500)

t.test(df$pe_minutes ~ df$isAsian)# for normal data  
wilcox.test(df$pe_minutes ~ df$isAsian)# for non parametric data  
boxplot(df$pe_minutes ~ df$isAsian, data = mtcars, xlab = "Provenance", ylab = "Minutes in Gym", main = "Weekly exercise")  

t.test(df$miles ~ df$isAsian)# for normal data  
wilcox.test(df$miles ~ df$isAsian)# for non parametric data  
boxplot(df$miles ~ df$isAsian, data = mtcars, xlab = "Provenance", ylab = "Miles to campus", main = "Miles to campus ")  

dfmw<-subset(df,gender=="Man" | gender=="Woman")
t.test(dfmw$miles ~ dfmw$gender)# for normal data  
wilcox.test(dfmw$miles ~ dfmw$gender)# for non parametric data  
boxplot(dfmw$miles ~ dfmw$gender, data = mtcars, xlab = "Gender", ylab = "Miles to campus", main = "Miles to campus ")  