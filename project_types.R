#histogram to show normalcy of data  
h <-hist(mtcars$mpg, breaks = 20, xlab = "Miles per Gallon", ylab = "Frequency", main = "Histogram of MPG Values")  
# Add a normal distribution line  
x <- seq(min(mtcars$mpg), max(mtcars$mpg), length = 100)  
y <- dnorm(x, mean = mean(mtcars$mpg), sd = sd(mtcars$mpg)) * length(mtcars$mpg)  
box.size <- diff(h$mids[1:2])   
y <- y * box.size  
lines(x, y, col = "red")  

#comparison of means  
t.test(mtcars$mpg ~ mtcars$am)# for normal data  
wilcox.test(mtcars$mpg ~ mtcars$am)# for non parametric data  
boxplot(mtcars$mpg ~ mtcars$am, data = mtcars, xlab = "Gear", ylab = "Miles per Gallon", main = "Boxplot of MPG by Gear")  

#comparison of proportion  
pt <- table(mtcars$cyl,mtcars$am)  
chisq.test(pt) 
percentages <- prop.table(pt, margin=2) * 100  
barplot(percentages, col = c("red", "blue", "yellow"), xlab = "Manual 0 vs Automatic 1", ylab = "Percentage", main = "Stacked Bar Chart of Cylinders by Transmission Type", ylim = c(0, 100), legend.text = c("4", "6", "8"), args.legend = list(x = "topright"))  


#correlation 
cor.test(mtcars$mpg, mtcars$hp)  
cor.test(mtcars$mpg, mtcars$hp, method="spearman")  
plot(mtcars$hp, mtcars$mpg, xlab = "Horsepower", ylab = "Miles per Gallon", main = "Scatterplot of MPG vs. Horsepower")  
# Add a trend line  
abline(lm(mpg ~ hp, data = mtcars), col = "red") 

