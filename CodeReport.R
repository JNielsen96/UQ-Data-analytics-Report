athlete = read.csv("Data.csv")
library(lattice)
athlete$Balance_Diff = athlete$BTECB - athlete$BTECA
str(athlete)
bwplot(Balance_Diff ~ Alcohol | Sex, data=athlete)
athlete$Sex = factor(athlete$Sex)
xyplot(Balance_Diff ~ Sex, data=athlete, group=Alcohol, type=c("p", "a"), auto.key=TRUE)
aggregate(Balance_Diff ~ Alcohol*Sex, data=athlete, mean)
anova_results <- aov(Balance_Diff ~ Alcohol * Sex, data=athlete)
summary(anova_results)
plot(anova_results, which=2)

athlete = read.csv("Data.csv")
library(lattice)
athlete$React_Diff = athlete$RTB - athlete$RTA
str(athlete)
bwplot(React_Diff ~ Alcohol | Sex, data=athlete, main="Reaction Times by Alcohol and Sex",
       xlab="Groups", ylab="Reaction Time Difference")
athlete$Sex = factor(athlete$Sex)
xyplot(React_Diff ~ Sex, data=athlete, group=Alcohol, type=c("p", "a"), 
       auto.key=list(columns=2, title="Alcohol Consumption", cex=0.8, points=TRUE, lines=TRUE),
       xlab="Sex", ylab="Reaction Time Difference",
       main="Interaction Plot: Reaction Times by Sex and Alcohol Consumption",
       par.settings=list(superpose.symbol=list(pch=c(16, 17), col=c("blue", "orange")),
                         superpose.line=list(col=c("blue", "orange"), lty=1:2)),
       grid=TRUE)
aggregate(React_Diff ~ Alcohol*Sex, data=athlete, mean)
anova_results = aov(React_Diff ~ Alcohol * Sex, data=athlete)
summary(anova_results)

plot(anova_results, which=2)
qqnorm(residuals(anova_results), main="Q-Q Plot of Residuals", 
       xlab="Theoretical Quantiles", ylab="Standardized Residuals", pch=16)
qqline(residuals(anova_results), col="blue", lwd=2)

plot(anova_results, which=1)
(1.566+1.211+0.46)/(1.566+1.211+0.46+20.275)
boxplot(React_Diff ~ Alcohol * Sex, data=athlete, main="Reaction Times by Alcohol and Sex",
        xlab="Groups", ylab="Reaction Time Difference")

interaction.plot(athlete$Alcohol, athlete$Sex, athlete$React_Diff,
                 col = c("red", "blue"), lty = 1:2, type = "b", pch = 16, xlab = "Alcohol",
                 ylab = "Mean Reaction Time Difference", main = "Interaction Plot: Alcohol and Sex")

interaction.plot(athlete$Alcohol, athlete$Sex, athlete$React_Diff,
                 col = c("red", "blue"), lty = 1:2, type = "b", pch = 16, 
                 xlab = "Alcohol", ylab = "Mean Reaction Time Difference", 
                 main = "Interaction Plot: Alcohol and Sex", 
                 legend = TRUE, legend.title = "Sex", legend.text = c("Female", "Male"))

hist(residuals(anova_results), main="Histogram of Residuals", xlab="Residuals", breaks=10)
hist(residuals(anova_results), breaks=10, col="gray", main="Histogram of Residuals", 
     xlab="Residuals", ylab="Frequency", freq=TRUE)
residuals_data = residuals(anova_results)
xfit = seq(min(residuals_data), max(residuals_data), length=40)
yfit = dnorm(xfit, mean=mean(residuals_data), sd=sd(residuals_data))
yfit = yfit * diff(hist(residuals_data, plot=FALSE)$mids[1:2]) * length(residuals_data)
lines(xfit, yfit, col="blue", lwd=2)


plot(anova_results, which=4)
lm_results = lm(React_Diff ~ Alcohol * Sex, data=athlete)
summary(lm_results)
summary_lm$r.squared
