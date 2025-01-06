
####### 
## Bryant Willoughby 
#######

### read in data 
carsdata <- read.table("auto-mpg.data", na.strings = "?")

### data cleaning 

#name variables appropriately
names(carsdata) <- c("mpg", "cylinders", "displacement", "horsepower", 
                     "weight", "acceleration", "modelyear", "origin", 
                     "carname")

#check for missing data; remove records with at least one missing value
anyNA(carsdata)
dim(carsdata)
carsdata <- na.omit(carsdata)
dim(carsdata)

#write data file to use in subsequent .R files
write.csv(carsdata, file = "cleancarsdata.csv", row.names = F)

### EDA 

## Graphical Summaries

#continuous variables
datasubset <- carsdata[c("mpg", "displacement", "horsepower", "weight", 
                         "acceleration")]

pdf("EDA_Histograms.pdf")
par(mfrow = c(2,3))
for (i in 1:ncol(datasubset)) {
  hist(datasubset[, i], main = names(datasubset)[i], prob = T, 
       xlab = "")
  lines(density(datasubset[, i]), col = "blue")
}
dev.off()

# for use in paper
pdf("EDA_Histogram.pdf")
par(mfrow = c(1,1))
hist(datasubset$mpg, main = names(datasubset)[1], prob = T, 
     xlab = "", cex.main = 1.5)
lines(density(datasubset$mpg), col = "blue")
dev.off()


pdf("EDA_Boxplots.pdf")
par(mfrow = c(2,3))
for (i in 1:ncol(datasubset)) {
  boxplot(datasubset[, i], main = names(datasubset)[i])
  abline(h = apply(datasubset, 2, mean)[i], col = "blue")
}
dev.off()

#for use in paper
pdf("EDA_Boxplot.pdf")
par(mfrow = c(1,1))
boxplot(datasubset$mpg, main = names(datasubset)[1], 
        cex.main = 1.5)
abline(h = apply(datasubset, 2, mean)[1], col = "blue")
dev.off()


#multi-valued discrete 
datasubset2 <- carsdata[c("cylinders", "modelyear", "origin")]

pdf("EDA_Barplots.pdf")
par(mfrow = c(3,1))
for (i in 1:ncol(datasubset2)){
  barplot(table(datasubset2[, i]), main = names(datasubset2)[i])
}
dev.off()

# for use in paper
pdf("EDA_Barplot.pdf")
par(mfrow = c(1,1))
barplot(table(datasubset2$cylinders), main = names(datasubset2)[1],
        cex.main = 1.5)
dev.off()

## Numerical Summaries
cat("Summary for continuous variables:\n")
print(summary(datasubset))
cat("\nSummary for multi-valued discrete variables:\n")
for (var in c("cylinders", "modelyear", "origin")) {
  cat("Variable:", var, "\n")
  print(table(datasubset2[[var]]))
  cat("\n")
}

### variables of interest exploration 

cormat <- cor(datasubset[c("mpg", "weight", "horsepower", 
                           "displacement", "acceleration")])
print(cormat)

pdf("correlation_matrix.pdf")
pairs(datasubset[c("mpg", "weight", "horsepower", 
                   "displacement", "acceleration")])
dev.off()

## Fit the model   
model1 <- lm(mpg ~ ., datasubset)
summary(model1)

model2 <- lm(mpg ~ horsepower + weight, datasubset)
summary(model2)

#general linear test 
anova(model2, model1)

#model with two predictors
summary(model2)

cormat <- cor(datasubset[c("mpg", "weight", "horsepower")])
print(cormat)

pairs(datasubset[c("mpg", "weight", "horsepower")])

## model checking (diagnostics)
par(mfrow = c(3,1))
#non-linear and non-constant variance check
plot(model2$fitted.values, model2$residuals, main = "Residuals vs. fits", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "blue")
#Independence of Errors (Index Plot)
plot(model2$residuals, ylab = "Residuals", main = "Residual time sequence plot")
abline(h = 0, col = "blue")
#normality of errors
qqnorm(model2$residuals)
qqline(model2$residuals)
#visual evidence of non-constant variance/non-normality of errors

#for paper 
par(mfrow = c(1,1))

pdf("resid_fits1.pdf")
plot(model2$fitted.values, model2$residuals, main = "Residuals vs. fits", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "blue")
dev.off()

pdf("Index1.pdf")
plot(model2$residuals, ylab = "Residuals", main = "Residual time sequence plot")
abline(h = 0, col = "blue")
dev.off()

pdf("QQ1.pdf")
qqnorm(model2$residuals)
qqline(model2$residuals)
dev.off()


#transform y-values; note: this is ln() = log_e; this is my final model
model4 <- lm(log(mpg) ~ horsepower + weight, datasubset)
summary(model4)

## model checking (diagnostics)
par(mfrow = c(3,1))
#non-linear and non-constant variance check
plot(model4$fitted.values, model4$residuals, main = "Residuals vs. fits", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "blue")
#Independence of Errors (Index Plot)
plot(model4$residuals, ylab = "Residuals", main = "Residual time sequence plot")
abline(h = 0, col = "blue")
#normality of errors
qqnorm(model4$residuals)
qqline(model4$residuals)
#visual evdidence of an improvement in diagnostics & performance metrics 


#for paper 
par(mfrow = c(1,1))

pdf("resid_fits2.pdf")
plot(model4$fitted.values, model4$residuals, main = "Residuals vs. fits", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "blue")
dev.off()

pdf("Index2.pdf")
plot(model4$residuals, ylab = "Residuals", main = "Residual time sequence plot")
abline(h = 0, col = "blue")
dev.off()

pdf("QQ2.pdf")
qqnorm(model4$residuals)
qqline(model4$residuals)
dev.off()

### results & implications 
summary(model4)

# 95% simultaneous confidence interval for horsepower and weight coefficient
n <- nrow(datasubset)
p <- 3
alpha <- 0.05  
tcrit <- qt(1 - alpha/4, n - p) 

# Get the number of predictors
num_predictors <- length(model4$coefficients) - 1  # excluding Intercept

# Loop through each predictor
for (i in 2:(num_predictors + 1)) {
  se <- summary(model4)$coefficients[i, 2]
  LB <- exp(summary(model4)$coefficients[i, 1] - tcrit * se)
  UB <- exp(summary(model4)$coefficients[i, 1] + tcrit * se)
  
  cat("Confidence Interval for", names(model4$coefficients)[i], "coefficient:", LB, "to", UB, "\n")
}

#final general linear test check
model5 <- lm(log(mpg) ~ weight, datasubset)
model6 <- lm(log(mpg) ~ horsepower, datasubset)

anova(model5, model4)
anova(model6, model4)


