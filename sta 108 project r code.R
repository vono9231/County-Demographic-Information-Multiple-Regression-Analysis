1.43. a.
#plotting

# X = total population (V5), Y = number of active physicans (V8),
plot(CDI$V5, CDI$V8, xlab ='Total Population', ylab = 'Number of Active Physicans', main = 
'County Demographic Information')

# X = number of hospital beds (V9), Y = number of active physicans (V8)
plot(CDI$V9, CDI$V8, xlab = 'Number of Hospital Beds', ylab = 'Number of Active Physicans', main = 
'County Demographic Information')

# X = total personal income (V16), Y = number of active physicans (V8)
plot(CDI$V16, CDI$V8, xlab = 'Total Personal Income (in millions of dollars)', 
     ylab = 'Number of Active Physicans', main = 'County Demographic Information')


#Regression function plot 1 (total population)
fit1 = lm(CDI$V8 ~ CDI$V5)
abline(fit1, col ='red')
summary(fit1) # Y_hat = -110.6 + 0.002795 X_hat
plot(CDI$V5, fit1$residuals, xlab = 'Total Populaton', ylab = 'Residuals', 
main = 'Total Population and Residuals ')
abline(h=0, col ='red')

qqplot = qqnorm(fit1$residuals)
qqline(fit1$residuals, col ='red')

#Regression function plot 2 (hospital beds)
fit2 = lm(CDI$V8 ~ CDI$V9)
abline(fit2, col = 'red')
summary(fit2) # Y_hat = -95.93218 + 0.74312 X_hat
plot(CDI$V9, fit2$residuals, xlab = 'Number of Total Hospital Beds', ylab = 'Residuals',
main = 'Total Hospital Beds and Residuals')
abline(h=0, col = 'red')

qqplot = qqnorm(fit2$residuals)
qqline(fit2$residual, col = 'red')

#Regression function plot 3 (total personal income)
fit3 = lm(CDI$V8 ~ CDI$V16)
abline(fit3, col = 'red')
summary(fit3) #Y_hat = -48.39485 + 0.13170 X_hat
plot(CDI$V16, fit3$residuals, xlab = 'Total Personal Income', ylab ='Residuals', 
main = 'Total Personal Income and Residuals')
abline(h=0, col = 'red')

qqplot = qqnorm(fit3$residuals)
qqline(fit3$residuals, col = 'red')

#MSE of plot1
anova(fit1)
372204

#MSE of plot2
anova(fit2)
310192

#MSE of plot3
anova(fit3)
324539


#1.44.a.
#region 1 (North East)
region1 = subset(CDI, V17 == '1')
plot(region1$V12, region1$V15)
fit_region1 = lm(region1$V15 ~ region1$V12)
summary(fit_region1)
abline(fit_region1, col ='red') # Y_hat = 9223.82 + 522.16X_hat, se(beta1) = 37.13, t-score = 1.28999
522.16 + 37.13*1.28999
522.16 - 37.13*1.28999
#90% confidence interval = [474.2627, 570.0573]

#region 2 (North Central)
region2 = subset(CDI, V17 == '2')
plot(region2$V12, region2$V15)
fit_region2 = lm(region2$V15 ~ region2$V12)
summary(fit_region2)
abline(fit_region2, col = 'green') # Y_hat = 13581.41 + 238.67X_hat, se(beta1) = 27.23, t-score = 1.289589
238.67 + 27.23 * 1.289589
238.67 - 27.23 * 1.289589
#90% confidence interval = [203.5545, 273.7855]

#region 3 (South)
region3 = subset(CDI, V17 == '3')
plot(region3$V12, region3$V15)
fit_region3 = lm(region3$V15 ~ region3$V12)
summary(fit_region3)
abline(fit_region3, col = 'blue') # Y_hat = 10529.79 + 330.61X_hat, se(beta1) = 27.13, t-score = 1.287221
330.61 + 27.13*1.287221
330.61 - 27.13*1.287221
#90% confidence interval = [295.6877, 365.5323]

#region 4 (West)
region4 = subset(CDI, V17 == '4')
plot(region4$V12, region4$V15)
fit_region4 = lm(region4$V15 ~ region4$V12)
summary(fit_region4)
abline(fit_region4) # Y_hat = 8615.05 + 440.32X_hat, se(beta1) = 45.37, t-score = 1.292941
440.32 + 45.37*1.292941
440.32 - 45.37*1.292941
#90% confidence interval = [381.6593, 498.9807]
#regression models on entire data
plot(CDI$V12, CDI$V15, xlab = "Percent Bachelor's degree", ylab = 'Per capita Income in dollars',
     main = '4 Regression Models')
fit_entire = lm(CDI$V15 ~ CDI$V12)
summary(fit_entire)


#MSE of region 1 = 7335008
anova(fit_region1) #Fscore = 197.75

#MSE of region 2 = 4411341
anova(fit_region2) #Fscore = 76.826

#MSE of region 3 = 7474349
anova(fit_region3) #Fscore = 148.49

#MSE of region 4 = 8214318
anova(fit_region4) #Fscore = 94.195



#2.62 R^2 values
#plot 1 = 0.8841
#plot 2 = 0.9034
#plot 3 = 0.8989 
#largest r^2 value = answer



#2.63 obtain a separate confidence interval for each region. 90% confidence intervral
# t-score for each region
#qt(c(0.9), df = 101)
#qt(c(0.9), df =106)
#qt(c(0.9), df = 150)
#qt(c(0.9), df = 75)

#region 1 - Y_hat = 9223.82 + 522.16X_hat, se(beta1) = 37.13, t-score = 1.28999
#confidence interval = [474.2627, 570.0573]

#region 2 - Y_hat = 13581.41 + 238.67X_hat, se(beta1) = 27.23, t-score = 1.289589
#confidence interval = [203.5545, 273.7855]

#region 3 - Y_hat = 10529.79 + 330.61X_hat, se(beta1) = 27.13, t-score = 1.287221
#confidence interval = [295.6877, 365.5323]

#region 4 - Y_hat = 8615.05 + 440.32X_hat, se(beta1) = 45.37, t-score = 1.292941
#confidence interval = [381.6593, 498.9807]


qf(0.9, df1 = 1, df = 101)
qf(0.9, df1 = 1, df = 106)
qf(0.9, df1 = 1, df = 150)
df(0.9, df1 = 1, df = 75)

