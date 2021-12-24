#6.28a
#Model 1
total_population = CDI$V5 #X1
stem(total_population)

land_area = CDI$V4 #X2
stem(land_area)

total_income = CDI$V16 #X3
stem(total_income)

#Model 2
population_density = total_population / land_area
CDI$V18 = population_density
stem(population_density)

elderly_percentage = CDI$V7
stem(elderly_percentage)

stem(total_income)

active_physician = CDI$V8

#6.28b
data1 = CDI[,c(8,5,4,16)]
colnames(data1) = c("Active Physicians", "Total Population", "Land Area", "Total Income")
pairs(data1, lower.panel = NULL)
cor(data1)

data2 = CDI[,c(8,18,7,16)]
colnames(data2) = c("Active Physicians", "Population Density", "Elderly Population", "Total Income")
pairs(data2, lower.panel = NULL)
cor(data2)

#6.28c
fit1 = lm(active_physician ~ total_population + land_area + total_income)
summary(fit1)

fit2 = lm(active_physician ~ population_density + elderly_percentage + total_income)
summary(fit2)

#6.28d
#summary Âü°í

#6.28e
#Model 1
residuals1 = fit1$residuals
yfit1 = fitted.values(fit1)
 
plot(yfit1, residuals1, xlab = "Fitted Y", ylab = "Resdual", main = "Residuals against Fitted Y" ) #residuals against fitted y-values
plot(total_population, residuals1, xlab ="Total Population", ylab = "Residuals", main = "Residuals against Total Population") #residuals against total population
plot(land_area, residuals1, xlab = "Land area (square miles)", ylab = "Residuals", main ="Residuals against Land Area") #residuals against land area
plot(total_income, residuals1, xlab = "Total Personal Income (in millions of dollars)", ylab = "Residuals", main = "Residuals against Total Personal Income") #residuals against total income
abline(h=0, col = 'red')

qqnorm(residuals1)
qqline(fit1$residuals, col ='red')

#Model 2
residuals2 = fit2$residuals
yfit2 = fitted.values(fit2)

plot(yfit2, residuals, xlab = "Fitted Y", ylab = "Residuals", main = "Residuals against Fitted Y") #residuals against fitted y-values
plot(population_density, residuals, xlab = "Population Density", ylab = "Residuals", main ="Residuals against Population Density") #residuals against population density
plot(elderly_percentage, residuals, xlab = "Percent of Population 65 or older", ylab = "Residuals", main = "Residuals against Elderly Percentage") #residuals against elderly percentage
plot(total_income, residuals, xlab = "Total Personal Income (in millions of dollars)", ylab = "Residuals", main = "Residuals against Total Personal Income") #residuals against total income 
abline(h=0, col ='red')

qqnorm(residuals2)
qqline(fit2$residuals, col = 'red')

#6.28 f
#Model 1
full.model = lm(active_physician ~ total_population + land_area + total_income)
full.model_x1x2 = lm(active_physician ~ total_population + land_area + total_income + total_population:land_area)
full.model_x1x3 = lm(active_physician ~ total_population + land_area + total_income + total_population:total_income)
full.model_x2x3 = lm(active_physician ~ total_population + land_area + total_income + land_area:total_income)

plot(fitted.values(full.model_x1x2), full.model_x1x2$residuals, xlab = "Fitted Y", ylab = "Residuals", main = "Residuals against Fitted Y (X1X2)")
plot(fitted.values(full.model_x1x3), full.model_x1x3$residuals, xlab = "Fitted Y", ylab = "Residuals", main = "Residuals against Fitted Y (X1X3)")
plot(fitted.values(full.model_x2x3), full.model_x2x3$residuals, xlab = "Fitted Y", ylab = "Residuals", main = "Residuals against Fitted Y (X2X3)")

abline(h=0, col ='red')

summary(full.model_x1x2)
summary(full.model_x1x3)
summary(full.model_x2x3)

#Model 2
full.model = lm(V8~V18+V7+V16, data=CDI)
full.model_x1x2 = lm(V8~V18+V7+V16+V18:V7, data=CDI)
full.model_x2x3 = lm(V8~V18+V7+V16+V7:V16, data=CDI)
full.model_x1x3 = lm(V8~V18+V7+V16+V18:V16, data=CDI)

plot(fitted.values(full.model_x1x2), y=full.model_x1x2$residuals, xlab = "Fitted Y", ylab = "Residuals", main = "Residuals against Fitted Y (X1X2)")
abline(h=0, col='red')
summary(full.model_x1x2)

plot(fitted.values(full.model_x1x3), y=full.model_x1x3$residuals, xlab = "Fitted Y", ylab = "Residuals", main = "Residuals against Fitted Y (X1X3)")
abline(h=0, col='red')
summary(full.model_x1x3)

plot(fitted.values(full.model_x2x3), y=full.model_x2x3$residuals, xlab = "Fitted Y", ylab = "Residuals", main = "Residuals against Fitted Y (X2X3)")
abline(h=0, col='red')
summary(full.model_x2x3)



#7.37a

model.before_1 = lm(V8~V5+V16, data=CDI)
model.after_1 = lm(V8~V5+V16+V4, data=CDI)
SSE.before_1 = sum(model.before_1$residuals^2) # SSE(x1,x2)
SSE.after_1 = sum(model.after_1$residuals^2) # SSE(x1, x2, x3)
partial_1.R2 = (SSE.before_1 - SSE.after_1)/(SSE.before_1)
partial_1.R2

model.before_2 = lm(V8~V5+V16, data=CDI)
model.after_2 = lm(V8~V5+V16+V7, data=CDI)
SSE.before_2 = sum(model.before_2$residuals^2) # SSE(x1,x2)
SSE.after_2 = sum(model.after_2$residuals^2) # SSE(x1, x2, x4)
partial_2.R2 = (SSE.before_2 - SSE.after_2)/(SSE.before_2)
partial_2.R2

model.before_3 = lm(V8~V5+V16, data=CDI)
model.after_3 = lm(V8~V5+V16+V9, data=CDI)
SSE.before_3 = sum(model.before_3$residuals^2) # SSE(x1,x2)
SSE.after_3 = sum(model.after_3$residuals^2) # SSE(x1, x2, x4)
partial_3.R2 = (SSE.before_3 - SSE.after_3)/(SSE.before_3)
partial_3.R2

#7.37b
extra_1.SS = SSE.before_1 - SSE.after_1
extra_1.SS

extra_2.SS = SSE.before_2 - SSE.after_2
extra_2.SS

extra_3.SS = SSE.before_3 - SSE.after_3
extra_3.SS

#7.37c
reduced.model = lm(V8~V5+V16, data=CDI)
full.model = lm(V8~V5+V16+V9, data=CDI)
anova(reduced.model, full.model)

SSE.reduced = sum(reduced.model$residuals^2)
SSE.full = sum(full.model$residuals^2)
df.reduced = nrow(CDI) - 2
#Because we add one more column
df.full = nrow(CDI) - 4
#% Because we add one more column
F.statistic = ( SSE.reduced-SSE.full ) / (SSE.full/df.full)
F.statistic

#7.37d x3 = v4, x4= v7, x5 =v9
model.before_d1 = lm(V8~V5+V16, data=CDI)
model.after_d1 = lm(V8~V5+V16+V4+V7, data=CDI)
SSE.before_d1 = sum(model.before_d1$residuals^2) # SSE(x1,x2)
SSE.after_d1 = sum(model.after_d1$residuals^2) # SSE(x1, x2,x3, x4)
partial_d1.R2 = (SSE.before_d1 - SSE.after_d1)/(SSE.before_d1)
partial_d1.R2

model.before_d2 = lm(V8~V5+V16, data=CDI)
model.after_d2 = lm(V8~V5+V16+V4+V9, data=CDI)
SSE.before_d2 = sum(model.before_d2$residuals^2) # SSE(x1,x2)
SSE.after_d2 = sum(model.after_d2$residuals^2) # SSE(x1, x2,x3, x5)
partial_d2.R2 = (SSE.before_d2 - SSE.after_d2)/(SSE.before_d2)
partial_d2.R2

model.before_d3 = lm(V8~V5+V16, data=CDI)
model.after_d3 = lm(V8~V5+V16+V7+V9, data=CDI)
SSE.before_d3 = sum(model.before_d3$residuals^2) # SSE(x1,x1)
SSE.after_d3 = sum(model.after_d3$residuals^2) # SSE(x1, x2,x4, x5)
partial_d3.R2 = (SSE.before_d3 - SSE.after_d3)/(SSE.before_d3)
partial_d3.R2

#F-test
reduced.model = lm(V8~V5+V16, data=CDI)
full.model = lm(V8~V5+V16+V7+ V9, data=CDI)
anova(reduced.model, full.model)

SSE.reduced = sum(reduced.model$residuals^2)
SSE.full = sum(full.model$residuals^2)
df.reduced = nrow(CDI) - 2
#Because we add one more column
df.full = nrow(CDI) - 5
#% Because we add one more column
F.statistic = ( SSE.reduced-SSE.full ) / (SSE.full/df.full)
F.statistic

