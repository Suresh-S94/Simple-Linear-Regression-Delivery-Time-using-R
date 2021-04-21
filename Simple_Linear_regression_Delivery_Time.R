install.packages('readr')
library(readr)
dt_st <-read_csv("C:/Users/Factory/Desktop/delivery_time.csv")
View(dt_st)
summary(dt_st)
attach(dt_st)
plot(`Sorting Time`, `Delivery Time`)
cor(`Sorting Time`, `Delivery Time`)
reg <- lm(`Delivery Time`~ `Sorting Time`)
summary(reg)
reg$fitted.values
reg$residuals
pred <- predict(reg)
attach(reg)
sum(residuals)
mean(residuals)
hist(residuals)
sqrt(sum(residuals^2)/nrow(dt_st))  #RMSE
sqrt(mean(residuals^2))
confint(reg,level=0.95)
predict(reg,interval="predict")
install.packages('ggplot2')
library(ggplot2)
ggplot(data = dt_st, aes(x = `Sorting Time`, y = `Delivery Time`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt_st, aes(x=`Sorting Time`, y=pred))
plot(log(`Sorting Time`), `Delivery Time`)
cor(log(`Sorting Time`), `Delivery Time`)
reg_log <- lm(`Delivery Time` ~ log(`Sorting Time`)) 
summary(reg_log)
reg_log$fitted.values
reg_log$residuals
predict(reg_log)
sum(reg_log$residuals)
mean(reg_log$residuals)
sqrt(sum(reg_log$residuals^2)/nrow(dt_st))  #RMSE
sqrt(mean(reg_log$residuals^2))
confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")
ggplot(data = dt_st, aes(x = log(`Sorting Time`), y = `Delivery Time`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt_st, aes(x=log(`Sorting Time`), y=pred))
plot(`Sorting Time`, log(`Delivery Time`)) #plot(x,log(y))
cor(`Sorting Time`, log(`Delivery Time`)) #cor(x,log(y))
reg_exp <- lm(log(`Delivery Time`) ~ `Sorting Time` )  #lm(log(Y) ~ X)
summary(reg_exp)
reg_exp$fitted.values
reg_exp$residuals
predict(reg_exp)
logdt <- predict(reg_exp)
dt <- exp(logdt)
error = dt_st$`Delivery Time` - dt
error
sum(error)
mean(error)
hist(error)
sqrt(sum(error^2)/nrow(dt_st))  #RMSE
sqrt(mean(error^2))
confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")
library(ggplot2)
ggplot(data = dt_st, aes(x = `Sorting Time`, y = log(`Delivery Time`))) + 
  geom_point(color='blue') +
plot(`Sorting Time`, `Delivery Time`)
plot(`Sorting Time`*`Sorting Time`, `Delivery Time`)
cor(`Sorting Time`, `Delivery Time`)
cor(`Sorting Time`*`Sorting Time`,(`Delivery Time`))
reg2degree <- lm((`Delivery Time`) ~ `Sorting Time` + I(`Delivery Time`*`Delivery Time`))
summary(reg2degree)
reg2degree$fitted.values
reg2degree$residuals
predict(reg2degree)
reg2degree$residuals
sum(reg2degree$residuals)
mean(reg2degree$residuals)
hist(reg2degree$residuals) # check errors are normally distributed or not.
sqrt(sum((reg2degree$residuals)^2)/nrow(dt_st))  #RMSE
sqrt(mean(reg2degree$residuals^2))
confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")
ggplot(data = dt_st, aes(x = `Sorting Time` + I((`Sorting Time`)^2), y = `Delivery Time`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt_st, aes(x=`Sorting Time`+I(`Sorting Time`^2), y=pred))
