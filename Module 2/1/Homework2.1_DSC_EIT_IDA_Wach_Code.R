library("ggplot2")
library("car")

data <- read.table("HW-diamonds.txt")


#give meaning to columns
names(data) <-  c("weight", "purity", "clarity", "certificate", "price")

#convert purity and clarity into ordinal variables. I think that ordinal type would be better in that case
#but they told us to treat it as categorical, so did I
data$purity <- factor(data$purity, levels = c("I", "H", "G", "F", "E", "D", "C"))
data$clarity <- factor(data$clarity, levels = c("VS2", "VS1", "VVS2", "VVS1", "IF"))

#convert certificate to nominal variable, we could also relevel
data$certificate <- factor(data$certificate, levels = c("HRD", "IGI", "GIA"))

#backup for future
dataOrig <- data

#seems cool
str(data)


#ex 1
ggplot(data, aes (weight, price)) + geom_point()
ggplot(data, aes (weight, I(log(price)))) + geom_point()
#Both seem nor very linear, basic one has a problem for the big weight, the second one has the problem with small weights
#Therefore we will use the first one to keep it simple, because making it logarithm would not increase quality of the model significantly
#We can see that many diamonds share the weight, but the price is slightly different.

#ex2
#We will use generalised linear model with one continuous, 2 ordinal and one nominal variable.
fittedModel <- glm(price ~ ., data, family = gaussian())
summary(fittedModel)
#We see that certificate is not so important, purity and clarity are important. However, first let's check residuals

plot(fittedModel$residuals)
#ojojoj, residuals are not random noise at all. Variance is not constant, we can see pattern. Not a good model


outlierTest(fittedModel)
#We found 4 outliers, even after Bonferonni correction for testing multiple times p-value was still much below confidence level

residualPlot(fittedModel)
#nice plot that residuals are not good. The error depends on the value, variance is not stable throughout the predictions

#ex3a
data$size <- 0
data[data$weight < 0.5,]$size <- "small"
data[0.5 <= data$weight & data$weight < 1,]$size <- "medium"
data[data$weight >= 1,]$size <- "large"
data$size <- factor(data$size, levels = c("small", "medium", "large"))


fittedModel2 <- glm(price ~ weight + purity + clarity + certificate + size + size*weight, data, family = gaussian())
summary(fittedModel2)

plot(fittedModel2$residuals)
#this time residuals much better, but we can see that there are few points on the top which seems to be too high, let's verify


outlierTest(fittedModel2)
#again 4 outliers

#
residualPlot(fittedModel2)
#We can see that there is some pattern for the low values of Predictor, but model is not terrible

#3a2
summary(fittedModel2)
#Hmm, interesting. We can see that if the size of a diamond is medium then the parameter (slope) of weight is 8845 + 3672 = 12517
#However, for large diamonds the coefficient of weight in the model is only 8845 - 7606 = 1239.
#We can see the huge difference in intercept for size medium and large
#We can interpret that the price of large diamond grows much slower with the growth of the weight after some point

#3a3
#Colour is much more valued. We can see that very pure diamonds has price 3180 greater than unpure diamonds
#For clarity the difference between the best and the worse category is only 1751

#3a4a
3180.57

#3a4b
3180.57 - 1932.54 #= 1248.03

#3a5
#GIA greater than IGI 412.55
#GIA greater than HRD 15.21
#HRD greater than IGI 397.34

#3b
fittedModel4 <- glm(price ~ weight + purity + clarity + certificate + I(weight^2), dataOrig, family = gaussian())
summary(fittedModel4)

plot(fittedModel4$residuals)
#also not bad. Definitely some outliers, but variance quite stable

outlierTest(fittedModel4)
#the same 4 outliers

residualPlot(fittedModel4)

#4
#We obtained similar results using both remedies. In my opinion second remedy is better, because we do not define borders for size of the diamond on our own
#In the first remedy we could obtain nice interpretable results that price of huge diamonds is less dependend on the weight
#but the result could be differ if we change the border of sizes
#Moreover, the second remedy is much faster to obtain.

#General remark: I think that maybe ordered type should be used instead of factor (ordinal instead of categorical)
#but I found it difficult to interpret results and she told that it should be categorical