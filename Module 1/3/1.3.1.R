library(ppcor)
library(nortest)
library("MVN")

library(moments)
library(Hmisc)
library(corrplot)
library(corrgram)
library("RcmdrMisc")
data <- read.csv('wines-PCA.txt', sep = "\t", header = FALSE)
names(data) <- c('fixed_acidity',
                 'volatile_acidity'
                 ,'citic_acid'
                 ,'residual_sugar'
                 ,'chlorides'
                 ,'free sulfur_dioxide'
                 ,'total_sulfur_dioxide'
                 ,'density'
                 ,'pH'
                 ,'sulphates'
                 ,'alcohol'
                 ,'quality'
                 ,'type')
#1.3.1a
Sugar <- data$residual_sugar
#not normal, seems skewed to right, but will confirmm later, only positive values, definietely not normal distribution
hist(Sugar)
#Mean value equals 4
mean(Sugar)
#variance (standard deviation squared), Rather big, points far from the center.
var(Sugar)
#very much skewed to right (positive skeweness)
skewness(Sugar)
# leptokurtic, because greater thant 3 (normal distribution). It means there are peaks
kurtosis(Sugar)

#statistical test to verify it. Number of elements > 50, so Lilliefors' test. Definietely not normal
lillie.test(Sugar)

#It seems more exponential distribution rather than normal one. Will try to make it normal


#test for skeweness. Definitely skewed
agostino.test(Sugar)
#test for kurtosis. Definitely kurtosis is not the same as in normal distribution
anscombe.test(Sugar)  


#logarithm still did not help to obtain normality
hist(log(Sugar))
#The same for sqrt
hist(sqrt(Sugar))
#1/x seems the best, but still far from normal
hist(1/Sugar)


#-------------------------------------------
#1.3.1b
dataChlorides <- data$chlorides
dataDensity <- data$density

#Single histograms. Seems that there is outlier in density
hist(dataChlorides)
hist(dataDensity)

#We have clearly some outlier in density. Let's remove it
qqplot(dataChlorides, dataDensity)

#Detect position of the outlier
which(dataDensity > 5)
#Need to be removed from both datasets
dataChlorides <- dataChlorides[-15]
dataDensity <- dataDensity[-15]

#Clearly not normal. Still outliers. Let's remove them
qqplot(dataChlorides, dataDensity)

#seems slightly better, let's check this with test
plot(dataChlorides[dataChlorides < 0.3], dataDensity[dataChlorides < 0.3])

#still not normal
mardiaTest(cbind(dataChlorides[dataChlorides < 0.3], dataDensity[dataChlorides < 0.3]), qqplot=T)
#estimate parameters for transformation, -0.55 and 3.0
powerTransform(cbind(dataChlorides[dataChlorides < 0.3], dataDensity[dataChlorides < 0.3]))

#perform boxcox transofmriation
transformed <- bcPower(cbind(dataChlorides[dataChlorides < 0.3], dataDensity[dataChlorides < 0.3]), c(-0.55,3.0))

#It still is not normal. What a pity. But seems better than previosly
mardiaTest(transformed, qqplot=T)
#interpretation? - not normal

#----------------------------
#1.3.1c

#Selecting 5 quantitive variables
dataLimited <- data[c(1,2,9,10,11)]

#Correlation matrix. The greatest one is negative correlation bitween fixed_acidity and pH
#It makes sense, because pH is the measure of acidity.
cor(dataLimited)

#Let's do some tests to prove correlations. Seems that we have 4 confirmed correlations. pH vs fixed_acidity, pH vs volatile_acidity
#sulphates vs fixed_acidity and sulphates vs volatile acidity. All of them have p-value less than 0.05.
rcorr(as.matrix(dataLimited))

# Pairwise correlations with p-values. Another point of view, the same conclusions about correlations
rcorr.adjust(dataLimited)

#-------------------------------------

#matrix of partial correlation. The same conclusion
pcor(dataLimited)

#interpretation?
corrgram(dataLimited,
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)


#Function defined during classes to calculate coefficient of determination
r2multv<-function(x){
  r2s=1-1/(diag(solve(cov(x)))*diag(cov(x)))
  sort(r2s, decreasing=TRUE)
}

#All R Squared small, so there are no strong linear relationships
r2multv(dataLimited)
#--------------------
#determinant R
#not linear because >>0
det(cor(dataLimited)) 
#partial correlations 
matrix.partial=pcor(dataLimited)$estimate

#Visualizing partial correlations
corrgram(matrix.partial,
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

#-------------------------------

#No small eigenvalues. Therefore no linear relationship
eigen(cor(dataLimited))

