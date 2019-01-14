library(FactoMineR)
library(factoextra)
data <- read.csv('wines-PCA.txt', sep = "\t", header = FALSE)
names(data) <- c('fixed_acidity',
                 'volatile_acidity'
                 ,'citric_acid'
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

#remove quality from data. We do not consider it for now
dataWQuality <- data[-12]

#We should perform analysis on correlation matrix instead of covariance one, because observed units have highly
#different value levels (from 0.0x to x00).
pcaWQuality <- PCA(dataWQuality, quali.sup = 12, scale.unit = TRUE)

#The most important factors for the first component are total sulfur dioxide and free sulfur dioxide, citric acid and residual sugar
#We can observe also strong negative impact of volatile acidity.

#The second factor is a linear combination of Fixed acidity, suplhates and chlorides with strong negative impact of pH

#First four PC explain 71.8% of  variance. 

#TODO: Explain more components
summary(pcaWQuality)

#Wines of type 1 have positive first PC! Cool - Dim.1
fviz_pca_ind(pcaWQuality,  habillage="type")

dataCopies <- data
dataCopies[dataCopies$quality > 6.5,]$quality <- "high"
dataCopies[dataCopies$quality == 6,]$quality <- "medium"
dataCopies[dataCopies$quality < 5.5,]$quality <- "low"

pcaQuality <- PCA(dataCopies, quali.sup = c(12,13), scale.unit = TRUE)
fviz_pca_ind(pcaQuality,  habillage="quality")

