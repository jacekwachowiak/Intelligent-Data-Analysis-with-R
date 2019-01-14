library('plyr')
library('ggplot2')
library(reshape2)
library(scales)  

data <- read.csv('wines-PCA.txt', sep = "\t", header = FALSE)
head(data)
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

#Removing outlier
data <- data [data$density < 2,]

data$type <- as.factor(data$type)

data <- unique(data)
nrow(data)

data$quality
class(data$type)
plot1 <- ggplot(data=data, aes(x=density, y=alcohol))

mapNumberToColor <- function (x)
{
  if (x == 1)
    return ('white')
  else
    return ('red')
}  

plot1 + geom_point(aes( color = sapply(type, mapNumberToColor), size = quality))+ scale_color_manual(values=c("red", "white")) +
  theme(panel.background = element_rect(fill = 'black', colour = 'black')) +
  
  ggtitle("Alcohol content depending on density, quality and type of wine")+
  xlab("Density")+
  ylab("Alcohol") +
  labs(color = 'color')

#--------------------------------------------

#plot2

df<- (as.data.frame(cbind(data$quality,data$sulphates, data$citic_acid, data$chlorides)))
names(df) <- c('Quality', 'Sulphates', 'Citic acid', 'Chlorides')
dfmelt<-melt(df, measure.vars = 2:4)  

ggplot(dfmelt, aes(x=factor(round_any(Quality,0.5)), y=value, fill=variable)) +
  geom_boxplot() + scale_y_continuous(trans=log2_trans()) +
ggtitle("Sulphates, citic acid and chlorides for different qualities")+
  xlab("Quality")+
  ylab("Value in logaritmic scale") +
  labs(color = 'color')

  #----------------------------------

#plot3

table0 <- xtabs(~quality+type, data=data)
mosaicplot(table0,shade=TRUE, type="pearson",main="Contingency table of quality and type")
chisq.test(table0)

#-------------------------------------------

#plot4

mapNumberToBarColor <- function (x)
{
  if (x == 1)
    return ('blue')
  else
    return ('red')
}  
ggplot(data, aes(x = residual_sugar, y=alcohol)) +
geom_col (width = 0.5, position = "identity", fill = sapply(data$type, mapNumberToBarColor), alpha = 0.15) +
  scale_fill_manual(values=c("red", "white")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,20)) +
  ggtitle("Alcohol strength based on sugar content")+
xlab("Residual sugar")+
  ylab("Alcohol strength") +
  geom_text(aes(label = "", colour = sapply(data$type, mapNumberToColor))) + 
  labs(color = 'Colour')
  
