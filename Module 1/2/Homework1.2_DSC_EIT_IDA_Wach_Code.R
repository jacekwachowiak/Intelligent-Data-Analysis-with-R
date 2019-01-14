library('plyr')
library('ggplot2')
library(reshape2)
library(scales)
library(ppcor)
library(nortest)
library("MVN")
library(moments)
library(Hmisc)
library(corrplot)
library(corrgram)
library("RcmdrMisc")
library(FactoMineR)
library(factoextra)
library(gridExtra)

data <- read.csv('paris_paintings.csv', sep = ",", header = TRUE, na.strings=c("", "NA"))
head(data)
# 61 var, 3394 obs
# NA values for each variable
na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count
# let's remove the vars that have too many NA - over 10%
vars <- names(data) %in% c("Interm", "Surface_Rnd", "Diam_in", "endbuyer", "type_intermed", "materialCat", "winningbiddertype", "count") 
data2 <- data[!vars]
#still some var are useless
vars <- names(data2) %in% c("other", "subject") 

data3 <- data2[!vars]
data3$price <- as.numeric(data3$price)
###########
## plot 1
## price by year and surface and if living
meanAll <- mean(data3$price, na.rm=TRUE)

aggdata <- aggregate(price ~ year, data3, FUN=median)
aggdataS <- aggregate(Surface ~ year, data3, FUN=median)
aggdataL <- aggregate(artistliving ~ year, data3, FUN=mean)
plot4 <- ggplot(data=aggdata, aes(x=aggdata$year, y=aggdata$price)) + 
  geom_point(mapping = NULL, data = aggdata, stat = "identity",position = "identity", na.rm = TRUE, show.legend = TRUE,inherit.aes = TRUE)+
  aes(colour =  100 * aggdataL$artistliving) +
  aes(size = aggdataS$Surface/75 ) +
  geom_hline(yintercept = meanAll, color = "red", linetype="dashed") +
  ggtitle("Median price depending on year of sale, size of the artwork and percantage of artists being alive") +
  xlab("Year of sale")+
  ylab("Price") +
  labs(color = 'Percentage of alive artists') +
  scale_size(name   = "Size in squared inches",
             range = c(2, 10),
             breaks = fivenum(aggdataS$Surface/100),
             labels = fivenum(aggdataS$Surface))
plot4
###########
## plot 2
## lets try to see if number of objects in the painting has sth to do with the price and surface// from var37 land_sc /subset
data3$objectsum <- rowSums(data3[37:51])
## only one with 5 objects - lets remove it to have a clear view
data4 <- data3 [data3$objectsum < 5,]
##
aggdataPrice <- aggregate(price ~ objectsum, data4, FUN=mean)
aggdataSurf <- aggregate(Surface ~ objectsum, data4, FUN=mean)
##
df<- (as.data.frame(cbind(data4$objectsum, data4$price, data4$Surface)))
names(df) <- c('objectsum', 'Price', 'Surface')
dfmelt<-melt(df, measure.vars = 2:3)  

plot5 <- ggplot(data=dfmelt, aes(x=factor(round_any(objectsum,0.5)), y=value, fill=variable)) + 
  geom_boxplot() + scale_y_continuous(trans=log2_trans(), limits = c(32, 2048)) +
  ggtitle("Price and surface for different number of noted objects on the painting")+
  xlab("Objects")+
  ylab("Values in logaritmic scale") +
  labs(color = 'color')
plot5


##########################
###plot3
##########################
mysample3 <- subset(data3, data3$origin_author == "D/FL")
plot10 <- ggplot(data=mysample3, aes(x=Width_in, y=Height_in)) +
  geom_point(mapping = NULL, stat = "identity",position = "identity", na.rm = TRUE, show.legend = TRUE,inherit.aes = TRUE, size=2, color = "blue") +
  scale_y_continuous(trans=log10_trans(), limits = c(1, 150)) +
  scale_x_continuous(trans=log10_trans(), limits = c(1, 230)) +
  xlab("Width in inches")+
  ylab("Height in inches") +
  ggtitle("D/FL = Dutch/Flemish")
plot10
##
mysample4 <- subset(data3, data3$origin_author == "F")
plot11 <- ggplot(data=mysample4, aes(x=Width_in, y=Height_in)) +
  geom_point(mapping = NULL, stat = "identity",position = "identity", na.rm = TRUE, show.legend = TRUE,inherit.aes = TRUE, size=2, color = "red") +
  scale_y_continuous(trans=log10_trans(), limits = c(1, 150)) +
  scale_x_continuous(trans=log10_trans(), limits = c(1, 230)) +
  xlab("Width in inches")+
  ylab("Height in inches")  +
  ggtitle("F = French")
plot11
##
mysample5 <- subset(data3, data3$origin_author == "I")
plot12 <- ggplot(data=mysample5, aes(x=Width_in, y=Height_in)) +
  geom_point(mapping = NULL, stat = "identity",position = "identity", na.rm = TRUE, show.legend = TRUE,inherit.aes = TRUE, size=2, color = "green") +
  scale_y_continuous(trans=log10_trans(), limits = c(1, 150)) +
  scale_x_continuous(trans=log10_trans(), limits = c(1, 230)) +
  xlab("Width in inches")+
  ylab("Height in inches")  +
  ggtitle("I = Italian")
plot12
##
mysample6 <- subset(data3, data3$origin_author != "I" & data3$origin_author != "F" & data3$origin_author != "D/FL")
plot13 <- ggplot(data=mysample6, aes(x=Width_in, y=Height_in)) +
  geom_point(mapping = NULL, stat = "identity",position = "identity", na.rm = TRUE, show.legend = TRUE,inherit.aes = TRUE, size=2, color = "orange") +
  scale_y_continuous(trans=log10_trans(), limits = c(1, 150)) +
  scale_x_continuous(trans=log10_trans(), limits = c(1, 230)) +
  xlab("Width in inches")+
  ylab("Height in inches") +
  scale_color_discrete(name = "Color")  +
  ggtitle("All other origins including unknown")
plot13
##
grid.arrange(plot10, plot11, plot12, plot13, ncol=2, nrow=2, top = "Comparison of painting's size by the nationality of painters")


#########
## plot 4
#########
## pca
#subset of random 300 obs
data33 <- data3[sample(1:nrow(data3),300, replace=FALSE),]
dataLimited <- data33[c(6,7,12,14,18,19,22,52)]
dataLimited <- dataLimited[-966,]
dataLimited2 <- dataLimited[complete.cases(dataLimited), ]
dataLimitedPCA <- dataLimited2[-2]
dataLimitedPCA <- dataLimitedPCA[-6]
pcaData <- PCA(na.omit(dataLimitedPCA), scale.unit = TRUE)
summary(pcaData)
##
#force colors
mapOriginToShape <- function (x)
{
  if (x == 'D/FL')
    return (15)
  else if (x == 'F')
    return (16)
  else if (x == 'I')
    return (17)
  else
    return (18)
}
mapPriceToColor1 <- function (x)
{
  if (x < 250)
    return ('#ff66b2')
  else if (x <450)
    return ('#ff007f')
  else if (x <700)
    return ('#99004c')
  else
    return ('#330000')
}
mapPriceToColor2 <- function (x)
{
  if (x < 250)
    return ('#00ffff')
  else if (x <450)
    return ('#0066cc')
  else if (x <700)
    return ('#000099')
  else
    return ('#330000')
}

#graph
fviz_pca_biplot(pcaData, geom = "points") +
  geom_point(size = log10(dataLimited2$Surface)*1.3+1, shape=sapply(dataLimited2$origin_author, mapOriginToShape), colour=sapply(dataLimited2$price, mapPriceToColor1), show.legend = TRUE)

