library(FactoMineR)
library(factoextra)
data <- read.csv('cars-PCA.txt', sep = "", header = FALSE)
names(data) <- c('mpg'
                 ,'cylinders'
                 ,'engine_displacement'
                 ,'horsepower'
                 ,'weight'
                 ,'acceleration'
                 ,'model_year'
                 ,'origin'
                 ,'car_name')

dataLimited <- data[c(1,3,4,5,6)]
#We should perform analysis on correlation matrix instead of covariance one
pcaData <- PCA(dataLimited, scale.unit = TRUE)
#For pca1-Dim1 strong positive impact of horsepower, engine displacement, weight and negative of mpg and accel
#For pca2-Dim2 biggest positive impact of accel, and lower negative mpg
summary(pcaData)
#First PC explains 81,7% of  variance. Second 12,9%

#graphs
#clustering with 3 culsters - by shape
#color=origin of the car 1=America=black, 2=Europe=red, 3=Asia(Japan?)=green
clust<-kmeans(dataLimited, 3)
data$origin <- as.factor(data$origin)
fviz_pca_ind(pcaData, geom = "text") +
  geom_point(size = 4, shape=14+clust$cluster, colour=data$origin, show.legend = TRUE)

#second
#clustering with 3 culsters - by size
#color=cylinders 3=green, 4=blue, 6=pink, 8=yellow
data$cylinder <- as.factor(data$cylinder)
fviz_pca_ind(pcaData) +
  geom_point(size=1.5 * clust$cluster, colour=data$cylinder)
