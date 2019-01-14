#1.3.2 permutation test

load("RestaurantTips.rda")
RestaurantTips$Bill


data <- RestaurantTips
#not clear if coefficient is different than 0
plot(data$Bill, data$PctTip)

#observed coeeficient
robs <- cor(data$Bill, data$PctTip)

#number of permutations
N <- 10000

#sample makes permutation. Computed correlation 10000 times
permutedCorrelations <- replicate(N, cor(data$Bill, sample(data$PctTip)))

#plot histogram of permuted alues and the line for observed correlation
hist(permutedCorrelations)
abline(v=robs,col="red") #robs?

#calculate p-value
pval <- sum(abs(permutedCorrelations) > abs(robs)) / N
pval

#pval greater than 0.05, so we cannot reject null hypothesis and coefficient can be zero.

#----------------------
#let's remove outliers

dataLimited <- data [data$PctTip < 30,]

#new correlation
robsLimited <- cor(dataLimited$Bill, dataLimited$PctTip)

##geater than previously
robsLimited

#Once again compute permutations
permutedCorrelationsLimited <- replicate(N, cor(dataLimited$Bill, sample(dataLimited$PctTip)))

#plot histogram of permuted alues and the line for observed correlation
hist(permutedCorrelationsLimited)
abline(v=robsLimited,col="red")

#calculate p-value
pval <- sum(abs(permutedCorrelationsLimited) > abs(robsLimited)) / N

#This time p-value is much below alpha = 0.05, so we reject the null hypothesis and the correlation between amount of bill
# and percentage of tip is significant
pval

