#4.2
library("vcd")

lowTraffic <- table (c("Car", "Truck"), c("Crossing","Retreat"))
lowTraffic[1,] <- c(287,40)
lowTraffic[2,] <- c(57,42)
names(attributes(lowTraffic)$dimnames) <- c("Vehicle","Behaviour")
highTraffic <- table (c("Car", "Truck"), c("Crossing","Retreat"))
highTraffic[1,] <- c(237,57)
highTraffic[2,] <- c(52,12)
names(attributes(highTraffic)$dimnames) <- c("Vehicle","Behaviour")

lowRatio <- oddsratio(lowTraffic, log = FALSE)
lowInt <- confint(lowRatio)
fourfold(lowTraffic)
mosaic(t(lowTraffic), gp=shading_max, split_vertical=TRUE)
#greater odds that elk will cross the road while meeting with car and retreat during the meeting with the Truck


highRatio <- oddsratio(highTraffic, log = FALSE)
highInt <- confint(highRatio)
fourfold(highTraffic)
mosaic(t(highTraffic), gp=shading_max, split_vertical=TRUE)
#almost no effect, variables are independent


#4.2.2
allTraffic <- highTraffic + lowTraffic
allRatio <- oddsratio(allTraffic, log = FALSE)
allInt <- exp(confint(allRatio))
fourfold(allTraffic)
mosaic(t(allTraffic), gp=shading_max, split_vertical=TRUE)
#We can see slight correlation. It would be incorrect, because we know that only during low traffic it really matters
#if it is a car or truck. Therefore adding high traffic is like adding noise to our data

#4.2.3
#Odds ratios are different from 1, so they are not conditionally independent
highRatio
lowRatio

#4.2.4
#I need the other structure for woolf test
allEvents <- data.frame(expand.grid(behaviour=c("Crossing","Retreat"), vehicle=c("Car", "Truck"), 
                                    traffic=c("Low", "High")), count <- c(287,40,57,42,237,57,52,12))

myEventTable <- xtabs(count ~ behaviour + vehicle + traffic, data = allEvents)
woolf_test(myEventTable)
#p-value very small, so we reject null hypothesis of homogenity of odds ratio. Therefore they are not homogenous