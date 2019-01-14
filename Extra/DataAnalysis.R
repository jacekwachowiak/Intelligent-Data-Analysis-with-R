library(dplyr)
library(tidyr)

data <- read.csv('kidney_fail_dataset_v4.csv', sep = ",", header = TRUE)
drugs <- read.csv('drugs2.csv', sep = ",", header = TRUE)

#write.csv(mysample, file = "input.csv", quote = FALSE)
df2 <- as.data.frame(sapply(drugs,gsub,pattern="--",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern="u/d",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern=" ",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern="\\.",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern="0",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern="1",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern="2",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern="3",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern="4",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern="5",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern="6",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern="7",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern="8",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern="9",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern="/",replacement=""))
df2$patient_id <- NULL
df3 <- cbind(patient_id = data$patient_id, df2) 
df4 <- mutate_all(df3, funs(toupper))
write.table(df4, file = "drugs2.csv", quote = FALSE, sep = ",", row.names = FALSE)

#drugs2 <- read.csv('drugs2.csv', sep = ",", header = TRUE, row.names=1)
drugs2 <- df4

t <- cbind(drugs2, data$kidney.failure)
head(t)
z <- unite(t, "drugs", c("drug1", "drug2", "drug3", "drug4", "drug5", "drug6", "drug7", "drug8", "drug9"), sep = '$$$$')
z$drugs <- lapply(strsplit(as.character(z$drugs),split='$$$$', fixed= TRUE),trimws)
ones <- z[z$`data$kidney.failure` == 1,]
zeroes <- z[z$`data$kidney.failure` == 0,]
d1 <- unlist(ones$drugs)
d0 <- unlist(zeroes$drugs)
#drop NA's
d1 <- Filter(function(x) !identical('NA',x), d1)
d0 <- Filter(function(x) !identical('NA',x), d0)

drugsTakenByPeopleWithKidneyFailures <- unique(d1)
drugsTakenByPeopleWithoutKidneyFailures <- unique(d0)

length(d1)
length(d0)

length(drugsTakenByPeopleWithKidneyFailures)
length(drugsTakenByPeopleWithoutKidneyFailures)

#All of the drugs taken by patients (with repetitions)
together <- c(d1, d0)
length(together)

#Number of unique drugs in dataset
length(unique(together))

i <- table(data.frame(together))
i0 <- table(data.frame(d0))
i1 <- table(data.frame(d1))

#If you want to obtain drugs with minimum occurences for any table, follow approach below
DrugsWithMinimumOccurencesTakenByPeopleWithoutKidneyFailures <- Filter(function(x) x > 25, i0)
DrugsWithMinimumOccurences <- Filter(function(x) x > 50, i)
length(DrugsWithMinimumOccurences)


diff <- function(x, y) 
{
  return(y[x] / DrugsWithMinimumOccurences[x])
}

#to get percentage of drugs taken by people with/without kidney failure just run a command below
q <- lapply(names(DrugsWithMinimumOccurencesTakenByPeopleWithoutKidneyFailures), diff, DrugsWithMinimumOccurencesTakenByPeopleWithoutKidneyFailures)
