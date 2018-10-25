library(ggplot2)
library(lubridate)
library(dplyr)





active <- read.csv("activity.csv")


dim(active)
head(active)
summary(active)
str(active)

#Pre processing stage
active$date = ymd(active$date)


#What is mean total number of steps taken per day?

act1 <- active %>%
        group_by(date) %>%
        summarise(total_steps = sum(steps,na.rm = TRUE))

#plotting the graph
ggplot(act1,aes(x = total_steps)) + geom_histogram(bins = 20)

#mean and median
mean_steps = mean(act1$total_steps,na.rm = TRUE)
median_steps = median(act1$total_steps,na.rm = TRUE)

# What is the average daily activity pattern?

act2 <- active %>%
        group_by(interval) %>%
        summarise(avg_steps = sum(steps,na.rm = TRUE))

#making plot





ggplot(active,aes(x = interval,))






