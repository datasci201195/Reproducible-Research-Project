#Loading the require Package
library(ggplot2)
library(lubridate)
library(dplyr)




#Pre-Processing the data
active <- read.csv("activity.csv")
active$date = ymd(active$date)
summary(active)


#1. What is mean total number of steps taken per day?

act1 <- active %>%
        group_by(date) %>%
        summarise(total_steps = sum(steps,na.rm = TRUE))

#plotting the graph
plot1 <- ggplot(act1,aes(x = date,y =  total_steps)) + geom_col() + ggtitle("1.Total steps taken per day")
print(plot1)

#mean and median
mean(act1$total_steps,na.rm = TRUE)
median(act1$total_steps,na.rm = TRUE)

#2. What is the average daily activity pattern?

act2 <- active %>%
        group_by(interval) %>%
        summarise(avg_steps = sum(steps,na.rm = TRUE))

#making plot

plot2 <- ggplot(act2,aes(x = interval,y = avg_steps)) + geom_line(colour = "darkgreen") +
        ggtitle("Average Daily Activity Pattern")
print(plot2)

#maxium number of steps in  a interval

act2$interval[act2$avg_steps == max(act2$avg_steps,na.rm = TRUE)]

#3. show a strategy for imputing missing data

#total number of rows with NA
sum(is.na(active))

#Imputing
act3 <- active


act3$steps[is.na(act3$steps)] <- mean(act3$steps,na.rm = TRUE)

#plotting the graph with and without NAs for Comparision
act3$date = ymd(act3$date)
act4 <- act3 %>%
        group_by(date) %>%
        summarise(total_steps = sum(steps))
        
        
plot3 <- ggplot(act4,aes(x = date,y =  total_steps)) + geom_col() + ggtitle("Total number of steps taken per day")
grid.arrange(plot1, plot3, ncol=2)

#4. number of steps taken per 5-minute interval across weekdays and weekends

#Filtering the data Based on Weekend and weekdays
act3$weekday <- factor(format(act3$date, "%A"))

levels(act3$weekday) <- list(weekday = c("Monday", "Tuesday",
                                              "Wednesday", "Thursday",
                                              "Friday"), weekend =
                                          c("Saturday", "Sunday"))

#ploting the graph

act5 <- act3 %>%
        filter(weekday == "weekday") %>%
        group_by(interval) %>%
        summarise(avg_steps = sum(steps))


act6 <- act3 %>%
        filter(weekday == "weekend") %>%
        group_by(interval) %>%
        summarise(avg_steps = sum(steps))

plot4 <- ggplot(act5,aes(x = interval,y = avg_steps)) + geom_line(colour = "darkgreen") +
        ggtitle("Weekday")


plot5 <- ggplot(act6,aes(x = interval,y = avg_steps)) + geom_line(colour = "darkred") +
        ggtitle("Weekend")

grid.arrange(plot4, plot5, nrow=2)

