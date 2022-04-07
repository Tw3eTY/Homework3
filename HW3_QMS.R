#####Problem 1##### ----

# Write a function, which uses a loop to calculate factorial.
# The base R function is called factorial and you should replicate its result.
# This is a function, which takes two numbers, multiplies them and returns
# the result as output:
# MultiplicationFunction <- function(inputNumber1, inputNumber2){
#   Result <- inputNumber1 * inputNumber2
#   return(Result)
# }
# MultiplicationFunction(5, 3)
# 
# Write a factorial function:

a <- 5

fctrl <- function (n){
     if (n <= 1){
       Result <- "If 0 or 1 = 1, if negative - not possible"
     }else{
       Result <- n
       
       while (n > 1){
         Result <- (n - 1) * Result
         n <- n - 1
       }
     }
     return(Result)
}

fctrl(a)

#####Problem 1#####

#####Problem 2##### ----

#Write a function which takes a vector and returns its standard deviation.
#You should get the same results as the sd() function.
# SDFunction <- function(inputVector){
#   ???
#     return(Result)
# }
# ??? is not Result <- sd(inputVector)

b <- c(1, 5, 6, 12 ,43)

sdf <- function (InputVector){
  avg = sum(InputVector)/length(InputVector)
  SumDiff <- sum((InputVector - avg)^2)
  FinalRes <- sqrt(sum((InputVector - avg)^2) / (length(InputVector)-1))
  return(FinalRes)
}

sdf(b)

#####Problem 2#####

#####Problem 3##### ----

# Read everything from https://r4ds.had.co.nz/transform.html, 
# in particular chapters 5.6/5.7
library(nycflights13)
library(tidyverse)
#Do all the exercises:

## WARNING ! - THESE TASKS ARE CO-WORKED WITH ALREADY SOLVED EXEMPLES ONLINE, BECAUSE I STILL CANT QUITE UNDERSTAND 5.6 AND 5.7 ... I FOLLOWED ALONG AND TRIED TO UNDERSTAND WHAT THE AUTHOR IS DOING ##

# 5.6.7 Exercises 
#1.Brainstorm at least 5 different ways to assess the typical delay characteristics of a group of flights. Consider the following scenarios:
  
not_cancelled <- flights %>% 
  filter(!is.na(air_time))

not_cancelled <- flights %>%
  group_by(tailnum)%>%
  mutate(count = n(), median_arr_delay = median(arr_delay), median_dep_delay = median(dep_delay))%>%
  filter(count > 25 ) %>%
  arrange(median_arr_delay, median_dep_delay)


#a)A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time.

not_cancelled %>%
  group_by(tailnum) %>%
  summarise(count = n(), min15_early_arr = mean(arr_delay < -15), min15_dep_arr = mean(dep_delay < -15)) %>%
  filter(min15_early_arr > 0.5 | min15_dep_arr > 0.5) %>%
  filter(count > 25) %>%
  arrange(desc(min15_early_arr), desc(min15_dep_arr))

#b)A flight is always 10 minutes late.

not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    count = n(),
    exact_10 = mean(arr_delay == 10)
  ) %>% 
  filter(count > 10) %>%
  arrange(desc(exact_10))


#c)A flight is 30 minutes early 50% of the time, and 30 minutes late 50% of the time.

not_cancelled %>%
  group_by(tailnum) %>%
  summarise(count = n(), min30_early_arr = mean(arr_delay < -30), min30_dep_arr = mean(dep_delay < -30)) %>%
  filter(min30_early_arr > 0.5 | min30_dep_arr > 0.5) %>%
  filter(count > 25) %>%
  arrange(desc(min30_early_arr), desc(min30_dep_arr))

#d)99% of the time a flight is on time. 1% of the time it's 2 hours late.

#Which is more important: arrival delay or departure delay? - Arrival



#2.Come up with another approach that will give you the same output as not_cancelled %>% count(dest) and not_cancelled %>% count(tailnum, wt = distance) (without using count()).

not_cancelled %>%
  group_by(dest) %>%
  summarise(
    count = n()
  )
#If not using count is only for the second part

not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    n = sum(distance)
  )

#3.Our definition of cancelled flights (is.na(dep_delay) | is.na(arr_delay) ) is slightly suboptimal. Why? Which is the most important column?
  
# air_time -> If its NA - flight is cancelled

#4.Look at the number of cancelled flights per day. Is there a pattern? Is the proportion of cancelled flights related to the average delay?
  
flights %>%
  group_by(year, month, day) %>%
  summarise(
    n_cancelled = sum(is.na(air_time) | air_time == 0),
    avg_arr_delay = mean(arr_delay, na.rm = TRUE),
    avg_dep_delay = mean(dep_delay, na.rm = TRUE) 
  ) %>%
  select(year, month, day, n_cancelled, avg_arr_delay, avg_dep_delay) %>%
  filter(avg_arr_delay > 0) %>%
  ggplot()+
  geom_point(aes(x = avg_arr_delay, y = n_cancelled, color = "red"))+
  geom_smooth(aes(x = avg_arr_delay, y = n_cancelled, color = "red"), se =FALSE,method = "gam") +
  geom_point(aes(x = avg_dep_delay, y = n_cancelled,color =  "blue"))  +
  geom_smooth(aes(x = avg_dep_delay, y = n_cancelled,color =  "blue"), se =FALSE, method = "gam") 

#5.Which carrier has the worst delays? Challenge: can you disentangle the effects of bad airports vs. bad carriers? Why/why not? (Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n()))

not_cancelled %>%
  ggplot()+
  geom_boxplot(mapping = aes(x = carrier, y = arr_delay))


not_cancelled %>%
  group_by(carrier) %>%
  summarise(
    count = n(),
    median_arr_delay = median(arr_delay),
    avg_arr_delay = mean(arr_delay)
  ) %>%
  filter(count > 1000)

#6.What does the sort argument to count() do. When might you use it?

# Sorts in decending order - when getting lange samples

# 5.7.1 Exercises

##1.Refer back to the lists of useful mutate and filtering functions. Describe how each operation changes when you combine it with grouping.

## We can use it in groups - mutate can be used to create new variables by using group statistics or filter out entire group or filter as normal

##2.Which plane (tailnum) has the worst on-time record?

not_cancelled %>%
  group_by(tailnum) %>%
  summarise(count = n(), max_arr_delay = max(arr_delay), is_on_time_freq = mean(arr_delay <= 0, na.rm = TRUE)) %>%
  filter(count > 25) %>%
  arrange(desc(is_on_time_freq))

##3. What time of day should you fly if you want to avoid delays as much as possible?

not_cancelled %>%
  mutate(dep_time_hour = dep_time %/% 100) %>%
  group_by(dep_time_hour) %>%
  summarise(count = n(), avg_arr_delay = mean(arr_delay,na.rm = TRUE), is_on_time_prop = mean(arr_delay <= 0)) %>%
  filter(count > 25) %>%
  arrange(desc(is_on_time_prop))

##4. For each destination, compute the total minutes of delay. For each, flight, compute the proportion of the total delay for its destination.

not_cancelled %>%
  group_by(dest) %>%
  filter(arr_delay > 0) %>%
  summarise(count = n(), total_delay = sum(arr_delay))
not_cancelled %>%
  filter(arr_delay > 0) %>% 
  group_by(flight) %>%
  mutate(total_delay = sum(arr_delay)) %>% 
  group_by(flight,dest) %>%
  summarise(
    delay_prop = sum(arr_delay)/mean(total_delay)
  )

##5. Delays are typically temporally correlated: even once the problem that caused the initial delay has been resolved, later flights are delayed to allow earlier flights to leave. Using lag() explore how the delay of a flight is related to the delay of the immediately preceding flight.

not_cancelled %>%
  group_by(origin) %>%
  arrange(year, month,day,dep_time) %>%
  mutate(
    lag_dep_delay = lag(dep_delay)
  ) %>%
  filter(month == 1 & day ==1 & origin == "JFK") %>%
  select(dep_delay, lag_dep_delay) %>%
  arrange(desc(origin)) %>%
  filter(!is.na(lag_dep_delay) & lag_dep_delay < 250 & dep_delay < 250) %>%
  ggplot(aes(lag_dep_delay,dep_delay))+
  geom_point()+
  geom_smooth(se = FALSE, method = "glm")

##6. Look at each destination. Can you find flights that are suspiciously fast? (i.e. flights that represent a potential data entry error). Compute the air time a flight relative to the shortest flight to that destination. Which flights were most delayed in the air?

select(not_cancelled,dest, air_time) %>%
  arrange(dest) %>%
  ggplot() +
  geom_boxplot(aes(x = dest, y = air_time))


not_cancelled %>%
  group_by(dest) %>%
  mutate(
    median = median(air_time),
    low_outlier_limit = quantile(air_time, 0.25) - 3*(IQR(air_time, na.rm = TRUE))
  )%>%
  filter(low_outlier_limit > air_time) %>%
  select(flight, dest,median,low_outlier_limit, air_time)

##7. Find all destinations that are flown by at least two carriers. Use that information to rank the carriers.

not_cancelled %>%
  group_by(dest) %>%
  summarise(
    distinct_carrier= n_distinct(carrier, na.rm = TRUE)
  ) %>% 
  filter(distinct_carrier > 1) %>%
  arrange(desc(distinct_carrier))

##8. For each plane, count the number of flights before the first delay of greater than 1 hour.

not_cancelled %>%
  group_by(origin, tailnum) %>%
  summarise(
    count = n(),
    agg_dep_delay = sum(cumsum(dep_delay > 60) < 1)
  )


#####Problem 3#####

#####Problem 4##### ----
#Find the following:
#4.1 For each carrier what is the most common destination?
#4.2 For each carrier what is the biggest delay?
#4.3 Which are the three plane which have flown the most/least miles?
#4.4 What are the first/last flights for each day in February 2013?
#4.5 Which company flew the most miles in March 2013? Which flew the least?
#4.6 Which month had the most delays over 60 minutes?
#4.7 What is the average time between two consecutive flights?
#4.8 Use the SDFunction function from exercise 2 to calculate the standard deviation
#of the flight delays for each month and for each destination.
#####Problem 4#####