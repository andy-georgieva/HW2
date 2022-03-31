# HW2 
#####Problem 1#####
#Write a loop which simulates 1000 times a martingale strategy based on a coin flip
#Martingale is a gambling strategy where you multiply your next bet twice if
#you have lost your previous one. You bet 1 and you win. Because you won you bet 1
# again. You lose. Then you bet 2, you lose again. You bet four and you win.
#Because you won, you go back to betting one etc. You start with 100 USD and you
#base bet is one. 
#If the coin flip is biased and you have 48.60% chance to win, when do you
#go broke on average(out of those 1000 simulations)? Look at the help for sample,
#to figure out how to pick incorporate the 48.6% probability.
#You can use a while loop for simulating when you go broke. A while loop
#loops until a condition is TRUE. Example:
# i <- 1
# while (i < 6) {
#   print(i)
#   i <- i + 1
# } 
#In your case you want to loop until your budget is > 0.
# Budget <- 100
# while (Budget > 0) {
#   Do something
# } 
#Pay attention to the fact that you can't bet more money than you have.
#If you lose 1, 2, 4, 8, 16, 32. Then your remaining money will be 
#100-32-16-8-4-2-1 = 37, so you can bet max 37 USD.

Money <- 100 
while(Money > 0){ 
Bet <- 1
TossACoin <- sample (c("win", "lose"), 1, prob = c(0.4, 0.6))
if(TossACoin == "win"){
  Money <- Money + Bet
} else { 
  Money <- Money - Bet
  Bet <- Bet*2
}
print(Money)}


##########

library(tidyverse)
library(nycflights13) 

nycflights13::flights
view(flights)
#ex.5.2.4 ----
# 1) Find all flights that: 
# a) Had an arrival delay of two or more hours 
ardelay <- dplyr::filter(flights, arr_delay > 120) 

# b) Flew to Houston (IAH or HOU) 
houston <- dplyr::filter(flights, dest == "IAH" | dest == "HOU")

# c) Were operated by United, American, or Delta 
operated <- dplyr::filter(flights, carrier == "UA" | carrier == "AA" | carrier == "DL" ) 

# d) Departed in summer (July, August, and September) 
summer <- dplyr::filter(flights, month %in% 7:9) 

# e) Arrived more than two hours late, but didn’t leave late 
arrive.leave <- dplyr::filter(flights, arr_delay > 120, dep_delay <= 0) 

# F) Were delayed by at least an hour, but made up over 30 minutes in flight 
delay.flight <- dplyr::filter(flights, dep_delay >= 60, dep_delay - arr_delay >30) 

# G) Departed between midnight and 6am (inclusive) 
departure_time <- dplyr::filter(flights, dep_time<=600 | dep_time ==2400)


#2) Another useful dplyr filtering helper is between(). What does it do? Can you use it to simplify the code needed to answer the previous challenge
summer <- filter(flights, between(month,7,9)) 

#3) How many flights have a missing dep_time? What other variables are missing? What might these rows represent? 
missing <- dplyr::filter(flights, is.na(dep_time)) 
summary(is.na(flights))  

#4) Why is NA ^ 0 not missing? Why is NA | TRUE not missing? Why is FALSE & NA not missing? Can you figure out the general rule? (NA * 0 is a tricky counterexample!)
NA^0 == 1 
NA | TRUE 
NA & FALSE 
NA & TRUE
NA | FALSE 
NA * 0 



# 5.3.1 Exercises ----
# 1)How could you use arrange() to sort all missing values to the start? (Hint: use is.na()). 
miss.start <- dplyr::arrange(flights, desc(is.na(dep_time)), dep_time)

# 2)Sort flights to find the most delayed flights. Find the flights that left earliest. 
delay.flight2 <- dplyr::arrange(flights, desc(dep_delay)) 
delay.flight3 <- dplyr::arrange(flights, dep_delay) 

# 3)Sort flights to find the fastest (highest speed) flights. 
fastest <- dplyr::arrange(flights, air_time)
head(fastest)

# 4)Which flights travelled the farthest? Which travelled the shortest? 
farthest <- dplyr::arrange(flights, desc(distance)) 
shortest <- dplyr::arrange(flights, distance) 


#5.4.1 Exercises
#1)Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights. 
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, "dep_time","dep_delay","arr_time","arr_delay")
select(flights, 4, 6, 7, 9)
#2)What happens if you include the name of a variable multiple times in a select() call? 
select(flights, dep_time, dep_time, dep_time, arr_time, arr_time, dep_delay)
  
# 2)What does the any_of() function do? Why might it be helpful in conjunction with this vector? 
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, any_of(vars))
  
 
# 4)Does the result of running the following code surprise you? How do the select helpers deal with case by default? How can you change that default?
  
  select(flights, contains("TIME")) 
  
  

  
# 1) Currently dep_time and sched_dep_time are convenient to look at, but hard to compute with because they’re not really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.
  timeOfflights <- mutate(flights, 
                          dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                          sched_dep_time_min = (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% 1440) 
  
  select ( timeOfflights, dep_time, dep_time_min, sched_dep_time, sched_dep_time_min)
  
#2)  Compare air_time with arr_time - dep_time. What do you expect to see? What do you see? What do you need to do to fix it? 
  airtime <- mutate(flights, 
                    dep_time = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                    arr_time = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
                    air_time1 = air_time - arr_time + dep_time 
                    )
    
#3) Compare dep_time, sched_dep_time, and dep_delay. How would you expect those three numbers to be related? 
    depTime <- dplyr::mutate(flights, 
                             dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440, 
                             sched_dep_time_mins = (sched_dep_time %/% 100 * 60 + sched_dep_time) %% 1440,
                             delay = dep_delay - (dep_time_mins - sched_dep_time_mins)) 
    
    filter(depTime, delay != 0 )
#4) Find the 10 most delayed flights using a ranking function. How do you want to handle ties? Carefully read the documentation for min_rank().
  most_delayed <- dplyr::mutate(flights, 
                                dep_delay_min_rank = min_rank(desc(dep_delay))) 
  most_delayed <- filter(most_delayed, 
                         !(dep_delay_min_rank > 10)) 
  most_delayed <- arrange(most_delayed, dep_delay_min_rank)

  #5) What does 1:3 + 1:10 return? Why? 
  
  1:3 + 1:10 
  # because it can not calculate the length of the vector
    
#6) What trigonometric functions does R provide?
? Trig
  cos(x)
  sin(x)
  tan(x)
  
  acos(x)
  asin(x)
  atan(x)
  atan2(y, x)
  
  cospi(x)
  sinpi(x)
  tanpi(x)
