# install.packages("maps")
# install.packages("reshape2")
library(maps) 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(reshape2)


# The functions might be useful for A4
source("../source/a4-helpers.R")
data <- read.csv("~/Documents/info201/data/incarceration_trends.csv", stringsAsFactors = FALSE)
us_states <- map_data("state")
us_counties <- map_data("county")




## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

## What is the total number of people in the prison of each State after Year 2000?
state_total_2000 <- data %>%
  filter(year >= 2000) %>%
  group_by(state) %>%
  summarise(total = sum(total_jail_pop, na.rm = TRUE)) %>%
  arrange(total)

## What is the average number of each category after Year 2000?
category_avg <- data %>%
  filter(year >= 2000) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

## Which year has the highest number of people in jail, which year has the lowest?
hi_total <- data %>%
  group_by(year) %>%
  summarise(total = sum(total_jail_pop, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  head(1) %>%
  select(year, total)

lo_total <- data %>%
  group_by(year) %>%
  summarise(total = sum(total_jail_pop, na.rm = TRUE)) %>%
  arrange(total) %>%
  head(1) %>%
  select(year, total)


## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#

# Growth of the U.S. Prison Population
get_year_jail_pop <- function() {
  # TODO: Implement this function
  total_jail_pop_year <- data %>%
    group_by(year) %>%
    summarise(total = sum(total_jail_pop, na.rm = TRUE))
  return(total_jail_pop_year)
}

# This function produce a bar chart shows the growth of the U.S. prison population from 1970 to 2018
plot_jail_pop_for_us <- function() {
  # TODO: Implement this function
  jail_pop_year_plot <- get_year_jail_pop() %>%
    ggplot(total_jail_pop_year, mapping = aes(x = year, y = total)) +
    geom_col() +
    labs(
      title = "Increase of Jail Population in U.S. (1970-2018)",
      x = "Year",
      y = "Population",
      caption = "Yearly growth of prison population from 1970-2018")
  return(jail_pop_year_plot)   
}
plot_jail_pop_for_us()


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

get_year_jail_pop_states <- function(states) {
  total_jail_pop_state <- data %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarise(year_jail_pop = sum(total_pop,na.rm = TRUE))
  return(total_jail_pop_state)
}

state <- c("WA", "CA", "DC", "NY", "TX", "ND")
plot_jail_pop_for_states <- function(states) {
  jail_pop_state_plot <- ggplot(get_year_jail_pop_states(states)) + 
    geom_line(mapping = aes(x =year, y=year_jail_pop, color = state)) + 
    labs(
      title = "Yearly Growth of Prison Population by State", 
      x = "Year", 
      y = "Population", 
      caption = "Yearly growth of prison population from 1970-2018 in the selected states")
  return(jail_pop_state_plot)
}
plot_jail_pop_for_states(state)

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_female_jail_pop_year <- function(){
  total_female_jail_pop_year <- data %>%
    group_by(year) %>%
    summarise(total = sum(female_jail_pop, na.rm = TRUE))
  return(total_female_jail_pop_year)
}

get_male_jail_pop_year <- function(){
  total_male_jail_pop_year <- data %>%
    group_by(year) %>%
    summarise(total = sum(male_jail_pop, na.rm = TRUE))
  return(total_male_jail_pop_year)
}

female_male_total_year <- function(){
  female_male <- merge(get_female_jail_pop_year(), get_male_jail_pop_year(), by = "year")
  colnames(female_male) <- c("year", "female", "male")
  return(female_male)
}
female_male_total_year()

female_male_total_year_melt <- function(){
  female_male_melt <- melt(female_male_total_year(), id.vars='year')
  return(female_male_melt)
}
female_male_total_year_melt()

plot_female_male_year <- function(){
  female_male_plot <- ggplot(female_male_total_year_melt(), 
                             aes(year, value, fill = variable)) + 
    geom_bar(stat="identity", position = "dodge") +
    labs(
      title = "Gender Comparison of Jail Population in U.S. (1970-2018)",
      x = "Year",
      y = "Population",
      caption = "Comparison between number of female and male of jail population in US"
    )
  return(female_male_plot)
}
plot_female_male_year()


## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

state_mean <- function(){
  state_mean <- data %>%
    group_by(state) %>%
    summarise(mean = mean(total_jail_pop, na.rm = TRUE))
  return(state_mean)
}


full_state_mean <- function(){
  full_state_mean <- state_mean() %>%
    mutate (region = state.name[match(state_mean()$state, state.abb)])
  full_state_mean$region = tolower(full_state_mean$region)
  return(full_state_mean)
}
full_state_mean()

state_mean_map_data <- function(){
  state_mean_map_data <- inner_join(us_states, full_state_mean(), by = "region")
  return(state_mean_map_data)
}
state_mean_map_data()

us_map <- function(){
  us_map <- ggplot() + 
    geom_polygon( data=us_states, aes(x=long, y=lat, group=group),
                  color="black", fill="lightblue" ) + 
    geom_polygon( data=state_mean_map_data(),
                  aes(x=long, y=lat, group=group, fill = mean), 
                  color="white", size = 0.2) +
    labs(
      title = "Jail Population Average in U.S. (1970-2018)",
      caption = "Average jail population of each state in US")
      
  return(us_map)
}
us_map()



