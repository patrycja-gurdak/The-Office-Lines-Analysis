library(ggplot2)
library(dplyr)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(DT)
library(shiny)
library(shinythemes)


###load data
df = read.csv("C:/code/Datasets/TheOfficeLines/the-office_lines.csv", encoding="UTF-8")

df$Line <- str_replace_all(df$Line, "’", "'")

#removing action descriptions (ex."[talking on phone]")
df$Line <-  str_replace_all(df$Line, "\\[.*?\\][ ]?", "")

reg_cast = c("Michael", "Pam", "Dwight", "Jim", "Ryan", "Stanley", "Kevin", "Meredith", "Angela", "Oscar",
             "Phyllis", "Roy", "Toby", "Jan", "Kelly", "Andy", "Creed", "Darryl", "Erin", "Gabe", "Holly",
             "Robert", "Nellie", "Clark", "Pete")

###functions
count_lines <- function(char, season){
  uniques = unique(df$Character[startsWith(df$Character, substr(char, 1, 3))])
  uniques = uniques[!startsWith(uniques, paste(char, "'S", sep=""))]
  count = sum(str_count(subset(df, Season %in% season)$Character, pattern = paste(uniques, collapse='|')))
  return(count)
}

get_data <- function(season = seq(1,9)){
  n_lines = data.frame(name = reg_cast, number = sapply(reg_cast, count_lines, season))
  rownames(n_lines) <- NULL
  return (n_lines)
}

first_episode <- function(char){
  x = subset(df, Character == char)[,c(4,5)]
  first = x[order(x$Season, x$Episode_Number),][1,]
  return(first)
}

last_episode <- function(char){
  x = subset(df, Character == char)[,c(4,5)]
  last = x[order(-x$Season, -x$Episode_Number),][1,]
  return(last)
}

lines_number = get_data()
