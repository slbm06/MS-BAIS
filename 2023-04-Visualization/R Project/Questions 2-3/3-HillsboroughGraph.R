rm(list=ls())
library(readxl)
library(ggplot2)
setwd("C:/Users/Dropbox/Education/MS-BAIS/3-Data Visualization for Story Telling/Assignment Module 9/Questions2-3")

hillsCovid <-  read_excel('Combined.xlsx', sheet='Hillsborough')
df_h <- data.frame(hillsCovid)
head(df_h)

# Fix the column headers
colnames(df_h) <- c('Date','NumberOfCases','MovingAverage')
head(df_h)

# Fix the data type of Date column
class(df_h$Date)
df_h$Date <- as.Date(df_h$Date,"%B %d, %Y")
class(df_h$Date)
head(df_h)

ggplot(df_h, aes(x=Date, y=MovingAverage)) +
  geom_bar(stat="summary_bin",fun=mean, fill='mediumblue', color='black') +
  geom_line(aes(y=NumberOfCases), color='magenta', size=.5) +
  labs(title='Moving Averages and Daily Covid Cases in Hillsborough County',
       x='',
       y='') +
  theme(plot.title = element_text(hjust = 0.5, size=16))

ggsave("Question3_Visualization.jpg")
