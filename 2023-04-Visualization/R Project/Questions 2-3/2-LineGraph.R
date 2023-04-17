rm(list=ls())
library(readxl)
library(ggplot2)
setwd("C:/Users/Dropbox/Education/MS-BAIS/3-Data Visualization for Story Telling/Assignment Module 9/Questions 2-3")

pascoCovid <-  read_excel('Combined.xlsx', sheet='Pasco')
df_pc <- data.frame(pascoCovid)
head(df_pc)

# Fix the column headers
colnames(df_pc) <- c('Date','NumberOfCases','MovingAverage')
head(df_pc)

# Fix the data type of Date column
class(df_pc$Date)
df_pc$Date <- as.Date(df_pc$Date,"%m/%d/%Y")
class(df_pc$Date)
head(df_pc)

ggplot(df_pc, aes(x=Date, y=MovingAverage)) +
  geom_line(aes(y=NumberOfCases), color='blue', size=.5) +
  geom_line(color='red', size=1.25) + 
  labs(title='Daily Covid Cases and Moving Average for Pasco County',
       x='',
       y='') +
  theme(plot.title = element_text(hjust = 0.5, size=16))

ggsave("Question2_Visualization.jpg")
