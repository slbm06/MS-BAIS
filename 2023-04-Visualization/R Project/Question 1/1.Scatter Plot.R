rm(list=ls())
library(readxl)
library(ggplot2)
setwd("C:/Users/Dropbox/Education/MS-BAIS/3-Data Visualization for Story Telling/Assignment Module 9/Question 1")

data2018 <-  read_excel('2018 Data.xlsx')
df <- data.frame(data2018)
head(df)

plt <- ggplot(data = df, mapping = aes(x = Mean.Family.Income, y=Perc.with.health.Insurance))

plt + geom_point(color="darkgreen", shape="+", size=df$Unemployment.Rate) +
  labs(title="Avg Household Income & Health Insurance Rate in the US",
       x = "Mean Household Income",
       y = "Percentage of US Population with Health Insurance") +
  theme(plot.title = element_text(hjust = 0.5, size=16)) 

ggsave("Question1_Visualization.jpg")



