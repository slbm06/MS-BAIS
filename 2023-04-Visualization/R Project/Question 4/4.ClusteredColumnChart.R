rm(list=ls())
library(readxl)
library(ggplot2)
setwd("C:/Users/Dropbox/Education/MS-BAIS/3-Data Visualization for Story Telling/Assignment Module 9/Question 4")

carData <-  read_excel('Simplified Car Sales Data.xlsx')
df_car <- data.frame(carData)
head(df_car)

# Create a new df, keeping only the data for the 3 countries we are interested in:
ssf_df <- df_car[(df_car$CountryName  == 'France' |
                    df_car$CountryName  == 'Spain' |
                    df_car$CountryName  == 'Switzerland'),]

# Rename the CountryName column to Country:
names(ssf_df)[names(ssf_df) == "CountryName"] <- "Country"
head(ssf_df)

ggplot(ssf_df, aes(x=Color, y=round(SalePrice/1000,0), fill=Country)) +
  geom_bar(stat="summary_bin",
           fun=sum, 
           position = position_dodge2(preserve = "single")) +
  labs(title='Total Sales ($000s) in France, Spain, and Switzerland by Color ',
       x = '',
       y = '',
       fill = NULL) +
  scale_fill_discrete(type = c("royalblue","purple","darkred")) +
  theme(plot.title = element_text(size = 16,vjust = 1.5,hjust = 0.5)) + 
  scale_y_continuous(breaks=c(100,200,300,400,500,600,700)) +
  theme(axis.text.x = element_text(size = 11, angle = 90, vjust = 0.5, hjust=1))

ggsave("Question4_Visualization.jpg")
