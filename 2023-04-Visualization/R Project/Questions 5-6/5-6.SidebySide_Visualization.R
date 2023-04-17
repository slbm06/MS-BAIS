rm(list=ls())
library(readr)
library(ggplot2)
library(dplyr)
library(ggpubr)
setwd("C:/Users/Dropbox/Education/MS-BAIS/3-Data Visualization for Story Telling/Assignment Module 9/Questions5-6")

df_2020 <- read_csv('2020 Cases only.csv')
df_2021 <- read_csv('2021 Cases only.csv')

head(df_2020)
head(df_2021)

# Clean up the columns to make 2020 match 2021 and remove unnecessary columns
df_2020_proc <- subset(df_2020,
                       select = -c(OBJECTID,Age_group,Case_,Case1,ChartDate))
names(df_2020_proc)[names(df_2020_proc) == 'ObjectId2'] <- 'ObjectId'
head(df_2020_proc)
df_2021_proc <- subset(df_2021,
                       select = -c(Age_group,Case_,Case1,ChartDate))
head(df_2021_proc)

# Combine the two dfs
df_all = rbind(df_2020_proc, df_2021_proc)
head(df_all)

# Convert the date field from char to date
class(df_all$EventDate)
df_all$EventDate <- as.Date(df_all$EventDate,"%m/%d/%Y")
class(df_all$EventDate)
head(df_all)

# Create a new df with the Counties we want to study
clean_df <- df_all[(df_all$County == 'Suwannee' |
                      df_all$County == 'Monroe' |
                      df_all$County == 'Hardee' |
                      df_all$County == 'Walton'),]

# Reordering group factor levels to control ordering of the visualization
clean_df$County <- factor(clean_df$County,      
                         levels = c("Suwannee", "Monroe", "Hardee", "Walton"))
# Check df
table(clean_df$County)

ggplot(clean_df, aes(x=EventDate)) +
  stat_count(geom = "line", color="darkblue") +
  labs(title='Daily Covid Cases for 4 Florida Counties',
       x = '',
       y = '')  +
  theme(plot.title = element_text(hjust = 0.5, color="darkblue", size=14),
        plot.background = element_rect(color="darkblue", fill = "grey90", linewidth=1),
        strip.background.x = element_rect(color="grey80")
        ) +
  facet_wrap(~County)

ggsave("Question5-6_Visualization.jpg")

