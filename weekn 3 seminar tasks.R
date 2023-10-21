data<-read.csv("wk03.csv")

vector1<-select(data, Entity, Year, injured_all_disasters, deaths_all_disasters, homeless_all_disasters)
vector1<-vector1%>%rename(injuries=injured_all_disasters, deaths=deaths_all_disasters, homeless=homeless_all_disasters)
vector1

#run three tables for each disaster to find the highest average homeless
data_homeless<-select(data, Entity, Year,homeless_all_disasters)
table_for_homeless<-dplyr::arrange(data_homeless, desc(homeless_all_disasters))
table_for_homeless

#table for deaths caused by all disasters
data_deaths<-select(data, Entity, Year, deaths)
table_for_deaths<-dplyr::arrange(data_deaths, desc(deaths_all_disasters))
table_for_deaths

#table for injuries caused by all disasters
injured_data<-select(data, Entity, Year, injured_all_disasters)
table_for_injured<-dplyr::arrange(injured_data, desc(injured_all_disasters))
table_for_injured

#a new binary variable
library(dplyr)
#vector1<-vector1%>%mutate(indicator = deaths)
#vector1
#vector1$indicator<-ifelse(vector1$deaths>500, 'yes','no')
#vector1
data<-data%>%mutate(higher.than500=deaths_all_disasters)
data$higher.than500<-ifelse(data$higher.than500>500, 'Yes','No')
data

#reshape and save dataset
library(pacman)
pacman::p_load(tidyverse, skimr, dplyr)
new.data<-data%>%
  pivot_longer(
    cols = starts_with("deaths"),
    names_to = "deaths information",
    values_to = "number",
    values_drop_na = TRUE
  )
write.csv(new.data, file = "new.data.csv", row.names = FALSE)
new.data
newdata<-read.csv("new.data.csv")
#make vector1 wider
vector2<-vector1%>%
  pivot_wider(
    names_from = Entity,
    values_from = deaths+injuries+homeless
  )
vector2


