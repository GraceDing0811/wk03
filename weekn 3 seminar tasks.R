data<-read.csv("wk03.csv")
library(pacman)
pacman::p_load(tidyverse, dplyr, kableExtra, flextable,skimr)

#select wanted variables
selected.data<-data%>%
  select(Entity, Year, injured_all_disasters, deaths_all_disasters, homeless_all_disasters)%>%
  rename(Country=Entity, Injuries=injured_all_disasters, Deaths=deaths_all_disasters, Homeless=homeless_all_disasters)

#too complex, use group_by and summarize to deal with this question, answers:
deaths.data<-selected.data%>%select(Country, Deaths)
injuries.data<-selected.data%>%select(Country, Injuries)
homeless.data<-selected.data%>%select(Country, Year, Homeless)
#for deaths
#calculate averages
average.deaths<-deaths.data%>%
  filter(!Country %in% c("World", "Soviet Union")) %>%
  group_by(Country)%>%
  summarise(
    avg_deaths=mean(Deaths, na.rm = TRUE),
  )
#run tables
#make countries from the highest to the lowest
top10.deaths<-average.deaths%>%
  arrange(desc(avg_deaths))%>%
  head(10)%>%
  kable(caption = "Top 10 Countries by Average Deaths")
#apply formatting
table.deaths<-top10.deaths%>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)
#for injuries
average.injuries<-injuries.data%>%
  filter(!Country %in% c("World", "Soviet Union"))%>%
  group_by(Country)%>%
  summarise(
    avg_injuries=mean(Injuries, na.rm = TRUE)
  )
top10.injuries<-average.injuries%>%
  arrange(desc(avg_injuries))%>%
  head(10)%>%
  kable(caption = "Top 10 Countries by Average Injuries")
table.injuries<-top10.injuries%>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)
#for homeless
average.homeless<-homeless.data%>%
  filter(!Country %in% c("World", "Soviet Union"))%>%
  group_by(Country)%>%
  summarise(
    avg_homeless=mean(Homeless, na.rm = TRUE)
  )
top10.homeless<-average.homeless%>%
  arrange(desc(avg_homeless))%>%
  head(10)%>%
  kable(caption = "Top 10 Countries by Average Homeless")
table.homeless<-top10.homeless%>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)
#three tables
table.homeless
table.deaths
table.injuries


#a new binary variable
#vector1<-vector1%>%mutate(indicator = deaths)
#vector1
#vector1$indicator<-ifelse(vector1$deaths>500, 'yes','no')
#vector1
selected.data<-selected.data%>%mutate(higher.than500=ifelse(Deaths>500, "Yes", "No"))

#reshape and save dataset
try.data<-homeless.data%>%
  pivot_wider(
    names_from = Year,
    values_from = Homeless
  )
#final result
new.data<-selected.data%>%
  pivot_wider(
    names_from = Country,
    values_from = c(Deaths, Homeless, Injuries)
  )
# Save the final data frame as a separate R data set
saveRDS(new.data, "Wider Data.rds")






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


