install.packages("acs")
library(acs)
api.key.install(key='0d4e16100d9569e1014b01fc6cc5f850959c3660')
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, scales, acs, tidyr)
library(knitr)

final.data<-read_tsv("data/output/acs_medicaid.txt")
brfss.data <- read_csv('data/input/BRFSS/BRFSS_Data.csv')

#so i need to figure out the number of states that expanded 
#control group is equivalent to states that never expanded or expanded 
#we are not going to analyze any states that expanded post 2014 
not_expanded<-final.data%>%
  filter(year==2012)%>%
  filter(expand_ever==FALSE)%>%
  select(State)
not_expanded
expanded_states_2014<-final.data%>%
  filter(year==2012)%>%
  filter(expand_year==2014)%>%
  select(State)
view(expanded_states_2014)

figure_1 <- final.data %>% 
  filter(year >= 2012 & year <= 2019) %>%
  mutate(expanded = ifelse(State %in% not_expanded$State, "Not Expanded", 
                           ifelse(State %in% expanded_states_2014$State, "Expanded", NA))) %>%
  drop_na(expanded) %>%
  group_by(year, expanded) %>%
  summarize(avg_prop_uninsured = mean(uninsured / adult_pop, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_prop_uninsured, color = expanded)) + 
  geom_line() +
  labs(title = "Average Proportion of Uninsured between expanded and not expanded states from 2012-2019",
       x = "Year",
       y = "Average Proportion of Uninsured") +
  geom_vline(xintercept = 2014, color = "red") +
  ylim(0, 1)
figure_1



#importing data set

colnames(brfss.data)
view(brfss.data)

#cleaning data set
brfss.data.final <- brfss.data %>%
  rename(State = Locationdesc) %>%
  select(Year, State, Class, Topic, Question, Response, Sample_Size, Data_value) %>%
  filter(Topic %in% c("Overall Health", "Depression", "Last Checkup", "Diabetes", "HIV Test", "Health Care Coverage"))%>%
  filter(Year>=2012 & Year<=2019)

head(subset(brfss.data.final, Topic == "Last Checkup")$Response)

brfss_expanded<-brfss.data.final %>%
  filter(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", "District of Columbia", 
                 "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", "Maryland", "Massachusetts", 
                 "Michigan", "Minnesota", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                 "New York", "North Dakota", "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia"))

brfss_not_expanded<-brfss.data.final %>%
  filter(State %in% c("Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                      "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                      "Texas", "Wisconsin", "Wyoming"))



health_expanded <- brfss_expanded %>%
  group_by(Year) %>%
  summarise(poorhealth_prop = sum(Response %in% c("Poor", "Fair"), na.rm = TRUE))
health_expanded
health_not_expanded<-brfss_not_expanded%>%
  group_by(Year)%>%
  summarise(poorhealth_prop = sum(Response %in% c("Poor", "Fair"), na.rm = TRUE) / n())
health_not_expanded

table1 <- left_join(health_expanded, health_not_expanded, by = "Year")

table1


depression_expanded <-brfss_expanded %>%
  filter(Topic=="Depression")%>%
  group_by(Year)%>%
  summarise(depression_prop = sum(Response=="Yes", na.rm = TRUE))
depression_expanded

depression_not_expanded <-brfss_not_expanded %>%
  filter(Topic=="Depression")%>%
  group_by(Year)%>%
  summarise(depression_prop = sum(Response=="Yes", na.rm = TRUE))
depression_not_expanded


checkup_expanded <-brfss_expanded %>%
  filter(Topic=="Last Checkup")%>%
  group_by(Year)%>%
  summarise(depression_prop = sum(Response=="Within the past year", na.rm = TRUE))
checkup_expanded

checkup_not_expanded <-brfss_not_expanded %>%
  filter(Topic=="Last Checkup")%>%
  group_by(Year)%>%
  summarise(depression_prop = sum(Response=="Within the past year", na.rm = TRUE))
checkup_not_expanded


count(brfss.data.final, Year==2012 & Response=="Poor")

      