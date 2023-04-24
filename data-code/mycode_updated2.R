if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
library(tidyverse)
library(scales)
library(knitr)
library(rdrobust)
library(modelsummary)
library(AER)
library(fixest)
library(acs)

final.data<-read_tsv("data/output/acs_medicaid.txt")
brfss.data <- read_csv('data/input/BRFSS/BRFSS_Data.csv')

not_expanded<-final.data%>%
  filter(year==2012)%>%
  filter(expand_ever==FALSE)%>%
  select(State)
#view(not_expanded)
expanded_states_2014<-final.data%>%
  filter(year==2012)%>%
  filter(! State %in% c("District of Columbia", "Puerto Rico"))%>%
  filter(expand_year==2014)%>%
  select(State)
#view(expanded_states_2014)

figure_1 <- final.data %>% 
  filter(year >= 2012 & year <= 2019) %>%
  mutate(expanded = ifelse(State %in% not_expanded$State, "Not Expanded", 
                           ifelse(State %in% expanded_states_2014$State, "Expanded", NA))) %>%
  drop_na(expanded) %>%
  group_by(year, expanded) %>%
  summarize(avg_prop_uninsured = mean(uninsured / adult_pop, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_prop_uninsured, color = expanded)) + 
  geom_line() +
  labs(
    x = "Year",
    y = "Average Proportion of Uninsured",) +
  geom_vline(xintercept = 2014, color = "red") + theme_bw()+
  ylim(0, 0.25)
figure_1


brfss.data.final <- brfss.data %>%
  rename(State = Locationdesc) %>%
  select(Year, State, Class, Topic, Question, Response, Sample_Size, Data_value, Break_Out, Break_Out_Category) %>%
  filter(Topic %in% c("Overall Health", "Depression", "Last Checkup", "Diabetes", "HIV Test", "Health Care Coverage","Fair or Poor Health"))%>%
  filter(Year>=2012 & Year<=2019)


brfss_expanded<-brfss.data.final %>%
  filter(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                      "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", "Maryland", "Massachusetts", 
                      "Michigan", "Minnesota", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                      "New York", "North Dakota", "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia"))

brfss_not_expanded<-brfss.data.final %>%
  filter(State %in% c("Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                      "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                      "Texas", "Wisconsin", "Wyoming"))


health_expanded <- brfss_expanded %>%
  group_by(Year) %>%
  filter(Topic=="Fair or Poor Health" & Response=="Fair or Poor Health")%>%
  filter(Break_Out=="Overall")%>%
  summarize(health=mean(Data_value, na.rm=TRUE))
health_expanded
health_not_expanded <- brfss_not_expanded %>%
  group_by(Year) %>%
  filter(Topic=="Fair or Poor Health" & Response=="Fair or Poor Health")%>%
  filter(Break_Out=="Overall")%>%
  summarize(health=mean(Data_value, na.rm=TRUE))
health_not_expanded
table1 <- left_join(health_expanded, health_not_expanded, by = "Year")
table1
figure2<-ggplot(table1, aes(x = Year)) +
  geom_line(aes(y = health.x, color = "Expanded")) +
  geom_line(aes(y = health.y, color = "Not Expanded")) +
  labs(x = "Year", y = "Percentage with Poor/Fair Health",
      color = "") +
  scale_color_manual(values = c("Expanded" = "red", "Not Expanded" = "blue")) +theme_bw()
figure2  


depression_expanded <- brfss_expanded %>%
  group_by(Year) %>%
  filter(Topic=="Depression" & Response=="Yes")%>%
  filter(Break_Out=="Overall")%>%
  summarize(depression=mean(Data_value, na.rm=TRUE))
depression_expanded

depression_not_expanded <-brfss_not_expanded %>%
  group_by(Year) %>%
  filter(Topic=="Depression" & Response=="Yes")%>%
  filter(Break_Out=="Overall")%>%
  summarize(depression=mean(Data_value, na.rm=TRUE))
depression_not_expanded
table2 <- left_join(depression_expanded, depression_not_expanded, by = "Year")
table2
figure3<-ggplot(table2, aes(x = Year)) +
  geom_line(aes(y = depression.x, color = "Expanded")) +
  geom_line(aes(y = depression.y, color = "Not Expanded")) +
  labs(x = "Year", y = "Percent with Depression",
        color = "") +
  scale_color_manual(values = c("Expanded" = "red", "Not Expanded" = "blue"))+ theme_bw()
figure3

checkup_expanded <- brfss_expanded %>%
  group_by(Year) %>%
  filter(Topic=="Last Checkup" & Response=="Within the past year")%>%
  filter(Break_Out=="Overall")%>%
  summarize(checkup=mean(Data_value, na.rm=TRUE))
checkup_not_expanded <-brfss_not_expanded %>%
  group_by(Year) %>%
  filter(Topic=="Last Checkup" & Response=="Within the past year")%>%
  filter(Break_Out=="Overall")%>%
  summarize(checkup=mean(Data_value, na.rm=TRUE))

table3 <- left_join(checkup_expanded, checkup_not_expanded, by = "Year")
table3
figure4<-ggplot(table3, aes(x = Year)) +
  geom_line(aes(y = checkup.x, color = "Expanded")) +
  geom_line(aes(y = checkup.y, color = "Not Expanded")) +
  labs(x = "Year", y = "Percent with checkup",
    color = "") +
  scale_color_manual(values = c("Expanded" = "red", "Not Expanded" = "blue"))+ theme_bw()
figure4


table1_data<- final.data %>%
  filter(!is.na(expand_ever) | expand_year==2014) %>%
  filter(year %in% c(2012, 2015)) %>%
  group_by(year, expand_ever) %>%
  summarize(avg_prop_uninsured = mean((uninsured / adult_pop)*100, na.rm = TRUE))

prop_unisured<-table1_data%>%
  pivot_wider(names_from = year, values_from = avg_prop_uninsured) %>%
  mutate(across(`2012`:`2015`, ~percent(.01 * .)), .keep = "unused") %>%
  rename("2012" = `2012`, "2015" = `2015`) %>%
  mutate(expand_ever = ifelse(expand_ever, "Expansion", "Non-Expansion")) %>%
  select(expand_ever, "2012", "2015")
#kable(Q5_table, digits = 3, caption = "Proportion of Uninusred in 2012 and 2015 for states that expanded and did not expanded")
prop_unisured

reg.data<-final.data %>%
  mutate(post=(year>=2014), 
         treat=post*expand_ever,
         prop_uninsured=uninsured/adult_pop)%>%
  filter(! State %in% c("District of Columbia", "Puerto Rico"))%>%
  filter(is.na(expand_year) | expand_year==2014)
fe.est<-feols(prop_uninsured ~treat | State + year, data=reg.data)
modelsummary(fe.est)

#------------------------------------
hlth<-brfss.data.final%>%
  filter(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                      "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", "Maryland", "Massachusetts", 
                      "Michigan", "Minnesota", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                      "New York", "North Dakota", "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia",
                      "Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                      "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                      "Texas", "Wisconsin", "Wyoming" ))%>%
  mutate(treat=case_when(
    State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                 "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", "Maryland", "Massachusetts", 
                 "Michigan", "Minnesota", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                 "New York", "North Dakota", "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia")~1,
    State %in% c("Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                 "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                 "Texas", "Wisconsin", "Wyoming")~0,))%>%
  filter(Topic=="Fair or Poor Health" & Response=="Fair or Poor Health" & Break_Out=="Overall")%>%
  filter(Year %in% c(2012, 2015)) %>%
  mutate(post = ifelse(Year == 2015, 1, 0))
  
fe.est2 <- feols(Data_value ~ treat*post | State + Year, data = hlth)

modelsummary(fe.est2)




depres<-brfss.data.final%>%
  filter(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                      "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", "Maryland", "Massachusetts", 
                      "Michigan", "Minnesota", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                      "New York", "North Dakota", "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia",
                      "Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                      "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                      "Texas", "Wisconsin", "Wyoming" ))%>%
  mutate(treat=case_when(
    State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                 "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", "Maryland", "Massachusetts", 
                 "Michigan", "Minnesota", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                 "New York", "North Dakota", "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia")~1,
    State %in% c("Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                 "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                 "Texas", "Wisconsin", "Wyoming")~0,))%>%
  filter(Topic=="Depression" & Response=="Yes" & Break_Out=="Overall")%>%
  filter(Year %in% c(2012, 2015)) %>%
  mutate(post = ifelse(Year == 2015, 1, 0))

fe.est3 <- feols(Data_value ~ treat*post | State + Year, data = depres)

modelsummary(fe.est3)



checkup<-brfss.data.final%>%
  filter(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                      "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", "Maryland", "Massachusetts", 
                      "Michigan", "Minnesota", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                      "New York", "North Dakota", "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia",
                      "Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                      "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                      "Texas", "Wisconsin", "Wyoming" ))%>%
  mutate(treat=case_when(
    State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                 "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", "Maryland", "Massachusetts", 
                 "Michigan", "Minnesota", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                 "New York", "North Dakota", "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia")~1,
    State %in% c("Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                 "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                 "Texas", "Wisconsin", "Wyoming")~0,))%>%
  filter(Topic=="Last Checkup" & Response=="Within the past year" & Break_Out=="Overall")%>%
  filter(Year %in% c(2012, 2015)) %>%
  mutate(post = ifelse(Year == 2015, 1, 0))

fe.est4 <- feols(Data_value ~ treat*post | State + Year, data = checkup)

modelsummary(fe.est4)

#------------------------------------
hlth_1<-brfss.data.final%>%
  filter(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                      "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", "Maryland", "Massachusetts", 
                      "Michigan", "Minnesota", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                      "New York", "North Dakota", "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia",
                      "Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                      "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                      "Texas", "Wisconsin", "Wyoming" ))%>%
  mutate(treat=case_when(
    State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                 "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", "Maryland", "Massachusetts", 
                 "Michigan", "Minnesota", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                 "New York", "North Dakota", "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia")~1,
    State %in% c("Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                 "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                 "Texas", "Wisconsin", "Wyoming")~0,))%>%
  filter(Topic=="Fair or Poor Health" & Response=="Fair or Poor Health" & Break_Out=="Overall")%>%
  mutate(post = ifelse(Year == 2015, 1, 0))

plot_data <- feols(Data_value~i(Year, treat, ref=2013) | State + Year,
                   cluster=~State,
                   data=hlth_1)

event.plot<-iplot(plot_data, 
                  xlab='Time to treatment',
                  main='Event Study')

view(brfss.data %>% filter(Topic=="Last Checkup" & Locationdesc=="California" & Year==2016 & Break_Out=="Overall" ))


save.image("final_proj_data2.Rdata")
