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
count(brfss.data.final)

brfss_expanded<-brfss.data.final %>%
  filter(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", "District of Columbia", 
                 "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", "Maryland", "Massachusetts", 
                 "Michigan", "Minnesota", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                 "New York", "North Dakota", "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia"))
count(brfss_expanded)

brfss_not_expanded<-brfss.data.final %>%
  filter(State %in% c("Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                      "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                      "Texas", "Wisconsin", "Wyoming"))


#So i filtered sample size to keep between IQR to not let outliers effect data 
health_expanded <- brfss_expanded %>%
  group_by(Year) %>%
  filter(Topic=="Overall Health")%>%
  filter(Sample_Size>=30 & Sample_Size<=600)%>%
  summarise(poorhealth_prop = sum(Response %in% c("Poor"), na.rm = TRUE) / n())
  
health_expanded  
  
ggplot(data=health_expanded, aes(x=as.factor(Year), y=poorhealth_prop))+ 
  geom_bar(stat="identity") + labs(
    x= "Year",
    y= "Proportion of people with poor health",
    title ="Proportion of people with poor health for Expanded States"
  ) + scale_y_continuous(labels=comma) +
  theme_bw()


health_not_expanded<-brfss_not_expanded%>%
  group_by(Year)%>%
  filter(Topic=="Overall Health")%>%
  filter(Sample_Size>=30 & Sample_Size<=600)%>%
  summarise(poorhealth_prop = sum(Response %in% c("Poor"), na.rm = TRUE) / n())
health_not_expanded
ggplot(data=health_not_expanded, aes(x=as.factor(Year), y=poorhealth_prop))+ 
  geom_bar(stat="identity")+ labs(
    x= "Year",
    y= "Proportion of people with poor health",
    title ="Proportion of people with poor health for Non-Expanded States"
  ) + scale_y_continuous(labels=comma) +
  theme_bw()

table1 <- left_join(health_expanded, health_not_expanded, by = "Year")
table1
figure2<-ggplot(table1, aes(x = Year)) +
  geom_line(aes(y = poorhealth_prop.x, color = "Expanded")) +
  geom_line(aes(y = poorhealth_prop.y, color = "Not Expanded")) +
  labs(x = "Year", y = "Proportion with Poor Health",
  title ="Proportion of people with poor health", color = "") +
  scale_color_manual(values = c("Expanded" = "red", "Not Expanded" = "blue")) +theme_bw()
figure2

depression_expanded <- brfss_expanded %>%
  group_by(Year) %>%
  filter(Topic=="Depression")%>%
  filter(Sample_Size>=30 & Sample_Size<=600)%>%
  summarise(d_prop = sum(Response=="Yes", na.rm = TRUE) / n())
depression_expanded

depression_not_expanded <-brfss_not_expanded %>%
  group_by(Year) %>%
  filter(Topic=="Depression")%>%
  filter(Sample_Size>=30 & Sample_Size<=600)%>%
  summarise(d_prop = sum(Response=="Yes", na.rm = TRUE) / n())
depression_not_expanded

table2 <- left_join(depression_expanded, depression_not_expanded, by = "Year")
table2
figure3<-ggplot(table2, aes(x = Year)) +
  geom_line(aes(y = d_prop.x, color = "Expanded")) +
  geom_line(aes(y = d_prop.y, color = "Not Expanded")) +
  labs(x = "Year", y = "Proportion with Depression",
       title ="Proportion of people with depression", color = "") +
  scale_color_manual(values = c("Expanded" = "red", "Not Expanded" = "blue"))+ theme_bw()
figure3


checkup_expanded <- brfss_expanded %>%
  group_by(Year) %>%
  filter(Topic=="Last Checkup")%>%
  filter(Sample_Size>=30 & Sample_Size<=600)%>%
  summarise(c_prop = sum(Response=="Within the past year", na.rm = TRUE) / n())
checkup_expanded

checkup_not_expanded <-brfss_not_expanded %>%
  group_by(Year) %>%
  filter(Topic=="Last Checkup")%>%
  filter(Sample_Size>=30 & Sample_Size<=600)%>%
  summarise(c_prop = sum(Response=="Within the past year", na.rm = TRUE) / n())
checkup_not_expanded

table3 <- left_join(checkup_expanded, checkup_not_expanded, by = "Year")
table3
figure4<-ggplot(table3, aes(x = Year)) +
  geom_line(aes(y = c_prop.x, color = "Expanded")) +
  geom_line(aes(y = c_prop.y, color = "Not Expanded")) +
  labs(x = "Year", y = "Proportion with checkup",
       title ="Proportion of people with checkup wihtin the last year", color = "") +
  scale_color_manual(values = c("Expanded" = "red", "Not Expanded" = "blue"))+ theme_bw()
figure4

#Time to show some Differences 
expanded_1<-final.data%>%
  filter(year==2012)%>%
  filter(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                      "District of Columbia", "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", 
                      "Maryland", "Massachusetts", "Michigan", "Minnesota", "Nevada", 
                      "New Hampshire", "New Jersey", "New Mexico", 
                            "New York", "North Dakota", 
                      "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia"))%>%
  summarize(avg_prop_uninsured_1 = mean(uninsured / adult_pop, na.rm = TRUE))
expanded_1
not_expanded_1<-final.data%>%
  filter(year==2012)%>%
  filter(State %in% c("Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                      "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                      "Texas", "Wisconsin", "Wyoming"))%>%
  summarize(avg_prop_uninsured_1 = mean(uninsured / adult_pop, na.rm = TRUE))
not_expanded_1

expanded_2<-final.data%>%
  filter(year==2015)%>%
  filter(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                      "District of Columbia", "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", 
                      "Maryland", "Massachusetts", "Michigan", "Minnesota", "Nevada", 
                      "New Hampshire", "New Jersey", "New Mexico", 
                      "New York", "North Dakota", 
                      "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia"))%>%
  summarize(avg_prop_uninsured_2 = mean(uninsured / adult_pop, na.rm = TRUE))
expanded_2
not_expanded_2<-final.data%>%
  filter(year==2015)%>%
  filter(State %in% c("Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                      "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                      "Texas", "Wisconsin", "Wyoming"))%>%
  summarize(avg_prop_uninsured_2 = mean(uninsured / adult_pop, na.rm = TRUE))
not_expanded_2



table_data <- matrix(nrow = 2, ncol = 2, dimnames = list(c("Baseline", "After Treatment"), c("Expanded", "Not Expanded")))

# Add values to the table
table_data[1,1] <- expanded_1$avg_prop_uninsured_1
table_data[2,1] <- expanded_2$avg_prop_uninsured_2
table_data[1,2] <- not_expanded_1$avg_prop_uninsured_1
table_data[2,2] <- not_expanded_2$avg_prop_uninsured_2

# Print the table
kable(table_data, digits = 2, caption = "Proportion of Uninusred before 2014 and after 2014 for states that were expanded and not expanded")



regression<-final.data%>%
  filter(year==2012 | year==2015)%>%
  filter(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                     "District of Columbia", "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", 
                     "Maryland", "Massachusetts", "Michigan", "Minnesota", "Nevada", 
                     "New Hampshire", "New Jersey", "New Mexico", 
                     "New York", "North Dakota", 
                     "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia",
                     "Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                     "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                     "Texas", "Wisconsin", "Wyoming"))%>%
  mutate(treatment=ifelse(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                      "District of Columbia", "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", 
                      "Maryland", "Massachusetts", "Michigan", "Minnesota", "Nevada", 
                      "New Hampshire", "New Jersey", "New Mexico","New York", "North Dakota", 
                      "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia"), 
         "Treatment", "Control"))%>%
    mutate(time=ifelse(year == 2012, "Pre", "Post"))

model <- lm(uninsured ~ treatment * year, data = regression)
head(regression)

install.packages("causaldata")
library(causaldata)
reg.dat <- causaldata::gapminder %>%
  mutate(lgdp_pc=log(gdpPercap)) %>%
  group_by(country) %>%
  mutate(demean_lifeexp=lifeExp - mean(lifeExp, na.rm=TRUE),
         demean_gdp=lgdp_pc - mean(lgdp_pc, na.rm=TRUE))
lm(demean_lifeexp~ 0 + demean_gdp, data=reg.dat)


rm(list=c("full.ma.data", "contract.service.area", "ma.pentration.data", 
          "plan.premiums", "final.plans", "final.data", "fig.avg.enrollment"))
save.image("Hwk1_workspace.Rdata")
