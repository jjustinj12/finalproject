if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, scales, acs, tidyr)

#importing data set
brfss.data <- read_csv('data/input/BRFSS/BRFSS_Data.csv')
colnames(brfss.data)
view(brfss.data)

#cleaning data set
brfss.data.final <- brfss.data %>%
  rename(State = Locationdesc) %>%
  select(Year, State, Class, Topic, Question, Response, Sample_Size, Data_value) %>%
  filter(Topic %in% c("Overall Health", "Depression", "Last Checkup", "Diabetes", "HIV Test", "Health Care Coverage"))%>%
  filter(State=="Georgia")

final.data.GA<-final.data %>%
  filter(State=="Georgia")
final.data.GA
