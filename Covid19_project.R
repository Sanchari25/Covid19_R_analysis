library(tidyverse)

covid_data<- read.csv("D:\\2019_nCoV_data.csv")

covid_data #To see the whole data

str(covid_data) # it shows the structure of the data
head(covid_data,10) #shows first 10 rows
tail(covid_data,10) #shows last 10 rows
summary(covid_data) #shows the range and distribution of numeric data
colSums(is.na(covid_data)) # it shows if there are any missing values

total_confirmed_cases<- sum(covid_data$Confirmed)
total_confirmed_cases # it shows the total confirmed covid cases

total_death_cases<- sum(covid_data$Deaths)
total_death_cases # it shows the total dead persons

total_recovered_cases<- sum(covid_data$Recovered)
total_recovered_cases # it shows the total number of recovered patients


# To see the total number of confirmed, death, and recovered cases all togather
library(dplyr)

covid_data %>%
  summarise(Total_confirmed= sum(Confirmed),
            Total_deaths= sum(Deaths),
            Total_recoved= sum(Recovered)) 

# To see the top 10 countries with most confirmed cases
Top_countries<- covid_data %>% group_by(Country) %>% 
  summarise(Total_confirmed= sum(Confirmed)) %>% 
  arrange(desc(Total_confirmed))  %>% slice(1:10)

Top_countries


# Bar chart of top 10 countries with most confirmed cases
ggplot(Top_countries,  aes(x = reorder(Country, -Total_confirmed), y = Total_confirmed)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(x = "Country", y = "Total Confirmed Cases", title = "Top 10 Countries by Confirmed Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Top 10 Recovered countries
Most_recovery<- covid_data%>% group_by(Country) %>%
  summarise(Total_recovered=sum(Recovered))%>%
  arrange(desc(Total_recovered))%>% slice(1:10)

Most_recovery

# Line chart of top 10 countries with most recoveries
ggplot(Most_recovery, aes(x=reorder(Country,Total_recovered), y= Total_recovered, group = 1))+
  geom_line(color="green",size=2)+
  geom_point(color="blue", size=3)+
  labs(x="Country", y="Total Recovered", title= "Top 10 countries with most recovered cases")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# To see the top 5 countries with most death cases
Most_death<- covid_data %>% group_by(Country) %>% 
  summarise(Total_death= sum(Deaths)) %>% 
  arrange(desc(Total_death))  %>% slice(1:5)
Most_death


# Point chart of top 5 countries with most death cases
ggplot(Most_death, aes(x=reorder(Country, -Total_death), y= Total_death))+
  geom_point(col="darkblue", size=4)+ 
  labs(x="Country", y="Total death", title= "Top 5 countries by most deaths")


# To see country wise death rate

Country_wise_death_rate<- covid_data %>%
  group_by(Country) %>%
  summarise(Total_confirmed= sum(Confirmed),
            Total_deaths= sum(Deaths)) %>%
  filter(Total_confirmed>0)%>% #avoiding division by 0
  mutate(Death_rate= (Total_deaths/ Total_confirmed)*100) %>% #calculating the death percentage
  arrange(desc(Death_rate))

Country_wise_death_rate

# Top 3 Countries with most deaths
top_death_rate <- Country_wise_death_rate %>%
  slice(1:3)

top_death_rate


# To see country wise recovery rate

Country_wise_recovery_rate<- covid_data%>%
  group_by(Country) %>%
  summarise(Total_confirmed= sum(Confirmed),
            Total_recovered= sum(Recovered)) %>%
  filter(Total_confirmed>0)%>% 
  mutate(Recovery_rate= (Total_recovered/ Total_confirmed)*100) %>%
  arrange(desc(Recovery_rate))

Country_wise_recovery_rate
  
# Top 5 Countries with most recoveries
top_recovery_rate <- Country_wise_recovery_rate %>%
  slice(1:5)

top_recovery_rate


covid_data$Date <- as.Date(covid_data$Date, format = "%m/%d/%Y") #To change the format of dates

covid_data <- covid_data %>% filter(!is.na(Date)) # To remove missing dates

sum(is.na(covid_data$Date)) # To check if there is any missing dates


# Global time series trend of COVID-19
covid_data%>%
  group_by(Date) %>%
  summarise(Total_confirmed= sum(Confirmed),
            Total_deaths= sum(Deaths),
            Total_recovered= sum(Recovered)) %>%
  ggplot(aes(x=Date))+ 
  geom_line(aes(y=Total_confirmed, color="Confimed"))+
  geom_line(aes(y=Total_deaths, color="Deaths"))+
  geom_line(aes(y=Total_recovered, color="Recovered"))+
  labs(x="Date", y="Count", title= "Global COVID-19 Trend Over Time")+
  scale_color_manual(values = c("blue","red","green"))


#Country wise death and recovery rate comparison
Rate_comparison<- covid_data%>%
  group_by(Country)%>%
  summarise(Total_confirmed= sum(Confirmed),
            Total_deaths= sum(Deaths),
            Total_recovered= sum(Recovered)) %>%
  filter(Total_confirmed>0)%>%
  mutate(Death_rate= (Total_deaths/Total_confirmed)*100,
        Recovery_rate=(Total_recovered/ Total_confirmed)*100)


top_5<- Rate_comparison %>%
  arrange(desc(Total_confirmed))%>%
  slice(1:5)
         
# Bar chart comparison
ggplot(top_5, aes(reorder(Country, -Total_confirmed))) +
  geom_bar(aes(y=Death_rate,fill = "Death Rate"), stat = "identity", position = "dodge")+
  geom_bar(aes(y=Recovery_rate,fill = "Recovery Rate"), stat = "identity", position = "dodge")+
  labs(x="Country", y="Rate", title= "Death Rate vs Recovery Rate in Top 5 Countries")+
  scale_fill_manual(values = c("Death Rate"= "red", "Recovery Rate"= "green"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pie chart of confirmed cases
Pie<- Top_countries

ggplot(Pie, aes(x="", y="Total_confirmed", fill = Country))+
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start = 0)+
  labs(title = "Confirmed case distribution among top 10 countries")+
  theme_void()


