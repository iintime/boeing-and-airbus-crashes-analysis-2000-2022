library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)
library(forcats) 
library(RColorBrewer)

# read csv file
df <- read_csv("Plane Crashes.csv")
# remove duplicate
df_clean <- df[!duplicated(df), ]
# create year column
df_clean$Year <- year(df_clean$Date)

#check column name
nrow(df_clean)
colnames(df_clean)

#### Question  
### 1.Which manufacturer made highest accident?
# filter for Boeing and Airbus crash (All Reason)
aircrashes_all_reason <- df_clean %>%
  filter(grepl("^Airbus A3", Aircraft) | grepl("^Boeing 7", Aircraft)) %>%
  mutate(Manufacturer = if_else(grepl("^Airbus", Aircraft), "Airbus", "Boeing")) %>%
  filter(Year >= 2000)

aircrashes_all_reason %>%
  count(Manufacturer)
# plot1 : bar_chart
ggplot(aircrashes_all_reason, aes(Year, fill=Manufacturer)) +
  geom_bar() +
  ggtitle("Airbus and Boeing Crashed Between 2000-2022") +
  theme_economist() +
  scale_fill_manual(values=c('#43766C', '#6C22A6'))
  
# plot2 : pie chart
# percentage
percentage_manufacturer <-aircrashes_all_reason %>%
  group_by(Manufacturer) %>%
  summarize(counts = n(),
            percentage = n()/nrow(aircrashes_all_reason))

percentage_manufacturer

# Plot Pie Chart
ggplot(percentage_manufacturer, aes(x="", y=percentage, fill=Manufacturer)) +
  theme_economist() +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(percentage*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18)) +
  ggtitle("Crashed by All Reason") 

### 2. Which aircraft type has the highest accident?
count_type <- head(aircrashes_all_reason %>%
                     select(Aircraft) %>%
                     group_by(Aircraft) %>%
                     summarise(counts = n()) %>%
                     arrange(desc(counts)),5)

count_type %>% 
  select(Aircraft, counts) %>% 
  mutate(Aircraft = fct_reorder(Aircraft, counts)) %>% 
  ggplot(aes(Aircraft, counts, fill=Aircraft)) +
  geom_col() +
  theme_economist() +
  coord_flip() +
  ggtitle("Top 5 Aircraft Type Crashed by Every Reason") +
  theme(legend.title=element_text(size=8)) +
  scale_fill_manual(values=c('#96E9C6', '#83C0C1', '#37B5B6', '#6962AD', '#6C22A6'))

### 3. What are the reason of accident?
crash_cause <- select(aircrashes_all_reason, Manufacturer, `Crash cause`)

# plot1 : 100% bar stack chart
ggplot(crash_cause, aes(Manufacturer, fill=`Crash cause`)) +
  geom_bar(position = "fill") +
  theme_economist() +
  ggtitle("Crash Causes") +
  scale_fill_manual(values=c('#96E9C6', '#0F1035', '#37B5B6', '#FF004D', '#FFC436', '#401F71'))

# Let's see how many case of accident that cause by technical failure?
# filter for Boeing and Airbus crash (Technical failure)
aircrashes_technical_reason <- aircrashes_all_reason %>%
  filter(grepl("Technical failure", `Crash cause`))

# percentage
percentage_thechnical_reason <-aircrashes_technical_reason %>%
  group_by(Manufacturer) %>%
  summarize(counts = n(),
            percentage = n()/nrow(aircrashes_technical_reason))

# Plot Pie Chart
ggplot(percentage_thechnical_reason, aes(x="", y=percentage, fill=Manufacturer)) +
  theme_economist() +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(percentage*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18)) +
  ggtitle("Crashed by Technical Failure") 

### 4. What aircraft type is the highest accident cause by technical failure in each manufacturer? 
## Boeing
boeing_tech_reason <- aircrashes_technical_reason %>%
  filter(grepl("^Boeing 7", Aircraft))

# plot boeing
ggplot(boeing_tech_reason, aes(Manufacturer, fill=Aircraft)) +
  geom_bar(position = "dodge")+
  theme_economist() +
  ggtitle("Boeing Crashed by Technical Failure in Each Aircraft Type") +
  scale_fill_manual(values=c('#FF204E', '#A0153E', '#5D0E41', '#FF6666',
                             '#FFA732', '#EF4040', '#C21292', '#F0997D',
                             '#F8DE22', '#F94C10', '#C70039', '#900C3F',
                             '#E25E3E'))

## Airbus
airbus_tech_reason <- aircrashes_technical_reason %>%
  filter(grepl("^Airbus A3", Aircraft))

# plot airbus
ggplot(airbus_tech_reason, aes(Manufacturer, fill=Aircraft)) +
  geom_bar(position = "dodge")+
  theme_economist() +
  ggtitle("Airbus Crashed by Technical Failure in Each Aircraft Type") +
  scale_fill_manual(values=c('#0E2954', '#1F6E8C', '#2E8A99', '#84A7A1'))
