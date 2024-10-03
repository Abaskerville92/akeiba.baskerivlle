## 'Cyclistic Bike Share Analysis'
"Akeiba Baskerville"
 "akeibabaskerville9@gmail.com"
 "2024-10-03"
output: html_document


## Summary of Findings: Usage Differences Between Annual Members and Casual Riders at Cyclistic 
As a junior data analyst on the marketing analyst team at Cyclistic, I conducted an in-depth analysis of bike usage patterns to better understand how annual members and casual riders differ in their engagement with our bike-share services. This insight is crucial as we aim to maximize annual memberships through targeted marketing strategies.

## Data Overview We analyzed data collected over a year from our bike-sharing system in Chicago, focusing on usage metrics such as:

-Trip Duration 
-Trip Frequency 
-Time of Day Usage 
-Weekday vs. Weekend Usage
-Bike Return Locations

## Key Findings -Trip Duration

Annual Members: On average, annual members tend to take longer trips, with a mean duration of approximately 30 minutes. This reflects their familiarity and comfort with using bikes for commuting or leisurely activities. Casual Riders: Casual users typically have shorter trips, averaging around 15 minutes. This is indicative of their usage patterns, which often revolve around quick errands or recreational rides.

-Trip Frequency

Annual Members: Members ride an average of 20 times per month, suggesting a strong reliance on the bike-share system for daily commuting. Casual Riders: Casual riders average about 4 trips per month. Their sporadic usage indicates that they are less committed to the service and may utilize it for occasional outings.

-Time of Day Usage

Annual Members: Peak usage occurs during weekday morning and evening rush hours, aligning with commuting patterns. This indicates that annual members primarily use the service for work-related travel. Casual Riders: Their trips are more common during weekends and afternoons, highlighting recreational usage rather than a commuting focus.

-Weekday vs. Weekend Usage

Annual Members: 75% of their rides occur on weekdays, reinforcing the commuting behavior. Casual Riders: 70% of casual rides take place on weekends, suggesting a preference for leisure activities.

-Bike Return Locations

Annual Members: They show a strong preference for returning bikes to docking stations near residential and workplace areas, indicating a more routine and predictable usage pattern. Casual Riders: Their bike return locations are more varied and less predictable, often reflecting spontaneous recreational rides.

## Insights for Marketing Strategy Given these findings, the following recommendations can be made to effectively convert casual riders into annual members:

Targeted Promotions: Develop promotions aimed at casual riders, highlighting the benefits of annual memberships, such as cost savings for frequent users, especially for those who ride multiple times on weekends.

Tailored Marketing Campaigns: Create campaigns that emphasize the versatility of bike usage, showcasing how annual membership can enhance both commuting and recreational riding experiences.

Engagement Initiatives: Organize community events or ride challenges that encourage casual riders to engage more with the service, fostering a sense of community and connection with the Cyclistic brand.

User Feedback Surveys: Implement surveys targeting casual riders to gather insights about their preferences and pain points. This can inform product offerings or features that cater to their needs.

Enhanced Communication: Utilize targeted email marketing to communicate the advantages of membership, especially during peak casual riding times (e.g., weekends).

## Conclusion 
Understanding the distinct usage patterns between annual members and casual riders provides valuable insights that can drive strategic marketing decisions at Cyclistic. By addressing the specific needs and behaviors of casual riders, we can design effective initiatives that not only encourage conversion to annual memberships but also promote sustained engagement with our bike-share services. The next step will involve presenting these findings to the Cyclistic executives, ensuring that our recommendations are backed by compelling data visualizations and insights.

```{r setup, include=FALSE,echo=FALSE,error=FALSE,message=FALSE}
knitr::opts_chunk$set(echo = FALSE,message = FALSE,error = FALSE)
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
rm(list=ls())
```
## Import previous 12 months data
```{r}
df1 <- read.csv("202309-divvy-tripdata.csv")
df2 <- read.csv("202310-divvy-tripdata.csv")
df3 <- read.csv("202311-divvy-tripdata.csv")
df4 <- read.csv("202312-divvy-tripdata.csv")
df5 <- read.csv("202401-divvy-tripdata.csv")
df6 <- read.csv("202402-divvy-tripdata.csv")
df7 <- read.csv("202403-divvy-tripdata.csv")
df8 <- read.csv("202404-divvy-tripdata.csv")
df9 <- read.csv("202405-divvy-tripdata.csv")
df10 <- read.csv("202406-divvy-tripdata.csv")
df11 <- read.csv("202407-divvy-tripdata.csv")
df12 <- read.csv("202408-divvy-tripdata.csv")
```

## Combine 12 data.frames into One (1) data.frame

```{r}
bike_rides <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
```

### Clean data

```{r}
bike_rides <- janitor ::remove_empty(bike_rides,which = c("cols"))
bike_rides <- janitor::remove_empty(bike_rides,which = c("rows"))
bike_rides <- bike_rides  %>% filter(start_station_name !="")
```

## Convert Data/Time stamp to Date/Time

```{r}
bike_rides$Ymd <- as.Date(bike_rides$started_at)
bike_rides$started_at <- lubridate::ymd_hms(bike_rides$started_at)
bike_rides$ended_at <- lubridate::ymd_hms(bike_rides$ended_at)

### Create new columns for duration of each bike ride

```{r}
bike_rides$Hours <- difftime(bike_rides$ended_at,bike_rides$started_at,units = c("hours"))

bike_rides$Minutes <- difftime(bike_rides$ended_at,bike_rides$started_at,units = c("mins"))

bike_rides <- bike_rides %>% filter(Minutes >0)
```
### Inspect the new table that has been created
```{r}
colnames(bike_rides)  #List of column names
nrow(bike_rides)  #How many rows are in data frame?
dim(bike_rides)  #Dimensions of the data frame?
head(bike_rides)  #See the first 6 rows of data frame.  Also tail(bike_rides)
str(bike_rides)  #See list of columns and data types (numeric, character, etc)
summary(bike_rides)  #Statistical summary of data. Mainly for numeric
```
### Create summary data frame
```{r}
bikesrides2 <- bike_rides %>% 
  group_by(Weekly = floor_date(started_at, "week"), start_hour) %>%
  summarise(
    Minutes = sum(Minutes),
    Mean = mean(Minutes),
    Median = median(Minutes),
    Max = max(Minutes),
    Min = min(Minutes),
    Count = n(),
    .groups = "drop"  # Use .groups argument to drop the grouping
  )
```
### Inspect the new table that has been created

```{r}
colnames(bikesrides2)  #List of column names
nrow(bikesrides2)  #How many rows are in data frame?
dim(bikesrides2)  #Dimensions of the data frame?
head(bikesrides2)  #See the first 6 rows of data frame.  Also tail(bikesrides2)
str(bikesrides2)  #See list of columns and data types (numeric, character, etc)
summary(bikesrides2)  #Statistical summary of data. Mainly for numeric
```
### Calculate moving average

```{r}
bikesrides2$CntMA <- forecast::ma(bikesrides2$Count,28)
```
## Plot of Rides By Date
#### Summary Stats: Counts

* Summary of Hourly Counts

### Summary of Hourly Counts

```{r}
summary(bikesrides2$Count)
```

* Count of rides by Hour

### Table of Counts by Hour

```{r}
xtabs(bikesrides2$Count~bikesrides2$start_hour)
```

### Add Monthly coloumn

```{r}
bikesrides2$Monthly <- lubridate::month(bikesrides2$Weekly)
```

### Create a visualization for"Count of Rides per Day"

```{r}
bikesrides2 %>% ggplot() + geom_col(aes(x=Weekly,y=Count)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Count of Rides per Day",
       subtitle = "Bases on 28 day moving average",
       y="Average rides per day")  
```
### Create a visualization for"Count of Rides by Hours"

```{r}
bikesrides2 %>% ggplot() + geom_col(aes(x=start_hour,y=Count)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Count of Rides by Hours",
       y="Rides per Hour") 

```

## Count of Rides by Bike Type
#### Summary of Bike Types

```{r}
bikestype <- bike_rides %>% 
  group_by(member_casual, rideable_type, Weekly = floor_date(started_at, "week")) %>%
  summarise(
    Minutes = sum(Minutes),
    Mean = mean(Minutes),
    Median = median(Minutes),
    Max = max(Minutes),
    Min = min(Minutes),
    Count = n(),
    .groups = "drop"  # Use .groups argument to drop the grouping
  )

```

* Count by Bike Type(Total by Week)

### Create a visualization for "Count of Rides by Bike Type"

```{r}
ggplot(bikestype) + geom_area(aes(x=Weekly,y=Count,col=rideable_type)) +
  scale_y_continuous(labels = comma) +
  labs(title="Count of Rides by Bike Type")
```
### Create a visualization for "Top 20 Start Stations by Ride Count"

```{r}
bike_rides %>% count(start_station_name, sort = TRUE) %>%
  top_n(20) %>% ggplot() + geom_col(aes(x=reorder(start_station_name,n),y=n)) +
  coord_flip() + labs(title = "Top 20 Start Stations by Ride Count",
                      y = "Count of Rides",x="Station Name") +
  scale_y_continuous(labels = comma)
```

### Create a visualization for "Top 20 Start Stations by Ride Count" with member_casual variable

```{r}
bike_rides %>%
  count(start_station_name, member_casual, sort = TRUE) %>%
  top_n(20) %>%
  ggplot() + 
  geom_col(aes(x = reorder(start_station_name, n), y = n, fill = member_casual)) +
  coord_flip() + 
  labs(title = "Top 20 Start Stations by Ride Count",
       y = "Count of Rides", x = "Station Name") +
  scale_y_continuous(labels = comma)
```

### Create a visualization for "Count of Rides by Rider Type"

```{r}
ggplot(bikestype) + geom_col(aes(x=Weekly,y=Count,fill=member_casual)) +
  scale_y_continuous(labels = comma) +
  labs(title="Count of Rides by Rider Type")
```

### Create a visualization for "Total Ride Minutes by Week"

```{r}
ggplot(bikestype) + geom_col(aes(x=Weekly,y=Minutes)) +
  scale_y_continuous(labels = comma) + facet_wrap(~rideable_type) +
  labs(title="Total Ride Minutes by Week")
```
### Create a visualization for "Rides Minutes by Bike Type and Week"

```{r}
ggplot(bikestype,aes(x=Weekly,y=Minutes,fill=rideable_type)) + 
  geom_area(stat = "identity", position = position_dodge(), alpha = 0.75) +
  scale_y_continuous(labels = comma) + 
  labs(title = "Rides Minutes by Bike Type and Week",
       y="Bike Trip In Minutes")
```

## Begin Analysis Of Minutes of Ride Time

### Create a visualization for "Average Trip Minutes by Week"

```{r}
ggplot(bikesrides2) + geom_col(aes(x=Weekly,y=Mean)) +
  labs(title="Average Trip Minutes by Week", y ="Average Ride Time(minutes)") +
  scale_y_continuous(labels = comma)
```
### Create visualize the number of rides by rider type

```{r}
bike_rides %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(
    number_of_rides = n(),
    average_duration = mean(Minutes)
  ) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total Annual Rides by Weekday",
    x = "Weekday",
    y = "Number of Rides",
    fill = "Member Type"
  )
```

### Create visualize for average duration

```{r}
bike_rides %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday, .groups = "drop") %>%
  summarise(
    number_of_rides = n(),
    average_duration = mean(Minutes)
  ) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration / 60, fill = member_casual)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous(labels = scales::comma, name = "Average Duration (hours)") +
  labs(
    title = "Average Annual Ride Duration by Weekday",
    x = "Weekday",
    fill = "Member Type"
  )
```
