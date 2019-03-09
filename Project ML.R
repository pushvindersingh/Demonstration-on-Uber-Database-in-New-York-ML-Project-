setwd("C:\\Users\\Raman\\Desktop\\Machine")
getwd()

library(ggplot2)

library(plotly) #used along with ggplot2 for data visualization.

library(ggmap)  #used for geocoding

library(plyr)   #used along with dplyr to aggregate data

library(dplyr)

# Load the .csv files
April <- read.csv("April.csv")
May <- read.csv("May.csv")
June <- read.csv("June.csv")
July <- read.csv("July.csv")
Aug <- read.csv("August.csv")
Sept <- read.csv("Sept.csv")
 
library(dplyr)

# Binding together all datasets
Data14 <- bind_rows(April , May , June , July , Aug , Sept)

summary(Data14)

# VIM library for using 'aggr'
library(VIM)

# 'aggr' plots the amount of missing/imputed values in each column
aggr(Data14)

#Lubridate makes it simple for you to identify the order in which the year, month, and day 
#appears in your dates and manipulate them.
library(lubridate)

# Separate or mutate the Date/Time columns
Data14$Date.Time <- mdy_hms(Data14$Date.Time)

Data14$Year <- factor(year(Data14$Date.Time))

Data14$Month <- factor(month(Data14$Date.Time))

Data14$Day <- factor(day(Data14$Date.Time))

Data14$Weekday <- factor(wday(Data14$Date.Time))

Data14$Hour <- factor(hour(Data14$Date.Time))

Data14$Minute <- factor(minute(Data14$Date.Time))

Data14$Second <- factor(second(Data14$Date.Time))

View(Data14)

Data14$Weekday

# Checking the first 15 entries
head(Data14 , n=15)

# Downloading the New York Map
NYCMap <- get_map("New York" , zoom = 10)

# Depicting points of Picup's in April
fullMap <- NYCMap + geom_point(data = April, aes(x = Lon, y = Lat), colour = '#000066',size = 0.1, alpha = 0.5, na.rm = TRUE) + labs(x = "Longitude", y = "Latitude", title = "Plot for all Base Codes", color = "Base Code")

fullMap 

### PLOT DATA BY UBER BASE ###
#Base B02512
base_B02512 <- ggmap(NYCMap) + 
  +     geom_point(data = uber_baseB02512, 
                   +                mapping = aes(x = Lon, y = Lat,  colour = factor(uber_baseB02512$Base)), 
                   +                alpha = 0.4, na.rm = TRUE) +
  +     scale_color_manual(values = c("#CA226B")) + 
  +     labs(x = "Longitude", y = "Latitude", title = "Plot for Base Code B02512", color = "Base Code")
base_B02512
#Base B02598
base_B02598 <- ggmap(NYCMap) + 
  +     geom_point(data = uber_baseB02598, 
                   +                mapping = aes(x = Lon, y = Lat,  colour = factor(uber_baseB02598$Base)), 
                   +                alpha = 0.4, na.rm = TRUE) +
  +     scale_color_manual(values = c("#7F462c")) + 
  +     labs(x = "Longitude", y = "Latitude", title = "Plot for Base Code B02598", color = "Base Code")
base_B02598
#Base B02617
base_B02617 <- ggmap(NYCMap) + 
  +     geom_point(data = uber_baseB02617, 
                   +                mapping = aes(x = Lon, y = Lat,  colour = factor(uber_baseB02617$Base)), 
                   +                alpha = 0.4, na.rm = TRUE) +
  +     scale_color_manual(values = c("#006600")) + 
  +     labs(x = "Longitude", y = "Latitude", title = "Plot for Base Code B02617", color = "Base Code")
base_B02617
#Base B02682
base_B02682 <- ggmap(NYCMap) + 
  +     geom_point(data = uber_baseB02682, 
                   +                mapping = aes(x = Lon, y = Lat,  colour = factor(uber_baseB02682$Base)), 
                   +                alpha = 0.4, na.rm = TRUE) +
  +     scale_color_manual(values = c("#2B60DE")) + 
  +     labs(x = "Longitude", y = "Latitude", title = "Plot for Base Code B02682", color = "Base Code")
base_B02682
#Base B02764
base_B02764 <- ggmap(NYCMap) + 
  +     geom_point(data = uber_baseB02764, 
                   +                mapping = aes(x = Lon, y = Lat,  colour = factor(uber_baseB02764$Base)), 
                   +                alpha = 0.4, na.rm = TRUE) +
  +     scale_color_manual(values = c("#008080")) + 
  +     labs(x = "Longitude", y = "Latitude", title = "Plot for Base Code B02764", color = "Base Code")
base_B02764

# Remove minutes and seconds from Date.Time and create Day column
April$Date.Time <- as.Date(April$Date.Time, "%m/%d/%Y")
April$Day <- format(as.Date(April$Date.Time, format = "%m/%d/%Y"), "%d") 

library(dplyr)
newber <- count(April, as.numeric(Day))
colnames(newber)[1] <- "Day"
colnames(newber)[2] <- "Number of Rides"

library(ggplot2)
line <- ggplot(newber, aes(x = Day, y = `Number of Rides`)) +
  geom_area(alpha = 1, position = position_dodge(width = 0.05), fill = '#0099ff') +
  xlab("Day") + ylab("Total Rides per Day") +
  ggtitle("Total Uber Rides in New York City in the month of April") + 
  theme_minimal()

library(plotly)
ggplotly(line)

set.seed(20)

# Making Clusters for all months
clusters <- kmeans(Data14[,2:3] , 5)

View(clusters)

# Save the cluster number in the dataset as column 'Borough'
Data14$Borough <- as.factor(clusters$cluster)
View(Data14)
str(clusters)

library(ggmap)

# Plotting Clusters on the Map 
ggmap(NYCMap) + geom_point(aes(x = Lon[] , y = Lat[] , colour = as.factor(Borough) , data = Data14)) + ggtitle("NYC Boroughs using KMean")

library(DT)

Data14$Month <- as.double(Data14$Month)

month_borough_14 <- count_(Data14, vars = c('Month', 'Borough'), sort = TRUE) %>% + arrange(Month, Borough)

datatable(month_borough_14)

library(dplyr)

# Predicting the Growth of Uber after considering all months Pickup's 
monthly_growth <- month_borough_14 %>% + mutate(Date = paste("04", Month)) %>% + ggplot(aes(Month, n, colour = Borough)) + geom_line() +
       +ggtitle("Uber Monthly Growth - 2014")
       
monthly_growth