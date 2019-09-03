#In this demo we are going to create an R project, load libraries,
#read the data, clean data and compute the annual maximum water 
#level and examine if there is any trend. 
#We will also visualize the data in various stages of the analysis.

# Load the necessary libraries 

library(tidyverse)
library(GGally)
library(tibbletime)
library(lubridate)
library(leaflet)

# Data path
data_path <- "data/water_level.csv"
#data_path <- "https://www.dropbox.com/s/0109edsvr4fuawd/all_stages.csv?dl=1"
# Read the data 
stage <- read_csv(data_path, skip = 21,progress = T) #skip = 21

# Change variable names 
stage <- rename(stage, st.4082 = s_15_4082.csv,
         st.4086 = s_15_4086.csv,
         st.4145 = s_4145.csv,
         st.4161 = s_4161.csv, 
         st.4174 = s_4174.csv,
         st.4195 = s_4195.csv)


# Compute the annual maximum water level at each gauging station
stage <- as_tbl_time(stage, date_time)
ann_max <- stage %>% time_summarise(period = "yearly",
                          amax.4082 = max(st.4082, na.rm = T),
                          amax.4086 = max(st.4086, na.rm = T),
                          amax.4145 = max(st.4145, na.rm = T),
                          amax.4161 = max(st.4161, na.rm = T),
                          amax.4174 = max(st.4174, na.rm = T),
                          amax.4195 = max(st.4195, na.rm = T))
ann_max$year <- year(ann_max$date_time)
# Plot the data and and fit linear regression line

plot1 <- ann_max %>% gather("station", "stage", 2:7) %>% 
  ggplot(., aes(year, stage))+
  theme_bw()+
  geom_point()+
  geom_smooth(method = "lm", se=T, span=0.50,color = "blue")+ # loess (Locally Weighted Scatterplot Smoothing)
  facet_grid(station ~ ., scales = "free_y") #facet_wrap(~station)

plot1
plot1+geom_line()

fit <- lm(formula = amax.4174 ~ year, data = ann_max)
# Plot in the same graphic   
  clrs <-  colorRamps::primary.colors(5)
  ann_max %>% gather("station", "stage", 2:6) %>% 
    ggplot(.,aes(x = date_time, y = stage, colour = station))+
    theme_bw()+
    geom_line()+
    scale_color_manual(values = clrs)
    

# Summary graphic 
ggpairs(ann_max, columns = 2:6)

# Correlation matrix 
ggcorr(ann_max[, -c(1,8)],label = TRUE,
       label_alpha = TRUE)

# Mapping the stations

locations <- read_csv("data/gauges_coord.csv")

leaflet(data = locations) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(riverName), label = ~as.character(riverName))


