library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(ggmap)    # for fortifying shapefiles
library(leaflet)
library(tidyverse)


electrol = readOGR(dsn = "C:\\Users\\ss3863\\Downloads\\national-midmif-09052016\\COM_ELB.TAB", layer = "COM_ELB")
glimpse(electrol)

campus = read.csv('J:\\My Documents\\NEEB\\tmp\\uniMap.csv')


nsw = subset(
  x = electrol,  # our data
  subset = grepl(  # subset the data where the following pattern is matched
    x = electrol@data$State,  # in this variable in this slot of this SPDF
    pattern = "NSW"  # subset anything starting with 'E'
  ))

campus %>% filter(state=='NSW') %>% leaflet() %>% addTiles() %>% 
  addPolygons(
    data = nsw,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "black",  # line colour
#    fillOpacity = ifelse(  # conditional fill opacity
#      test = electrol@data$Area_SqKm > 51357,  # if area is over this value
#      yes = 0.5,  # then make it half-opaque
#      no = 0  # otherwise make it entirely transparent
#    ),
    fillColor = "red",
    label = ~Elect_div  # LAD name as a hover label
  ) %>%
  
  addMarkers(~longitude, ~latitude, popup = ~as.character(Street.address), label = ~as.character(query))
  
  
  
  addCircleMarkers(label=~query,
                   weight = 1, 
                   radius=2, 
                   color="#ffa500")

  
  
dim(electrol@data)  
