library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(ggmap)    # for fortifying shapefiles
library(leaflet)


electrol = readOGR(dsn = "C:\\Users\\ss3863\\Downloads\\national-midmif-09052016\\COM_ELB.TAB", layer = "COM_ELB")
glimpse(electrol)

nsw = subset(
  x = electrol,  # our data
  subset = grepl(  # subset the data where the following pattern is matched
    x = electrol@data$State,  # in this variable in this slot of this SPDF
    pattern = "NSW"  # subset anything starting with 'E'
  )
)

map = leaflet() %>%
                 addProviderTiles(providers$OpenStreetMap)

map  # show the map

map_elect <- map %>%
  leaflet::addPolygons(
    data = electrol,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "black",  # line colour
    fillOpacity = ifelse(  # conditional fill opacity
      test = electrol@data$Area_SqKm > 51357,  # if area is over this value
      yes = 0.5,  # then make it half-opaque
      no = 0  # otherwise make it entirely transparent
    ),
    fillColor = "red",
    label = ~Elect_div  # LAD name as a hover label
  )

map_elect  # show the map


factpal <- colorFactor(topo.colors(9), electrol@data$State)

map_elect <- map %>%
  leaflet::addPolygons(
    data = electrol,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
   # color = "black",  # line colour
    fillOpacity =0.3,
    color =  ~factpal(State),
    label = ~Elect_div  # LAD name as a hover label
  )

map_elect  # show the map

#bins = c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
#pal = colorBin("YlOrRd", domain = states$density, bins = bins)


map %>% addPolygons(
  data = electrol,
  fillColor = ~factpal(State),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  label = ~Elect_div,
  highlight = highlightOptions(
    weight = 3,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE))



map_elect  # show the map




# save the map
library(htmlwidgets)
map_elect$width <- 874
map_elect$height <- 700
saveWidget(map_elect, file="map_elect.html",selfcontained = FALSE)


################### with electrol and Universities

library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(ggmap)    # for fortifying shapefiles
library(leaflet)


electrol = readOGR(dsn = "C:\\Users\\ss3863\\Downloads\\national-midmif-09052016\\COM_ELB.TAB", layer = "COM_ELB")
glimpse(electrol)

dat = read.csv('J:\\My Documents\\NEEB\\tmp\\uniMap.csv')

  leaflet(dat) %>% 
    addPolygons(
      data = electrol,  # LAD polygon data from geojson
      weight = 1,  # line thickness
      opacity = 1,  # line transparency
      color = "black",  # line colour
      fillOpacity = ifelse(  # conditional fill opacity
        test = electrol@data$Area_SqKm > 51357,  # if area is over this value
        yes = 0.5,  # then make it half-opaque
        no = 0  # otherwise make it entirely transparent
      ),
      fillColor = "red",
      label = ~Elect_div  # LAD name as a hover label
    ) %>%
    addCircleMarkers(label=~query,
                     weight = 1, 
                     radius=10, 
                     color="#ffa500")
