library(tidyverse)
library(leaflet)
library(crosstalk)
#library(geojsonio)
#library(geojsonR)



dat = read_csv("data/ncast_sample.csv") %>%
  rename(state_name = state) %>%
  mutate(extraction_date = as.Date(extraction_date, "%d/%m/%Y")) %>%
  drop_na() 


dat

df_filter = dat %>% 
  select(-prediction) %>%
  drop_na() %>% 
  distinct()

df_shared <- SharedData$new(df_fl)

# bs4DashGallery()  
#################################

sa4_json =rgdal::readOGR("data/sa4_aust.json",layer = "SA4_2016_AUST")

pal <- colorFactor(
  palette = 'Dark2',
  domain = sa4_json$SA4_NAME16
)

leaflet(sa4_json) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(sa4_json$SA4_NAME16),
              label = sa4_json$SA4_NAME16,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)
  )


