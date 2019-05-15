library(tidyverse)
library(rmapshaper)
library(geojsonio)
library(sp)
library(sf)
library(leaflet)
library(readxl)

## data
elec = st_read('C:\\Users\\Samuel\\Documents\\Mapping\\electrol map and univ campus\\national-midmif-09052016\\COM_ELB.TAB')
univ = read_excel('C:\\Users\\Samuel\\Documents\\Mapping\\electrol map and univ campus\\Uni_geocoded_OSM.xlsx',sheet = 'uni_campus_local')
Comprate = read_excel('C:\\Users\\Samuel\\Documents\\Mapping\\electrol map and univ campus\\Uni_geocoded_OSM.xlsx',sheet = '6yr_completionRate')

#
completed = "Completed <br> (in any year))"
enrolled = "Still enrolled at the <br> end of the 6 year<br> cohort period"
re_enolled = "Re-enrolled,<br> but dropped out"
dropout = "Never came back <br> after the first year"

domestic_overseas = data.frame(
  Students = factor(c("Domestic","Domestic","Domestic","Domestic",
                      'Overseas','Overseas','Overseas','Overseas')),
  Cohort = factor(c(completed,enrolled,re_enolled,dropout),
                  levels=c(completed,enrolled,re_enolled,dropout)),
  Rate = c(64.4, 11.9, 15.3, 8.3,79.3, 1.5, 11.0, 8.2)
)

## simplify the shapefile
## very simple, "+init=epsg:4326" remove the small island
elec_very_simp = elec %>% 
  st_transform(crs="+init=epsg:4326") %>%
  ms_simplify(.,keep = 0.001)

names(st_geometry(elec_very_simp)) = NULL

#
pal = colorFactor(
  palette = 'Dark2',
  domain = elec_very_simp$State
)

leaflet(univ) %>% addTiles() %>% 
  addPolygons(data = elec_very_simp,
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(State),
              label = as.character(elec_very_simp$Elect_div),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addMarkers(~longitude, ~latitude, popup = ~as.character(display_name), 
             label = ~as.character(query))
######
mapdat = list(univCoord = univ, Comprate = Comprate,
              domestic_overseas = domestic_overseas, elect =elec_very_simp,
              pal = pal)


saveRDS(mapdat,'C:\\Users\\Samuel\\Documents\\AppDev\\HE_providers\\app\\data\\mapdat.rds')





#### not used
elec = st_read('C:\\Users\\Samuel\\Documents\\Mapping\\electrol map and univ campus\\national-midmif-09052016\\COM_ELB.TAB')
#univ = read_csv('C:\\Users\\Samuel\\Documents\\Mapping\\electrol map and univ campus\\Uni_geocoded_OSM.csv')
univ = read_excel('C:\\Users\\Samuel\\Documents\\Mapping\\electrol map and univ campus\\Uni_geocoded_OSM.xlsx',sheet = 'uni_campus_local')


## simplify the shapefile

# elec_simp = elec %>% 
#            st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") %>%
#            ms_simplify(.)
# ## ms_simplify() adds names to the geometry column, therefore should be deleted
# names(st_geometry(elec_simp)) = NULL
# 
# leaflet(elec_simp) %>%
#   addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
#               opacity = 1.0, fillOpacity = 0.5,
#               fillColor = ~pal(State),
#               highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                   bringToFront = TRUE))

## very simple, "+init=epsg:4326" remove the small island
elec_very_simp = elec %>% 
  st_transform(crs="+init=epsg:4326") %>%
  ms_simplify(.,keep = 0.001)

names(st_geometry(elec_very_simp)) = NULL

# leaflet(elec_very_simp) %>%
#   addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
#               opacity = 1.0, fillOpacity = 0.5,
#               fillColor = ~pal(State),
#               highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                   bringToFront = TRUE))

leaflet(univ) %>% addTiles() %>% 
  addPolygons(data = elec_very_simp,
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(State),
              label = as.character(elec_very_simp$Elect_div),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addMarkers(~longitude, ~latitude, popup = ~as.character(display_name), 
             label = ~as.character(query))
######
greenLeafIcon <- makeIcon(
  iconUrl = "https://nominatim.openstreetmap.org/images/mapicons/education_university.p.20.png",
  iconAnchorX = 22, iconAnchorY = 94
)

leaflet(univ) %>% addTiles() %>% 
  addPolygons(data = elec_very_simp,
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(State),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addMarkers(~longitude, ~latitude, popup = ~as.character(display_name), 
             label = ~as.character(query),icon = greenLeafIcon)


##
elec_df = elec %>% st_set_geometry(NULL) %>% tbl_df()
unique(elec_df[,c('Elect_div', 'State')]) %>% print(n =)
