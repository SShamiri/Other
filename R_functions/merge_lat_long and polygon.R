library(rgdal)     
library(tidyverse)
library(readxl)
library(maptools)

sa2 = readOGR(dsn = "J:\\My Documents\\geospatial data\\SA2_2016_AUST\\SA2_2016_AUST.shp", layer = "SA2_2016_AUST")
glimpse(sa2)

head(sa2@data)
coordinates(sa2)[1:5, ]
proj4string(sa2) 


dat = read_excel('J:\\My Documents\\geospatial data\\External Data Request - 2019 - NQS Data Q4 2018 with latitude and longitude.XLSX',
                 sheet = 'Approved Services')
# filter out zeros
dat1 = dat %>% filter(Longitude != 0 | Latitude != 0)

# load the data into SpatialPointsDataFrame
dat2 = SpatialPointsDataFrame(coords = cbind(dat1$Longitude,dat1$Latitude),
                                      data = dat1,
                                      proj4string = CRS("+proj=longlat"))

#make sure the shapefile has the same projection as our spatial point data frame (dat2)
sa2_2 = spTransform(sa2, CRS('+proj=longlat'))
 
# for every lat long, I want to get information about their polygon. Return the row number of the observation in Data 2 (Sa2_2) that intersects with any given observation in Data 1 (dat2)               
joinDat = over(dat2, sa2_2)                    
joinDat[1:5, ]

# merge the two spatial objects
joinDat2 = spCbind(dat2, joinDat)
names(joinDat2)

dim(joinDat2@data)
dim(dat1)

write_csv(joinDat2@data, 'merge_polygon_and_latlong.csv')
