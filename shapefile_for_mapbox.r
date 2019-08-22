##### Instruction on simplifing shapefile and adding data to it ########################

# Step 1: read the shapefile into R SA2_2011_AUST.shp

sa2 = readOGR(dsn = "PathToFile\\SA2_2011_AUST.shp",layer = "SA2_2011_AUST")
glimpse(sa2)
head(sa2@data)

sa2@data = sa2@data[,c(1:3)]

sa2@data$SA2_MAIN11 =   as.numeric(as.character(sa2@data$SA2_MAIN11))
class(sa2@data$SA2_MAIN11)

# Step 2: add the required fields to shapefile, example SEIFA 

seifa = read_excel('PathToFile\\SEIFA_SA2_2011.xlsx', sheet = 'SEIFA_Advantage_Disadvantage')
sa2@data = sa2@data %>% left_join(seifa, by= c('SA2_MAIN11' = 'SA2_code_2011')) %>% select(-SA2_Name_2011)

# Step 3: Convert to GeoJson and save the results

sa2_json = geojson_json(sa2)
geojson_write(sa2_json, file = "PathToFile\\SEIFA_SA2_2011.geojson")


# Step 3- drop the GeoJson into mapshaper (https://mapshaper.org/) Example, SEIFA_SA2_2011.geojson 
      # a- simplify the shapefile 2% 
      # b- export as Shapefile 

# Step 4- take the shapefile produced by mapshaper into MapBox

