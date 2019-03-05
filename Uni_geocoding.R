library(tmap)
library(tmaptools)
library(tidyverse)
library(readxl)

dat = read_excel('Uni for geocoding.xlsx',sheet = 'Uni')
dat = dat %>% filter(!is.na(University_1))


datList = list()
for (i in 1: nrow(dat)){
  datList[[i]] = geocode_OSM(dat$University[i],return.first.only = FALSE,details = TRUE)
}


dfList = list()
for (i in 1: 41){ #length(datList)
 # i = 5
  n = length(datList[[i]][[1]])
 if(n > 1) {
   tmp = data.frame(matrix(unlist(datList[[i]]), nrow=length(datList[[i]]), byrow=T))
    names(tmp) = c('query','coords_x','coords_y', 'bbox_xmin','bbox_ymin','bbox_xmax','bbox_ymax','place_id','osm_type','osm_id','place_rank',
                 'display_name','class','type','importance','icon')
    dfList[[i]] = tmp  
    rm(tmp) } else {
      tmp = data.frame(matrix(unlist(datList[[i]]), nrow = 1, byrow=T))
      names(tmp) = c('query','coords_x','coords_y', 'bbox_xmin','bbox_ymin','bbox_xmax','bbox_ymax','place_id','osm_type','osm_id','place_rank',
                     'display_name','class','type','importance','icon')
      dfList[[i]] = tmp    
    }
}

df = bind_rows(dfList)
