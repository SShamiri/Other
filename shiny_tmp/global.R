
# Load packages
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(fresh)
library(echarts4r)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(DT)
#library(RColorBrewer)
library(viridis)
library(shinycssloaders)


# load data
df_list <- read_rds('data/shiny_df.rds')

### map 

# # sa 4 map colour
# n <- length(unique(df_list$filters_df$sa4_code))
# 
# qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

# labels
sa4_map_labels <- sprintf(
  "<strong>%s</strong><br/> Original: %g <br/> Trend: %g",
  df_list$sa4_json$sa4_name, df_list$sa4_json$original,df_list$sa4_json$trend
) %>% lapply(htmltools::HTML)





