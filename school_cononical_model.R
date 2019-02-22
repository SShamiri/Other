library(tidyverse)
library(visNetwork)
require(shiny)
library(sankeyD3)

Nodes = read_excel('J:\\My Documents\\NEEB\\tmp\\cononical_model_shools.xlsx',sheet = 'nodes')
Edges = read_excel('J:\\My Documents\\NEEB\\tmp\\cononical_model_shools.xlsx',sheet = 'edges')

Nodes$shape = NULL
Nodes$color = NULL
visNetwork(Nodes, Edges, width = "100%") %>% 
  visGroups(groupname = 'PARTY', color = "darkblue", shape = 'icon',
            icon = list(code = 'f0c0', size = 75,color = '#ff0066')) %>% 
  visGroups(groupname = 'LOCATION', color = "pink", shape = 'icon',
            icon = list(code = 'f0ac', size = 75)) %>%
  visGroups(groupname = 'ROLE', color = "red", shape = "box",
            shadow = list(enabled = TRUE)) %>%
  visGroups(groupname = 'EVENT', color = "#090a3c", shape = "diamond",
            shadow = list(enabled = TRUE)) %>%
  visGroups(groupname = 'SERVICE', color = "#ff8000", 
            shadow = list(enabled = TRUE)) %>%
  visGroups(groupname = 'OUTCOME', shape = 'icon',
            icon = list(code = 'f192', size = 75,color = "#00cc00")) %>% addFontAwesome()
#visGroups(groupname = 'OUTCOME', color = "chartreuse", shape = "square",
#         shadow = list(enabled = TRUE)) %>% addFontAwesome()


#f19c, f0ac, f192