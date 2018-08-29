library(tidyverse)
#library(networkD3)
library(sankeyD3)

dat = read_csv('edu_vet.csv')

nodes = data.frame(name=unique(c(dat$from, dat$to)))
# reorder the nodes
nodes$name = c('University','Private provider','Other','TAFE','Community provider',
                 'Enterprise provider','University (VET)','School',
                 'F4','F6','F1','F2','F3','F','F5',
                 'M4','M6','M1','M2','M3','M','M5')
dat$source = match(dat$from, nodes$name) - 1
dat$target = match(dat$to, nodes$name) - 1

colors = paste(dat$colors, collapse = '", "')
colorJS = paste('d3.scaleOrdinal(["', colors, '"])')


sankeyNetwork(Links = dat, Nodes = nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              fontSize = 11, nodeWidth = 40, LinkGroup = 'to', 
              colourScale = colorJS, showNodeValues = FALSE, dragY = TRUE,dragX = TRUE,
               iterations = 0)

