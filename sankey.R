library(tidyverse)
library(networkD3)

dat = read_csv('edu_vet.csv')

nodes = data.frame(name=unique(c(dat$from, dat$to)))
dat$source = match(dat$from, nodes$name) - 1
dat$target = match(dat$to, nodes$name) - 1

colors = paste(dat$colors, collapse = '", "')
colorJS = paste('d3.scaleOrdinal(["', colors, '"])')


sankeyNetwork(Links = dat, Nodes = nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              fontSize = 11, nodeWidth = 40, LinkGroup = 'to', 
              colourScale = colorJS, 
               iterations = 100)

