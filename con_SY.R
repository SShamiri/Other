library(tidyverse)
library(networkD3)
library(visNetwork)

dat = read_csv('NEEB\\ConModel.csv')
dn = data.frame(name=unique(c(dat$source, dat$target)))
dn$nodeid = 1:nrow(dn)
dat = dat %>% left_join(dn,by=c('source'='name')) %>%
        left_join(dn,by=c('target'='name')) %>%
        rename(from = nodeid.x, to = nodeid.y)


# dat$source = match(dat$from, nodes$name) - 1
# dat$target = match(dat$to, nodes$name) - 1

Node = data.frame(id = 1:nrow(dat),label = dn$name)
Node$group = c('PARTY','ROLE','EVENT','SERVICE','SERVICE',
               'ROLE','EVENT','SERVICE','PARTY','OUTCOME','LOCATION')
edge = dat %>% select(from , to) 
visNetwork(Node, edge, width = "100%") %>% 
      visOptions(highlightNearest = TRUE) %>%
      visLayout(randomSeed = 123) %>%
      visGroups(groupname = 'PARTY', color = "darkblue", shape = 'icon',
                icon = list(code = 'f0c0', size = 75)) %>% 
  # red triangle for group "B"
  visGroups(groupname = 'ROLE', color = "red", shape = "square",
            shadow = list(enabled = TRUE)) %>%  addFontAwesome() %>% 
  visEdges(arrows = "to") #%>% visConfigure(enabled = TRUE)
  visHierarchicalLayout(#direction = "RL", 
                        levelSeparation = 300)




#### end
nb <- 10
nodes <- data.frame(id = 1:nb, label = paste("Label", 1:nb),
                    #group = sample(LETTERS[1:3], nb, replace = TRUE), value = 1:nb,
                    #title = paste0("<p>", 1:nb,"<br>Tooltip !</p>"), 
                    stringsAsFactors = FALSE)

edges <- data.frame(from = c(8,2,7,6,1,8,9,4,6,2),
                    to = c(3,7,2,7,9,1,5,3,2,9),
                    #value = rnorm(nb, 10), label = paste("Edge", 1:nb),
                    title = paste0("<p>", 1:nb,"<br>Edge Tooltip !</p>"))

visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
  visOptions(highlightNearest = TRUE) %>%
  visLayout(randomSeed = 123)
