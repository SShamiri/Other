
source("server_elements/nat_view_server.R")
source("server_elements/reg_view_server.R")
source("server_elements/occ_view_server.R")
source("server_elements/detail_view_server.R")

server <- function(input, output, session) {
  nat_view_server("nat_view")
  sa4_view_server("sa4_view") 
  occ4_view_server("occ4_view", tabname = reactive(input$tabname))
  detail_view_server("detail_view")
  
} 

