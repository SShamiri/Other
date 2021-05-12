

national_view_ui <- function(id, label = "nat_view") {
  ns = NS(id)
  bs4TabItem(tabName = "national_tab",
             fluidRow(
               bs4Card(
                 id = "national_bx",
                 tabName = "national_tab",
                 title = "Placeholder for National weekly index", 
                 width = 12,
                 status = "primary", 
                 closable = FALSE,
                 maximizable = FALSE, 
                 collapsible = TRUE,
                 echarts4rOutput(ns("wkl_indx_plot"))
                 #p('something goes here')
               )
             ),
             fluidRow(
               bs4ValueBoxOutput(ns("indx_chg_wkl")),
               bs4ValueBoxOutput(ns("indx_chg_mth")),
               bs4ValueBoxOutput(ns("indx_chg_qtr"))
               #p('something goes here')
               
             )
    
  )
}


