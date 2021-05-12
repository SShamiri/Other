

state_view_ui <- function(id, label = "state_view") {
  ns = NS(id)
  bs4TabItem(
    tabName = "state_tab",
    fluidRow(
      box(
        id = "state_bx",
        tabName = "state_tab",
        title = "TODO", 
        width = 12,
        status = "primary", 
        closable = FALSE,
        maximizable = FALSE, 
        collapsible = TRUE,
        p('Placeholder: Employment by State goes here')
      )
    )
  )
}

sa4_view_ui <- function(id, label = "sa4_view") {
  ns = NS(id)
  bs4TabItem(
    tabName = "sa4_tab",
    fluidRow(
      column(
        width = 4,
        box(
          id = "sa4_map_bx",
          tabName = "sa4_tab",
          title = "Select region",
          width = 12,
          status = "primary", 
          closable = FALSE,
          maximizable = TRUE, 
          collapsible = TRUE,
          leafletOutput(ns("sa4_map")) %>% withSpinner(color = "#2E3CB6")
          #p('something goes here')
        )
      ),
      column(
        width = 6,
        box(
          id = "sa4_plot_bx",
          tabName = "sa4_tab",
          title = textOutput(ns("title_agg_sa4")),
          width = 12,
          status = "primary",
          closable = FALSE,
          maximizable = TRUE,
          collapsible = TRUE,
          echarts4rOutput(ns("sa4_plot"),  width = "100%", height = "400px") %>% withSpinner(color = "#2E3CB6")
          #p('something goes here')
        )),
      column(
        width = 2,
        fluidRow(
          bs4ValueBoxOutput(ns("sa4_chng_qtr"),width = 12),
          bs4ValueBoxOutput(ns("sa4_chng_year"),width = 12),
          bs4ValueBoxOutput(ns("sa4_chng_5year"),width = 12)
        )
      )
    )
    ## -------
    ,
    fluidRow(
      box(
        id = "sa4_tbl_bx",
        tabName = "sa4_tab",
        title = textOutput(ns("title_table")),
        width = 6,
        status = "primary",
        closable = FALSE,
        maximizable = TRUE,
        collapsible = TRUE,
        dataTableOutput(ns("tbl_top_chng"))%>% withSpinner(color = "#2E3CB6")
        #p('something goes here')
      )
      ,
      box(
        id = "sa4_plot2_bx",
        tabName = "sa4_tab",
        title = textOutput(ns("title_sa4_anzsco4")),
        width = 6,
        status = "primary",
        closable = FALSE,
        maximizable = TRUE,
        collapsible = TRUE,
        echarts4rOutput(ns("sa4_chng_plot")) %>% withSpinner(color = "#2E3CB6")
        #p('something goes here')

      )
      
    )
  )
  
}
