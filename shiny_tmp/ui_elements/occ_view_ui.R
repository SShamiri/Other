# ANZSCO 4
occ4_view_ui <- function(id, label = "occ4_view") {
  ns = NS(id)
  bs4TabItem(
    tabName = "occ4_tab",
    # ROW 0: INPUT SELECTION
    fluidRow(
      selectInput(
        inputId = ns("anzsco4_filter"),
        label = "Select occupation",
        choices = sort(unique(df_list$filters_df$anzsco4_name))
      )
    ),
    # ROW 1
    fluidRow(
      # Time series of aggregated ANZSCO4
      box(
        id = "occ4_agg_plot_bx",
        tabName = "occ4_tab",
        title = textOutput(ns("title_agg_anzsco4")),
        width = 5,
        status = "primary", 
        closable = FALSE,
        maximizable = FALSE, 
        collapsible = TRUE,
        echarts4rOutput(ns("anzsco4_plot"))%>% 
          withSpinner(color = "#2E3CB6")
      ),
      # Change cards for the ANZSCO4
      column(
        width = 2,
        valueBoxOutput(ns("anzsco4_chng_qtr"), width = 12),
        valueBoxOutput(ns("anzsco4_chng_year"), width = 12),
        valueBoxOutput(ns("anzsco4_chng_5year"), width = 12)
      ),
      # Heatmap of change
      box(
        id = "occ4_map_bx",
        tabName = "occ4_tab",
        title = textOutput(ns("title_heatmap")),
        width = 5,
        status = "primary", 
        closable = FALSE,
        maximizable = TRUE, 
        collapsible = TRUE,
        leafletOutput(ns("anzsco4_map")) %>% 
          withSpinner(color = "#2E3CB6")
        #p("Placeholder: Heatmap of changes across SA4 for selected ANZSCO4")
      )
    ),
    
    # ROW 2
    fluidRow(
      # Table of SA4s for the ANZSCO4
      box(
        id = "occ4_tbl_bx",
        tabName = "occ4_tab",
        title = textOutput(ns("title_table")),
        width = 6,
        status = "primary", 
        closable = FALSE,
        maximizable = FALSE, 
        collapsible = TRUE,
        dataTableOutput(ns("occ4_tbl")) %>% 
          withSpinner(color = "#2E3CB6")
      ),
      # Time series of the ANZSCO4 and SA4
      box(
        id = "occ4_plot_bx",
        tabName = "occ4_tab",
        title = textOutput(ns("title_sa4_anzsco4")),
        width = 6,
        status = "primary", 
        closable = FALSE,
        maximizable = FALSE, 
        collapsible = TRUE,
        echarts4rOutput(ns("sa4_anzsco4_plot")) %>% 
          withSpinner(color = "#2E3CB6")
      )
    )
  )
}

# ANZSCO 6
occ6_view_ui <- function(id, label = "occ6_view") {
  ns = NS(id)
  bs4TabItem(
    tabName = "occ6_tab",
    fluidRow(
      box(
        id = "occ6_bx",
        tabName = "occ6_tab",
        title = "TODO", 
        width = 12,
        status = "primary", 
        closable = FALSE,
        maximizable = FALSE, 
        collapsible = TRUE,
        p('Placeholder: Employment by ANZSCO6 goes here')
      )
    )
  )
}
