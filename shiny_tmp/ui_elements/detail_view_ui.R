
#------------------ detial view 
detail_view_ui <- function(id, label = "detail_view") {
  ns = NS(id)
  
  bs4TabItem(
    tabName = "detial_tab",
    fluidRow(
      column(
        width = 4,
        ### filters
        selectInput(
          inputId = ns("sa4_filter"),
          label = "Select region",
          choices = unique(df_list$filters_df$sa4_name)
        ),
        selectInput(
          inputId = ns("anzsco4_filter"), 
          label = "Select occupation", 
          choices = NULL
        ),
        br(),br(),br(),
        tags$h4("Download"),
        p("The data items in the downloads are contained in CSV files."),
        downloadButton(ns("download_plot_df"), "Download selected data"),
        downloadButton(ns("download_all_df"), "Download full data")
        #textOutput(ns("selected_anzsco4"))
        #p('filters goes here')
      ),
      column(
        width = 8,
        ### plot
        box(
          #id = "detial_bx",
          id = "sa4_anzsco4_bx",
          tabName = "detial_tab",
          title = textOutput(ns("title_plot")),
          width = 12,
          status = "primary", 
          closable = FALSE,
          maximizable = FALSE, 
          collapsible = TRUE,
          echarts4rOutput(ns("sa4_anzsco4_plot"))
        )
      )
      
    ) # end row
  ) # end tab
}



