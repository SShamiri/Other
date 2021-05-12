#------------------ method view 
method_view_ui <- function(id, label = "method_view") {
  ns = NS(id)
  bs4TabItem(
    tabName = "method_tab",
    fluidRow(
      p('We followed a four step process to carry out this project: collect and transform input data, feature selection, modelling and validation.
      The details of these steps are further defined below.')
    ),
    fluidRow(
      bs4Callout(
        title = tags$h4("Collect and transform input data"),
        elevation = 2,
        status = "info",
        width = 3,
        tags$ol(
          tags$li("Confirm release dates and reference periods for all data"), 
          tags$li("Concord all data to the same occupational (ANZSCO 4-digit) and regional (SA4) basis"), 
          tags$li("Remove NFDs & other territories"),
          tags$li("Impute missing values where necessary"),
          tags$li("Smooth and trend the data where necessary (seasonal adjustment and Hodrick-Prescott trending)"),
          br(),
          br()
        )
      ),
      bs4Callout(
        title = tags$h4("Feature selection"),
        elevation = 3,
        status = "info",
        width = 3,
        #height = 1080,
        tags$li("Build features (variables) to use in the model"),
        tags$li("Test and choose the most predictive model inputs"),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br()
      ),
      bs4Callout(
        title = tags$h4("Modelling"),
        elevation = 4,
        status = "info",
        width = 3,
        tags$li("Determine best target variable (ABS LFS custom data or ABS Census) depending on underlying data sparcity"),
        tags$li("Test different variations of types of models, model parameters, and features (variables) for best performance"),
        tags$li("Build and test three types of models: Random Forest; Gradient Booster; Linear Regression"),
        tags$li("Split the model into training periods and test periods"),
        tags$li("Test combinations of models (stack)"),
        br()
      ),
      bs4Callout(
        title = tags$h4("Validation"),
        elevation = 5,
        status = "success",
        width = 3,
        tags$li("Review model diagnostics, performance metrics, and predictions"),
        tags$li("Compare model outputs with other sources, such as ABS LFS data"),
        tags$li("Return to previous steps as necessary to improve the model"),
        br(),
        br(),
        br(),
        br(),
        br(),
        br()
      )
    ),
    fluidRow(
      bs4Card(
        title = tags$h4("Overview of modelling training, validation and stacking process"),
        status = "primary",
        solidHeader = TRUE,
        headerBorder = FALSE,
        background = NULL,
        width = 12,
        collapsible = FALSE,
        closable = FALSE,
        fluidRow(
          column(width = 6,
                 p("We chose a stacked modelling approach in this project. The final stacked  meta-model combine (stack) 
                   the prediction based on different machine learning models. 
                   The driving principle of stacked ensemble methods is to combine the predictions of several base estimators 
                   to improve robustness and generalisability over a single predictor."),
                 p('More details on the methodology can be found', a(href = 'www','here'), ".")
                 ),
          column(
            width = 6,
            div(img(src="model_process.png")))
          )
        )
      
    )
  )
}
