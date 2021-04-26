


server = function(input, output) {
  # filters --------------------------------------------------------
  state <- reactive({
    filter(df_filter, state_name == input$state_filter)
  })
  observeEvent(state(), {
    choices <- unique(state()$sa4_name)
    updateSelectInput(inputId = "sa4_filter", choices = choices) 
  })
  
  sa4 <- reactive({
    req(input$sa4_filter)
    filter(state(), sa4_name == input$sa4_filter)
  })
  observeEvent(sa4(), {
    choices <- unique(sa4()$anzsco4_name)
    updateSelectInput(inputId = "anzsco4_filter", choices = choices)
  })
  # plots -------------------------------------------------------------------
  # this is not reactive but just for fixing the plot size on the client side.
  df_sa4 <- reactive({
    filter(dat, state_name == input$state_filter, sa4_name == input$sa4_filter,anzsco4_name == input$anzsco4_filter)
  })
  
  output$sa4_plot <- renderEcharts4r({
    req(input$anzsco4_filter)
    
    df_sa4() %>% 
    e_charts(extraction_date) %>%
      e_datazoom(
        type = "slider", 
        toolbox = FALSE,
        bottom = -5
      ) %>% 
      e_tooltip() %>% 
      e_title("Selected SA4") %>% 
      e_x_axis(extraction_date, axisPointer = list(show = TRUE)) %>%
      e_line(prediction)
    
    # df_sa4() %>% 
    #   e_charts(extraction_date) %>%
    #   e_river(prediction) %>%
    #   e_tooltip(trigger = "axis") %>%
    #   e_title("River charts", "(Streamgraphs)") 
    #   #e_theme("shine")
  })

}
