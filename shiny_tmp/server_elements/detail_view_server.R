
detail_view_server = function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      
      sa4_filter <- reactive({
        #req(input$sa4_filter)
        filter(df_list$filters_df, sa4_name == input$sa4_filter)
      })

      observeEvent(sa4_filter(), {
        choices <- unique(sa4_filter()$anzsco4_name)
        updateSelectInput(inputId = "anzsco4_filter", choices = choices)
      })

      
      output$selected_anzsco4 <- renderText({
        input$anzsco4_filter
      })
      
      output$title_plot <- renderText({
        paste0("Employment of ", input$anzsco4_filter, " in ", input$sa4_filter)
      })

      output$sa4_anzsco4_plot <- renderEcharts4r({

        df_list$sa4_anzsco4_df  %>%
          filter(sa4_name == input$sa4_filter & anzsco4_name == input$anzsco4_filter) %>%
          e_charts(extraction_date) %>%
          e_line(original, name = "Original", legend = FALSE, color = "#1a3edd") %>%
          e_line(trend, name = "Trend", legend = FALSE, color = "#4287f5") %>%
          e_mark_area(
            itemStyle = list(normal = list(color = "#13ceb2", opacity = 0.3)),
            label = list(normal = list( color = "#000000")),
            data = list(list(xAxis = min(df_list$recent_dates)), list(xAxis = max(df_list$recent_dates)))
            ) %>%
          e_datazoom() %>%
          e_tooltip(trigger = "axis") %>%
          e_x_axis(extraction_date, axisPointer = list(show = TRUE)) %>%
          e_toolbox_feature(feature = "saveAsImage")

      })
      
      output$download_plot_df <- downloadHandler(
        filename = function() {
          paste0("dataset-", Sys.Date(), ".csv")
        },
        content = function(file) {
          tmp = df_list$sa4_anzsco4_df  %>%
            filter(sa4_name == input$sa4_filter & anzsco4_name == input$anzsco4_filter) %>%
            rename(date = extraction_date)
          
          vroom::vroom_write(tmp, file)
          
        }
      )
      
      output$download_all_df <- downloadHandler(
        filename = function() {
          paste0("dataset-", Sys.Date(), ".csv")
        },
        content = function(file) {
          vroom::vroom_write(df_list$sa4_anzsco4_df %>% rename(date = extraction_date) , file)
          
        }
      )
      
    }
  )
  
  
} 
