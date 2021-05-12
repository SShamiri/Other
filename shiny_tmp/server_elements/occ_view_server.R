# ANZSCO 4
occ4_view_server <- function(id, tabname) {
  moduleServer(
    id,
    
    function(input, output, session) {
      # Chosen filters
      output$selected_anzsco4 <- renderText(input$anzsco4_filter)
      
      # Time series plot of aggregated ANZSCO4
      output$anzsco4_plot <- renderEcharts4r({
        df_list$agg_anzsco4_df  %>%
          filter(anzsco4_name == input$anzsco4_filter) %>%
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
      
      # Change cards
      chng_prc <- reactive({
        df_list$agg_anzsco4_df %>% 
          filter(extraction_date ==  max(extraction_date), anzsco4_name == input$anzsco4_filter) %>% 
          mutate(trd_chg_mth = trd_chg_mth*100,
                 trd_chg_qtr = trd_chg_qtr *100,
                 trd_chg_year = trd_chg_year *100,
                 trd_chg_5year = trd_chg_5year *100)
      })
      
      output$anzsco4_chng_qtr <- renderbs4ValueBox({
        bs4ValueBox(
          value = if(chng_prc()$trd_chg_qtr > 0)
            tags$h1(HTML("&uarr;"), paste0(chng_prc()$trd_chg_qtr,"%")) 
          else if(chng_prc()$trd_chg_qtr < 0) tags$h1(HTML("&darr;"), paste0(chng_prc()$trd_chg_qtr, "%"))
          else tags$h1(paste0(chng_prc()$trd_chg_qtr, "%")),
          subtitle = "Quarterly change",
          color = if(chng_prc()$trd_chg_qtr > 0) "primary" 
          else if(chng_prc()$trd_chg_qtr < 0) "danger"
          else  "gray-dark" ,
          icon = icon("chart-bar")
        )
      })
      
      output$anzsco4_chng_year <- renderbs4ValueBox({
        bs4ValueBox(
          value = if(chng_prc()$trd_chg_year > 0)
            tags$h1(HTML("&uarr;"), paste0(chng_prc()$trd_chg_year,"%")) 
          else if(chng_prc()$trd_chg_year < 0) tags$h1(HTML("&darr;"), paste0(chng_prc()$trd_chg_year, "%"))
          else tags$h1(paste0(chng_prc()$trd_chg_year, "%")),
          subtitle = "Yearly change",
          color = if(chng_prc()$trd_chg_year > 0) "primary" 
          else if(chng_prc()$trd_chg_year < 0) "danger"
          else  "gray-dark" ,
          icon = icon("chart-bar")
        )
      })
      
      output$anzsco4_chng_5year <- renderbs4ValueBox({
        bs4ValueBox(
          value = if(chng_prc()$trd_chg_5year > 0)
            tags$h1(HTML("&uarr;"), paste0(chng_prc()$trd_chg_5year,"%")) 
          else if(chng_prc()$trd_chg_5year < 0) tags$h1(HTML("&darr;"), paste0(chng_prc()$trd_chg_5year, "%"))
          else tags$h1(paste0(chng_prc()$trd_chg_5year, "%")),
          subtitle = "5-year change",
          color = if(chng_prc()$trd_chg_5year > 0) "primary" 
          else if(chng_prc()$trd_chg_5year < 0) "danger"
          else  "gray-dark" ,
          icon = icon("chart-bar"),
          #href = "#"
        )
      })
      
      # Placeholder: Heatmap
      
      
      
      # Table of SA4s for the selected ANZSCO4
      anzsco4_chng_tbl <- reactive({
        df_list$sa4_anzsco4_df %>%
          select(sa4_name,sa4_code, anzsco4_name, original, trend, trd_chg_qtr, trd_chg_year, trd_chg_5year, extraction_date) %>%
          filter(anzsco4_name == input$anzsco4_filter & extraction_date == max(extraction_date)) %>%
          select(-extraction_date, -anzsco4_name) %>%
          arrange(desc(trd_chg_qtr)) %>%
          column_to_rownames(var = "sa4_name")
      })
      
      output$occ4_tbl <- renderDataTable({
        anzsco4_chng_tbl() %>%
          select(-sa4_code) %>% 
          datatable(
            colnames = c("Region", "Original", "Trend", "Quarterly change", "Yearly change", "5-year change"),
            selection = "single",
            extensions = 'Buttons',
            options = list(scrollX = TRUE,
                           dom = 'Bfrtip',
                           buttons = c('copy', 'csv')
            )) %>%
          formatPercentage(c("trd_chg_qtr", "trd_chg_year", "trd_chg_5year"), 0)
      })
      
      # Placeholder: Selected SA4 from the table
      clicked_anzsco4_chng_tbl <- reactive({ 
        if(length(input$occ4_tbl_rows_selected) > 0) {
          tmp = rownames(anzsco4_chng_tbl())[input$occ4_tbl_rows_selected]   
        } else {
          tmp = rownames(anzsco4_chng_tbl())[1]
        }
        return(tmp) 
      })
      
      # Time series plot of ANZSCO4 and SA4
      output$sa4_anzsco4_plot <- renderEcharts4r({
        df_list$sa4_anzsco4_df  %>%
          filter(anzsco4_name == input$anzsco4_filter & sa4_name == clicked_anzsco4_chng_tbl()) %>%
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
          e_x_axis(extraction_date, axisPointer = list(show = TRUE))%>%
          e_toolbox_feature(feature = "saveAsImage")
      })
      
      # Box header - Time series aggregated ANZSCO4
      output$title_agg_anzsco4 <- renderText({
        paste0("Total employment of ", input$anzsco4_filter)
      })
      
      # Box header - Heatmap
      output$title_heatmap <- renderText({
        paste0("Quarterly change of ", input$anzsco4_filter, " across regions")
      })
      
      # Box header - Heatmap
      output$title_table <- renderText({
        paste0(input$anzsco4_filter, " across regions")
      })
      
      # Box header - Time series
      output$title_sa4_anzsco4 <- renderText({
        paste0("Employment of ", input$anzsco4_filter, " in ", clicked_anzsco4_chng_tbl())
      })
      
    
      col_vec <- reactive({
       # req(input$anzsco4_filter)
         anzsco4_chng_tbl() %>% select(sa4_code, trd_chg_qtr) %>% 
          mutate(pal = inferno(nrow(anzsco4_chng_tbl()),direction = -1)) %>% 
          arrange(sa4_code)    
        
       
      })
      
      output$anzsco4_map <- renderLeaflet({
        # Use leaflet() here, and only include base map
        leaflet() %>%
          setView(lng = 133.2016, lat = -30.16052, zoom = 4) %>%
          addTiles()%>% 
          addResetMapButton()
      })
      
      observe({
        req(tabname())
        pal  <- colorNumeric(col_vec()$pal, col_vec()$trd_chg_qtr)
      
        leafletProxy("anzsco4_map") %>%
          addPolygons(data = df_list$sa4_json,
                      color = pal(col_vec()$trd_chg_qtr),
                      weight = 1, 
                      smoothFactor = 0.5,
                      opacity = 1.0, 
                      fillOpacity = 0.5,
                      fillColor = pal(col_vec()$trd_chg_qtr),
                      label = df_list$sa4_json$sa4_name,
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) 
      })
      
      # observer to recreate the legend as needed.
      observe({
        req(tabname())
        proxy <- leafletProxy("anzsco4_map", data = df_list$sa4_json)
        # Remove any existing legend, and create a new one.
        proxy %>% clearControls()
        pal <- colorNumeric(col_vec()$pal, col_vec()$trd_chg_qtr)
        
        proxy %>% addLegend(position = "bottomright",
                            pal = pal, 
                            #values = anzsco4_chng_tbl()$trd_chg_qtr*100,
                            values = col_vec()$trd_chg_qtr,
                            title = "Qtr change",
                            #labFormat = labelFormat(suffix = "%"),
                            opacity = 0.8)
        
      })
       
      
    }
  )
}
