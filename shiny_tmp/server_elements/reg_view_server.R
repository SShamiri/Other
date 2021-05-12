
sa4_view_server = function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      ### map
      output$sa4_map = renderLeaflet({
        leaflet() %>%
          setView(lng = 133.2016, lat = -30.16052, zoom = 4) %>%
          addTiles() %>%
          addPolygons( data = df_list$sa4_json,
                       color = "#444444", weight = 1, smoothFactor = 0.5,
                       opacity = 1.0, fillOpacity = 0.5,
                       #fillColor = col_vector,
                       fillColor = ~df_list$sa4_json$pal,
                       label = sa4_map_labels,
                       highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                       layerId = ~df_list$sa4_json$sa4_code) %>% 
          addResetMapButton()
        
      })
      
      # collect click info
      clicked_sa4_df <- reactive({ 
        if(length(input$sa4_map_shape_click$id) > 0) {
          tmp = df_list$sa4_json@data %>% filter(sa4_code == input$sa4_map_shape_click$id) %>% pull(sa4_name)
        } else {
          tmp = "Capital Region"
        }
        return(tmp) 
      }) 
      
     ## map-box header 
     output$selected_sa4 <- renderText({ clicked_sa4_df()})
      
    ## sa4 agg plot
     output$sa4_plot <- renderEcharts4r({
       min_y <- min(df_list$agg_sa4_df %>% filter(sa4_name == clicked_sa4_df()) %>% pull(original)) *0.75
       
       df_list$agg_sa4_df %>% 
         filter(sa4_name == clicked_sa4_df()) %>% 
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
         e_y_axis(min = min_y )%>% 
         e_toolbox_feature(feature = "saveAsImage") 

       
     })
      
     # --------------------- value box
     
     chng_prc <- reactive({
       df_list$agg_sa4_df %>% 
         filter(extraction_date ==  max(extraction_date), sa4_name == clicked_sa4_df()) %>% 
         mutate(trd_chg_mth = trd_chg_mth*100,
                trd_chg_qtr = trd_chg_qtr *100,
                trd_chg_year = trd_chg_year *100,
                trd_chg_5year = trd_chg_5year *100)
     })
     
     
    
     
     output$sa4_chng_qtr <- renderbs4ValueBox({
       bs4ValueBox(
         value = if(chng_prc()$trd_chg_qtr > 0)
           tags$h1(HTML("&uarr;"), paste0(chng_prc()$trd_chg_qtr,"%"))
                else if(chng_prc()$trd_chg_qtr < 0) tags$h1(HTML("&darr;"), paste0(chng_prc()$trd_chg_qtr, "%"))
                else tags$h1(paste0(chng_prc()$trd_chg_qtr, "%")),
         subtitle = "Quarterly change",
         color = if(chng_prc()$trd_chg_qtr > 0) "primary"
                  else if(chng_prc()$trd_chg_qtr < 0) "danger"
                  else  "gray-dark" ,
         icon = icon("chart-bar"),
         #href = "#"
       )
     })
     
     output$sa4_chng_year <- renderbs4ValueBox({
       bs4ValueBox(
         value = if(chng_prc()$trd_chg_year > 0)
           tags$h1(HTML("&uarr;"), paste0(chng_prc()$trd_chg_year,"%"))
                else if(chng_prc()$trd_chg_year < 0) tags$h1(HTML("&darr;"), paste0(chng_prc()$trd_chg_year, "%"))
                else tags$h1(paste0(chng_prc()$trd_chg_year, "%")),
         subtitle = "Yearly change",
         color = if(chng_prc()$trd_chg_year > 0) "primary"
                  else if(chng_prc()$trd_chg_year < 0) "danger"
                  else  "gray-dark" ,
         icon = icon("chart-bar"),
         #href = "#"
       )
     })

     output$sa4_chng_5year <- renderbs4ValueBox({
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

     
     
     ## tbl_top_chng
     sa4_chng_tbl <- reactive({ 
       df_list$sa4_anzsco4_df %>% 
         select(sa4_name, anzsco4_name, original, trend, trd_chg_qtr, trd_chg_year,trd_chg_5year, extraction_date) %>%
         filter(sa4_name == clicked_sa4_df() & extraction_date == max(extraction_date)) %>%
         select(-extraction_date, -sa4_name) %>%
         arrange(desc(trd_chg_qtr)) %>%
         column_to_rownames(var = "anzsco4_name")
     }) 
     
     output$tbl_top_chng = renderDataTable({
       datatable( sa4_chng_tbl(),
                  colnames = c( 'Occupation','Original','Trend' ,'Quarterly change', 'Yearly change', '5-year change'),
                  selection = 'single',
                  extensions = 'Buttons',
                  options = list(scrollX = TRUE,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv')
                                 )) %>%
         formatPercentage(c("trd_chg_qtr", "trd_chg_year", "trd_chg_5year"), 0)
     })
     
     ### ---------------------------  plot table tbl_top_chng
     
     clicked_sa4_chng_tbl <- reactive({
       if(length(input$tbl_top_chng_rows_selected) > 0) {
         tmp = rownames(sa4_chng_tbl())[input$tbl_top_chng_rows_selected]
       } else {
         tmp = rownames(sa4_chng_tbl())[1]
       }
       return(tmp)
     })

     output$sa4_chng_plot <- renderEcharts4r({

       df_list$sa4_anzsco4_df  %>%
         filter(sa4_name == clicked_sa4_df() & anzsco4_name == clicked_sa4_chng_tbl()) %>%
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
     # 
      output$selected_anzsco4 <- renderText({ clicked_sa4_chng_tbl()})
     # 
     # Box header - Aggregated time series
     output$title_agg_sa4 <- renderText({
       paste0("Employment in ", clicked_sa4_df())
     })
     # 
     # Box header - Table
     output$title_table <- renderText({
       paste0("Occupations in ", clicked_sa4_df())
     })
     # 
     # Box header - Time series
     output$title_sa4_anzsco4 <- renderText({
       paste0("Employment of ", clicked_sa4_chng_tbl(), " in ", clicked_sa4_df())
     })
      
    }
  )
  
  
} 
