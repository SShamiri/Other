nat_view_server = function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      

      output$wkl_indx_plot <- renderEcharts4r({
        min_y <- min(df_list$wkl_indx %>% pull(stacked_smoothed)) *0.85
        
        df_list$wkl_indx  %>%
          filter(extraction_date > '2020-01-01') %>% 
          e_charts(extraction_date) %>%
          e_line(stacked_smoothed, name = "weekly index", legend = FALSE, color = "#1a3edd") %>%
          e_mark_area(
            itemStyle = list(normal = list(color = "#13ceb2", opacity = 0.3)),
            label = list(normal = list( color = "#000000")),
            data = list(list(xAxis = '2021-04-01'), list(xAxis = '2021-04-29')
            )) %>%
          e_datazoom() %>%
          e_tooltip(trigger = "axis") %>%
          e_x_axis(extraction_date, axisPointer = list(show = TRUE)) %>%
          e_y_axis(min = min_y ) %>% 
          e_toolbox_feature(feature = "saveAsImage")
        
      })
      
      indx_chng_prc <- reactive({
        df_list$wkl_indx %>%
          filter(extraction_date ==  max(extraction_date)) %>%
          mutate(chg_wkl = round(chg_wkl *100,2),
                 chg_mth = round(chg_mth *100,2),
                 chg_qtr = round(chg_qtr *100,2)
                 )
      })
      
      output$indx_chg_wkl <- renderbs4ValueBox({
        bs4ValueBox(
          value = if(indx_chng_prc()$chg_wkl > 0)
            tags$h1(HTML("&uarr;"), paste0(indx_chng_prc()$chg_wkl,"%"))
          else if(indx_chng_prc()$chg_wkl < 0) tags$h1(HTML("&darr;"), paste0(indx_chng_prc()$chg_wkl, "%"))
          else tags$h1(paste0(indx_chng_prc()$chg_wkl, "%")),
          subtitle = "Weekly change",
          color = if(indx_chng_prc()$chg_wkl > 0) "primary"
          else if(indx_chng_prc()$chg_wkl < 0) "danger"
          else  "gray-dark" ,
          icon = icon("chart-bar"),
          #href = "#"
        )
      })
      
      output$indx_chg_mth <- renderbs4ValueBox({
        bs4ValueBox(
          value = if(indx_chng_prc()$chg_mth > 0)
            tags$h1(HTML("&uarr;"), paste0(indx_chng_prc()$chg_mth,"%"))
          else if(indx_chng_prc()$chg_mth < 0) tags$h1(HTML("&darr;"), paste0(indx_chng_prc()$chg_mth, "%"))
          else tags$h1(paste0(indx_chng_prc()$chg_mth, "%")),
          subtitle = "Monthly change",
          color = if(indx_chng_prc()$chg_mth > 0) "primary"
          else if(indx_chng_prc()$chg_mth < 0) "danger"
          else  "gray-dark" ,
          icon = icon("chart-bar"),
          #href = "#"
        )
      })
      
      output$indx_chg_qtr <- renderbs4ValueBox({
        bs4ValueBox(
          value = if(indx_chng_prc()$chg_qtr > 0)
            tags$h1(HTML("&uarr;"), paste0(indx_chng_prc()$chg_qtr,"%"))
          else if(indx_chng_prc()$chg_qtr < 0) tags$h1(HTML("&darr;"), paste0(indx_chng_prc()$chg_qtr, "%"))
          else tags$h1(paste0(indx_chng_prc()$chg_qtr, "%")),
          subtitle = "Quaterly change",
          color = if(indx_chng_prc()$chg_qtr > 0) "primary"
          else if(indx_chng_prc()$chg_qtr < 0) "danger"
          else  "gray-dark" ,
          icon = icon("chart-bar"),
          #href = "#"
        )
      })
 
 
      
    }
  )
  
  
} 
