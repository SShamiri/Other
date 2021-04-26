library(shiny)
library(bs4Dash)
library(echarts4r)
library(thematic)
library(waiter)

thematic_shiny()
thematic_shiny()

# toast options
toastOpts <- list(
  autohide = TRUE,
  icon = "fas fa-home",
  close = FALSE,
  position = "bottomRight"
)

# echarts4r theme #3d444c
echarts_dark_theme <- list(
  options = '{
    "color":["#6610f2", "#ffc107", "#e83e8c", "#ff851b", "#17a2b8", "#3d9970"], 
    "backgroundColor": "#343a40", 
    "textStyle": {
        color: "#fff"
    }
  }',
  name = "dark_theme"
)

# header
header <- dashboardHeader(
  # title = bs4DashBrand(
  #     title = "bs4Dash",
  #     color = "primary",
  #     href = "https://divadnojnarg.github.io/outstanding-shiny-ui/",
  #     image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
  #     opacity = 0.8
  #   ),
  fixed = TRUE
)



# navbar
#navbar = bs4DashNavbar(h2('Nowcasting'))

# sidebar
sidebar = dashboardSidebar(
  fixed = TRUE, skin = "light", status = "primary", id = "sidebar",
  #sidebarUserPanel(name="logo",subtitle = "Test",image = "www/SamuelShamiri.jpg"),
  sidebarMenu(
    id = "current_tab", flat = FALSE, compact = FALSE, childIndent = TRUE,
    menuItem("National view", tabName = "national", icon = icon("laptop-code")),
    menuItem("Regional view", icon = icon("bar-chart-o"), startExpanded = TRUE,
             menuSubItem("State", tabName = "state"),
             menuSubItem("SA4 level", tabName = "sa4")
    ),
    menuItem("Occupation view", icon = icon("bar-chart-o"), startExpanded = TRUE,
             menuSubItem("ANZSCO4", tabName = "anzsco4"),
             menuSubItem("ANZSCO6", tabName = "anzsco6")
    ),
    menuItem("Detail view", icon = icon("th"), tabName = "detial"),
    menuItem("Methodology", icon = icon("th"), tabName = "method")
  )
)



# controlbar
controlbar = dashboardControlbar(id = "controlbar", skin = "light", pinned = TRUE, overlay = FALSE)

# footer
footer = bs4DashFooter(p('footer'))

## tabs
region_tab <- tabItem(
  tabName = "sa4_plot",
  fluidRow(
    selectInput(
      inputId = "state_filter", 
      label = "Select state", 
      choices = unique(df_filter$state_name)
    ),
    selectInput(
      inputId = "sa4_filter", 
      label = "Select SA4 Region", 
      choices = NULL
    ),
    selectInput(
      inputId = "anzsco4_filter", 
      label = "Select ANZSCO4", 
      choices = NULL
    )
  ),
  fluidRow(
    box(
      id = "box_map",
      title = "Select SA4", 
      width = 6,
      status = "danger", 
      closable = FALSE,
      maximizable = FALSE, 
      collapsible = TRUE,
      p("sa4 map goes here")
      #plotOutput("sa4_map")
    ),
    box(
      id = "box_sa4_plot",
      title = "Selected SA4", 
      width = 6,
      status = "danger", 
      closable = FALSE,
      maximizable = TRUE, 
      collapsible = TRUE,
      #p("sa4 plot goes here")
      echarts4rOutput("sa4_plot")
    )
    
  )
)



## body
body = dashboardBody(
  e_theme_register(echarts_dark_theme$options, name = echarts_dark_theme$name),
   tabItems(
     region_tab
  #   state_tab,
  #   sa4_tab,
  #   anzsco4_tab,
  #   anzsco6_tab,
  #   detial_tab,
  #   method_tab
   )
)

## title
title = "National Skills Commission"


##
ui = dashboardPage( 
                    # preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
                    # dark = TRUE,
                    # help = TRUE,
                    # fullscreen = TRUE,
                    # scrollToTop = TRUE,
                    ####
                    header,
                    sidebar, 
                    controlbar, 
                    body, 
                    footer,
                    title)
