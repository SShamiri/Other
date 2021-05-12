
source('global.R')
# 
source('ui_elements/nat_view_ui.R')
source('ui_elements/reg_view_ui.R')
source('ui_elements/occ_view_ui.R')
source('ui_elements/detail_view_ui.R')
source('ui_elements/method_view_ui.R')

library(shiny)
library(bs4Dash)
library(fresh)

# header
HEADER <- bs4DashNavbar(title = dashboardBrand(
  title = tags$div(img(src="National Skills Commission logo_White.png", width = "180px", height = "80px")),
  color = "primary",
  #href = "",
  #image = "National Skills Commission logo_White.png",
  opacity = 0.8),
  tags$strong("NSC Employment Estimates")
  )

## sidebar
SIDEBAR <- bs4DashSidebar(
  fixed = TRUE,
  skin = "light",
  status = "primary",
  id = "sidebar",
  #--------------------------------------- menues
  bs4SidebarMenu(
    id = "tabname",
    bs4SidebarMenuItem(
      text = "National view",
      icon = icon("bars"),
      tabName = "national_tab",
      badgeLabel = "Soon",
      badgeColor = "success"
    ),
    bs4SidebarMenuItem(
      text = "Regional view",
      icon = icon("bars"),
      startExpanded = TRUE,
      bs4SidebarMenuSubItem(
        text = "State",
        tabName = "state_tab",
        icon = icon("circle-thin")
      ),
      bs4SidebarMenuSubItem(
        text = "SA4 level",
        tabName = "sa4_tab",
        icon = icon("circle-thin")
      )
    ),
    bs4SidebarMenuItem(
      text = "Occupation view",
      icon = icon("bars"),
      startExpanded = FALSE,
      #active = FALSE,
      bs4SidebarMenuSubItem(
        text = "ANZSCO 4-digit",
        tabName = "occ4_tab",
        icon = icon("circle-thin")
      ),
      bs4SidebarMenuSubItem(
        text = "ANZSCO 6-digit",
        tabName = "occ6_tab",
        icon = icon("circle-thin")
      )
    ),
    bs4SidebarMenuItem(
      text = "Detail view",
      icon = icon("bars"),
      tabName = "detial_tab"
    ),
    bs4SidebarMenuItem(
      text = "Methodology",
      icon = icon("bars"),
      tabName = "method_tab"
    )
  )
)

# controlbar
CONTROLBAR = bs4DashControlbar(id = "controlbar", 
                               skin = "light", 
                               pinned = FALSE, 
                               overlay = FALSE,
                               div(img(src="National Skills Commission logo_RGB.jpg", width = "180px", height = "80px")),
                               br(),
                               tags$div('Nowcasting & Economic Modelling', 
                                        br(),
                                        ' National Skills Commission' , style = "font-size:15px;padding:5px;"),
                               
                               tags$a(href = "www.nationalskillscommission.gov.au", " www.nationalskillscommission.gov.au", style = "font-size:10px;padding:5px;"),
                               br(), br(),
                               p(strong("Latest release: "), max(df_list$recent_dates), style = "padding:5px;"),
                               p(strong("Contact us on: "), 'blablah@skillscommission.gov.au', style = "padding:5px;"),
                               br(), br(),br(), br(),
                               p(strong("Disclaimer: "), 'These series remain experimental and are subject to revisions and errors. Caution should be exercised when utilising these employment estimates.', style = "padding:5px;"),
                               p(strong("Note: "), "A minimum value of 10 has been applied to all estimates.", style = "padding:5px;")
                               )

# footer
FOOTER = bs4DashFooter(left = 'Copyright © 2021 National Skills Commission All rights reserved.',
                       right = 'Version 0.1 Beta')


## title
TITLE = "National Skills Commission"


## body
BODY =  bs4DashBody(
  
  use_theme(create_theme(
    bs4dash_status(
      primary = "#2E3CB6",
      secondary = "#2E3CB6"
    )
  )),
  #tags$div(img(src="analytics_32px.png", style="float:left;" ),h1("NSC Employment Estimates")),
  bs4TabItems(
    national_view_ui("nat_view"),
    state_view_ui("state_view"),
    sa4_view_ui("sa4_view"),
    occ4_view_ui("occ4_view"),
    occ6_view_ui("occ6_view"),
    detail_view_ui("detail_view"),
    method_view_ui("method_view")
    
  )
)

# ##
ui =  bs4DashPage(
  dark = FALSE,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  header = HEADER,
  sidebar = SIDEBAR,
  body = BODY,
  controlbar = CONTROLBAR,
  footer = FOOTER,
  title = TITLE)

