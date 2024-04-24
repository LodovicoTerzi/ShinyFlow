# create User Interface -------------------------------------------------

library(BiocManager)
options(repos = BiocManager::repositories())
options(repos = getOption("repos"))
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(shiny)
library(readxl)
library(flowCore)
library(dplyr)

## Dashboard on top -------------------------------------------------
dashboard <- dashboardHeader(title = strong("ShinyFlow"),
                             tags$li(a(img(src = 'logoIOR.svg',height = "50px"), style = "padding-top:00px; padding-bottom:00px; background: #F5F5F5; width: 100px; border-right:0px;"), class = "dropdown"),
                             tags$li(a(img(src = 'logoUSI.svg',height = "50px"), style = "padding-top:00px; padding-bottom:00px; background: #F5F5F5; width: 100px"), class = "dropdown"),
                             tags$li(a(img(src = 'logoETH.svg',height = "50px"), style = "padding-top:00px; padding-bottom:00px; background: #F5F5F5; width: 200px"), class = "dropdown"))


## Sidebar left side -------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("ShinyFlow", icon = icon("calculator", class = "duotone"), tabName = "ShinyFlow"),
    menuItem("Read Me", icon = icon("info"), tabName = "readme"),
    h6("Lodovico Terzi di Bergamo", style="padding-top: 10px; padding-left: 10px; font-size: 15px"),
    h6("lodovico.terzi@ior.usi.ch", style="padding-bottom: 30px; padding-left: 10px")),
  tags$li(a(onclick = "onclick =window.open('https://github.com/LodovicoTerzi')", href = NULL, icon("github"), title = "GitHub", style = "cursor: pointer; padding-left: 40px; font-size: 36px; list-style-type: none;")),
  collapsed = TRUE)



## Body of the application -------------------------------------------------
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "ShinyFlow",
            useShinyjs(),
            
            fluidRow(
              
              column(2,
                     div(style="width: 100%; margin-bottom: 0px", fileInput("file", label = "Please select input file", accept = c(".fcs"))),
                     div(style="display: inline-block; vertical-align:top; width: 100%; margin-top: -20px", actionButton("loadData", "Load Data")),
                     div(style="display:inline-block;display:center-align; margin-top: 50px; width: 100%;", numericInput(inputId = "CutoffForPositivity", label="Cutoff X-axis Marginal Plot", value=2.5)),
                     div(style="display:inline-block;display:center-align; margin-top: 30px; width: 100%;", numericInput(inputId = "CutoffForPositivity_x", label="Cutoff X-axis Percentage Plot", value=2.5)),
                     numericInput(inputId = "CutoffForPositivity_y", label="Cutoff Y-axis Percentage Plot", value=2.5),
                     div(style="display:inline-block;display:center-align; margin-top: 50px; width: 100%;", textInput("FilenameDownload", width='100%', label="Filename")),
                     div(style="display:inline-block;display:center-align; margin-top: 3px; width: 100%; display: flex;justify-content: center;align-items: center;", downloadButton("downloadTable", label = "Download table")),
                     div(style="display:inline-block;display:center-align; margin-top: 3px; width: 100%; display: flex;justify-content: center;align-items: center;", downloadButton("downloadPlotPerc", label = "Download Percentage plot")),
                     div(style="display:inline-block;display:center-align; margin-top: 3px; width: 100%; display: flex;justify-content: center;align-items: center;", downloadButton("downloadPlot", label = "Download Density plot")),
              ),
              
              
              column(10,
                     tabsetPanel(
                       tabPanel("Table", DTOutput('PlotDataTable')),
                       tabPanel("PercentagePlot", h1(), 
                                div(style="display:inline-block;", uiOutput(outputId = "x_var_perc")), 
                                div(style="display:inline-block;", uiOutput(outputId = "y_var_perc")), 
                                h1(), plotOutput('PercentagePlot', width = 600, height = 600)),
                       tabPanel("MarginalPlot", h1(), 
                                div(style="display:inline-block;", uiOutput(outputId = "x_var")), 
                                div(style="display:inline-block;", uiOutput(outputId = "y_var")), 
                                h1(), plotOutput('MarginalPlot', width = 600, height = 500), h1(), verbatimTextOutput('TestOutput')),
                       div(style="display:inline-block;display:center-align; margin-top: 3px; width: 100%; display: flex;justify-content: center;align-items: center;", shinyjs::hidden(downloadButton("downloadAll", 'Download tables'))),),
                     )),
            
            #color of switch input
            tags$head(tags$style(HTML('
                                      /* switch input */
                                       .bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-Yes,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-Yes {background: #3CB371; color: white;}
                                       .bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-No,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-No {background: #191970; color: white;}
                                      /* logo */
                                       .skin-blue .main-header .logo {background-color: #00688B; color: #FF4500}
                                      /* logo when hovered */
                                       .skin-blue .main-header .logo:hover {background-color: #FF4500; color: #00688B;}
                                      /* header bar */
                                       .skin-blue .main-header .navbar {background-color: #191970;}
                                      /* active selected tab in the sidebarmenu */
                                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #F5F5F5; color: #FF4500;}
                                      /* background */ 
                                       .content-wrapper, .right-side {background-color: #F5F5F5;}
                                      /* panels color */ 
                                       .tabbable > .nav > li > a {color: black;}
                                      /* remove bullet point from github */ 
                                       li {list-style-type: none;}
                                      /* toggle button when hovered  */
                                       .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: #191970; color: #F5F5F5;}
                                      /* active tab  */
                                      .tabbable > .nav > li[class=active]    > a {background-color: #3CB371; color:#F5F5F5}
                                      ')))),
    tabItem(tabName = "readme",
            uiOutput("pdfview"))
  ))




ui <- shinyUI(dashboardPage(dashboard, sidebar, body))
