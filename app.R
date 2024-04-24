

######################################################################################
####### Application for plotting .fcs files from flow-cytometry analysis   ###########
#######              Coded by: Lodovico Terzi di Bergamo                   ###########
#######                    DR Lab - 03.04.2023                             ###########
######################################################################################

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
library(periscope)

# Run the application 
shiny::runApp()



