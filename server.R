
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

server <- function(input, output, session) {
  source("MarginalDensity_Scatterplot_mod.R", local = TRUE)
  
  # Read me file
  output$pdfview <- renderUI({
    tags$iframe(style="height:700px; width:100%", src="ShinyFlow_Instructions.pdf")
  })

  # Boh
  rv <- reactiveValues(dataset=data.frame("startingvalues"))

  readFcsData <- function() {
    req(input$file)  # Ensure file is uploaded
    # Read the Excel file and extract column names
    file <- input$file$datapath
    df <- read.FCS(file)
    df <- as.data.frame(df@exprs)
    rv$dataset <- df
    return(df)
  }

  observeEvent(input$loadData, {
    readFcsData()
  })

  reactiveData<-reactiveValues()
  
  
  
  
  
  
  ## PART 1 -- Table ## ----------------------------------------------------------------------------------------------------------------------------------

  # Render table
  observe({
    req(input$loadData)
    output$PlotDataTable <- renderDT({
      # Read the Excel file and extract the selected column
      file <- rv$dataset
      file},
    selection = 'none', filter = 'none',
    extensions = "FixedColumns",
    options = list(
      autoWidth = TRUE, pageLength = 15, paging = TRUE, searching = TRUE, info = FALSE,
      sort = TRUE, scrollX = TRUE, fixedColumns = list(leftColumns = 1),
      columnDefs = list(list(targets = 0, width = '100px'))
    ))
  })
  
  
  
  
  
  
  ## PART 2 -- Percentage Plot ## ----------------------------------------------------------------------------------------------------------------------------------
  
  # column options for percentage plot
  output$x_var_perc <- renderUI({
    req(input$loadData)
    file <- as.data.frame(rv$dataset)
    selectInput(inputId = "x_var_perc", label="Select x axis", choices = colnames(file))
  })
  output$y_var_perc <- renderUI({
    req(input$loadData)
    file <- as.data.frame(rv$dataset)
    selectInput(inputId = "y_var_perc", label="Select y axis", choices = colnames(file))
  })
  
  # Reactive percentage plot
  PercentagePlot_reactive <- reactive({
    req(input$loadData)
    file <- as.data.frame(rv$dataset)
    cut_x=input$CutoffForPositivity_x
    cut_y=input$CutoffForPositivity_y
    cols <- RColorBrewer::brewer.pal(name = "Set3", n=10)[3:7]
    col_list <- rep(NA, nrow(file))
    col_list[ (log10(file[, input$x_var_perc]+1) < cut_x) & (log10(file[, input$y_var_perc]+1) >= cut_y)] <- cols[1]
    col_list[ (log10(file[, input$x_var_perc]+1) >= cut_x) & (log10(file[, input$y_var_perc]+1) >= cut_y)] <- cols[2]
    col_list[ (log10(file[, input$x_var_perc]+1) >= cut_x) & (log10(file[, input$y_var_perc]+1) < cut_y)] <- cols[3]
    col_list[ (log10(file[, input$x_var_perc]+1) < cut_x) & (log10(file[, input$y_var_perc]+1) < cut_y)] <- cols[4]
    percs <- c(sum(col_list==cols[1], na.rm=T), sum(col_list==cols[2], na.rm=T), sum(col_list==cols[3], na.rm=T), sum(col_list==cols[4], na.rm=T))
    percs <- round(percs/sum(percs)*100, 2)
    plot(log10(file[, input$x_var_perc]+1), log10(file[, input$y_var_perc]+1), col=adjustcolor(col_list, alpha.f = 0.6), xlab=input$x_var_perc, ylab=input$y_var_perc, pch=16, las=1)
    legend("topleft", legend = percs[1], xjust = 0.5, yjust = 0.5, pch=16, col=adjustcolor(cols[1], alpha.f = 1))
    legend("topright", legend = percs[2], xjust = 0.5, yjust = 0.5, pch=16, col=adjustcolor(cols[2], alpha.f = 1))
    legend("bottomright", legend = percs[3], xjust = 0.5, yjust = 0.5, pch=16, col=adjustcolor(cols[3], alpha.f = 1))
    legend("bottomleft", legend = percs[4], xjust = 0.5, yjust = 0.5, pch=16, col=adjustcolor(cols[4], alpha.f = 1))
    abline(v=cut_x, col="grey35", lty=2)
    abline(h=cut_y, col="grey35", lty=2)
    #IMPORTANT - I had issues with downloading the file, so the code is repeated in the download section - change both if needed!!
  })
  
  output$PercentagePlot <- renderPlot({
    PercentagePlot_reactive()
  })
  
  # VERY important inconsistency between log10 and log10+1 in plots
  
  
  
  
  
  ## PART 3 -- Marginal Density Plot ## ----------------------------------------------------------------------------------------------------------------------------------
  
  # column options for density plot
  output$x_var <- renderUI({
    req(input$loadData)
    file <- as.data.frame(rv$dataset)
    selectInput(inputId = "x_var", label="Select x axis", choices = colnames(file))
  })
  output$y_var <- renderUI({
    req(input$loadData)
    file <- as.data.frame(rv$dataset)
    selectInput(inputId = "y_var", label="Select y axis", choices = colnames(file))
  })

  # Reactive marginal plot
  MarginalPlot_reactive <- reactive({
    req(input$loadData)
    file <- as.data.frame(rv$dataset)
    cut=input$CutoffForPositivity
    marginal_plot(log10(file[, input$x_var]+1), log10(file[, input$y_var]+1), group = (log10(file[,input$x_var]) > cut), plot_legend = F, alpha=0.3, xlab=input$x_var, ylab=input$y_var)
    #IMPORTANT - I had issues with downloading the file, so the code is repeated in the download section - change both if needed!!
  })

  # Plot marginal plot
  output$MarginalPlot <- renderPlot({
    MarginalPlot_reactive()
  })
  
  
  # Print test p-value
  output$TestOutput <- renderText({
    req(input$loadData)
    file <- as.data.frame(rv$dataset)
    cut=input$CutoffForPositivity
    
    validate(need(length(log10(file[(log10(file[,input$x_var]) > cut) == T , input$y_var])) >1, message = "Please select appropriate cutoff and/or channel"))
    validate(need(length(log10(file[(log10(file[,input$x_var]) > cut) == F , input$y_var])) >1, message = "Please select appropriate cutoff and/or channel"))
    
    w.test <- wilcox.test(log10(file[, input$y_var]) ~ (log10(file[,input$x_var]) > cut))
    print(paste0("p-value for difference in density distribution for '", input$y_var, "' (Mann-Whitney) = ", round(w.test$p.value, 4)))
  })
  
  
  
  ## PART 4 -- Download everything ## ----------------------------------------------------------------------------------------------------------------------------------
  
  
  # Download table
  observe({
    input$FilenameDownload
    req(input$loadData)
    output$downloadTable <- downloadHandler(
      filename = paste0(input$FilenameDownload, "_Table.txt"),
      content = function(filenameout) {
        file <- as.data.frame(rv$dataset)
        write.table(file, filenameout, quote=F, row.names=F, sep="\t")
      }
    )
  })
  
  
  # Download percentage plot
  observe({
    input$FilenameDownload
    req(input$loadData)
    output$downloadPlotPerc <- downloadHandler(
      filename = paste0(input$FilenameDownload, "_PercentagePlot.svg"),
      content = function(filenameout2) {
        svg(filenameout2, width=7, height=6)
        file <- as.data.frame(rv$dataset)
        cut_x=input$CutoffForPositivity_x
        cut_y=input$CutoffForPositivity_y
        cols <- RColorBrewer::brewer.pal(name = "Set3", n=10)[3:7]
        col_list <- rep(NA, nrow(file))
        col_list[ (log10(file[, input$x_var_perc]+1) < cut_x) & (log10(file[, input$y_var_perc]+1) >= cut_y)] <- cols[1]
        col_list[ (log10(file[, input$x_var_perc]+1) >= cut_x) & (log10(file[, input$y_var_perc]+1) >= cut_y)] <- cols[2]
        col_list[ (log10(file[, input$x_var_perc]+1) >= cut_x) & (log10(file[, input$y_var_perc]+1) < cut_y)] <- cols[3]
        col_list[ (log10(file[, input$x_var_perc]+1) < cut_x) & (log10(file[, input$y_var_perc]+1) < cut_y)] <- cols[4]
        percs <- c(sum(col_list==cols[1], na.rm=T), sum(col_list==cols[2], na.rm=T), sum(col_list==cols[3], na.rm=T), sum(col_list==cols[4], na.rm=T))
        percs <- round(percs/sum(percs)*100, 2)
        plot(log10(file[, input$x_var_perc]+1), log10(file[, input$y_var_perc]+1), col=adjustcolor(col_list, alpha.f = 0.6), xlab=input$x_var_perc, ylab=input$y_var_perc, pch=16, las=1)
        legend("topleft", legend = percs[1], xjust = 0.5, yjust = 0.5, pch=16, col=adjustcolor(cols[1], alpha.f = 1))
        legend("topright", legend = percs[2], xjust = 0.5, yjust = 0.5, pch=16, col=adjustcolor(cols[2], alpha.f = 1))
        legend("bottomright", legend = percs[3], xjust = 0.5, yjust = 0.5, pch=16, col=adjustcolor(cols[3], alpha.f = 1))
        legend("bottomleft", legend = percs[4], xjust = 0.5, yjust = 0.5, pch=16, col=adjustcolor(cols[4], alpha.f = 1))
        abline(v=cut_x, col="grey35", lty=2)
        abline(h=cut_y, col="grey35", lty=2)
        dev.off()
        #IMPORTANT - I had issues with downloading the file, so the code is repeated in the plot and download section - change both if needed!!
      }
    )
  })
  
  
  # Download marginal density plot
  observe({
    input$FilenameDownload
    req(input$loadData)
    output$downloadPlot <- downloadHandler(
      filename = paste0(input$FilenameDownload, "_MarginalDensityPlot.svg"),
      content = function(filenameout2) {
        svg(filenameout2, width=6, height=6)
        file <- as.data.frame(rv$dataset)
        cut=input$CutoffForPositivity
        marginal_plot(log10(file[, input$x_var]+1), log10(file[, input$y_var]+1), group = (log10(file[,7]) > cut), plot_legend = F, alpha=0.3, xlab=input$x_var, ylab=input$y_var)
        dev.off()
        #IMPORTANT - I had issues with downloading the file, so the code is repeated in the plot and download section - change both if needed!!
      }
    )
  })
  
 

}