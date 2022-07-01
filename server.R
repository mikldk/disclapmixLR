#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(here)
library(readxl)
library(readODS)
library(ggplot2)
library(dplyr)
library(tidyr)

theme_set(theme_bw(base_size = 20))

source("utils.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    rv <- reactiveValues(fits = NULL, 
                         profiles = NULL, 
                         profile_names = NULL) 
    
    observeEvent(input$button_run_userfiles, {
      input$fits_file
      input$profiles_file
      
      stopifnot(file.exists(input$fits_file$datapath))
      stopifnot(file.exists(input$profiles_file$datapath))
      
      new_fits <- readRDS(input$fits_file$datapath)
      verify_fits_file(new_fits)
      
      ###
      
      ext <- tools::file_ext(input$profiles_file$datapath)
      
      new_profiles <- if (ext == "ods") {
        readODS::read_ods(input$profiles_file$datapath)
      } else if (ext == "xlsx") {
        readxl::read_xlsx(input$profiles_file$datapath)
      } else {
        NULL
      }
      
      # Reorder loci etc.
      new_profiles_res <- verify_process_profiles_file(new_fits, new_profiles)
      
      rv$fits <- new_fits
      rv$profiles <- new_profiles$profiles
      rv$profile_names <- new_profiles$profile_names
    })
    
    observeEvent(input$button_run_example, {
      new_fits <- readRDS(here("fits.Rdata"))
      
      new_profiles <- readODS::read_ods(here("profiles.ods"))
      new_profiles_res <- verify_process_profiles_file(new_fits, new_profiles)
      
      rv$fits <- new_fits
      rv$profiles <- new_profiles$profiles
      rv$profile_names <- new_profiles$profile_names
      
    })

    output$plot_BICs <- renderPlot({
      fits <- rv$fits
      
      if (is.null(fits)) {
        return(NULL)
      }
      
      BICs <- sapply(fits, function(x) x$BIC_marginal)
      bestfit <- fits[[which.min(BICs)]]
      
      clusters <- sapply(fits, function(x) nrow(x$y))
      
      d_tmp <- tibble(Clusters = clusters, BIC = BICs)
      d_tmp_best <- tibble(Clusters = nrow(bestfit$y), 
                           BIC = bestfit$BIC_marginal, 
                           label = "Best model (lowest BIC)")
      
      ggplot(d_tmp, aes(Clusters, BIC)) + 
        geom_line() + 
        geom_point()  +
        geom_point(data = d_tmp_best, aes(color = label),
                   pch = 4, 
                   size = 8)  +
        geom_hline(data = d_tmp_best, aes(yintercept = BIC), color = "red", linetype = "dashed") + 
        scale_x_continuous(breaks = clusters) + 
        scale_color_manual(values = c("Best model (lowest BIC)" = "red")) +
        labs(y = "BIC", color = NULL) + 
        theme(legend.position = "bottom")
    })
    
    output$download_fits_example <- downloadHandler(
      filename = "fits.Rdata",
      content = function(file) {
        file.copy(here("fits.Rdata"), file)
      }
    )
    
    output$download_profiles_example_ods <- downloadHandler(
      filename = "profiles.ods",
      content = function(file) {
        file.copy(here("profiles.ods"), file)
      }
    )
    
    output$download_profiles_example_xlsx <- downloadHandler(
      filename = "profiles.xlsx",
      content = function(file) {
        file.copy(here("profiles.xlsx"), file)
      }
    )
    

})
