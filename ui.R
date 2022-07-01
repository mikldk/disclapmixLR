#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(here)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Upload fits"),

    sidebarLayout(
        sidebarPanel(
            fileInput(inputId = "fits_file", 
                      label = "File with fits", 
                      multiple = FALSE,
                      accept = c(".rda",
                                 ".Rda",
                                 ".Rdata")),
            
            fileInput(inputId = "profiles_file", 
                      label = "File with profiles",
                      multiple = FALSE,
                      accept = c(".xlsx")),
            
            actionButton("button_run_userfiles", "Run"),
            
            
            h3("Generate fits object"),
            pre(paste0(c(
              'stopifnot(packageVersion("disclapmix") >= "1.7.4")', 
              'library(disclapmix)', 
              'data(danes)', 
              'db <- as.matrix(danes[rep(seq_len(nrow(danes)), danes$n), seq_len(ncol(danes)-1)])',
              'fits <- disclapmix_adaptive(x = db)', 
              'saveRDS(fits, file = "fits.Rdata")'), 
              collapse = "\n")),
            downloadButton("download_fits_example", "Download example fits object"),
            
            h3("Profiles file"),
            p(paste0(c(
              "File (OpenOffice ods / Excel xlsx) with first row column names (marker names, ", 
              "same as used in fitting the models, not necessarily same order), and ", 
              "potentially an additional column that is assumed to be sample name ", 
              "if it exists."), 
              collapse = "\n")),
            downloadButton("download_profiles_example_ods", "Download example profiles file (OpenOffice ods)"),
            downloadButton("download_profiles_example_xlsx", "Download example profiles file (Excel xlsx)"),
            
            h3("Demo"),
            actionButton("button_run_example", "Run using example files")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot_BICs", width = "100%")
        )
    )
))
