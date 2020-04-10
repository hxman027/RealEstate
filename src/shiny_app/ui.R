# Mallory - STAT 550 2020###
# This is the user interface version of the shiny app

library(shinythemes)
library(png)

ui <- fluidPage(theme = shinytheme("cerulean"), #maybe journal theme?

                # header
                div(id = "headerSection",
                    h2("BC Mill Rate & Assessment Value Predictions"),
                    
                    span(
                      style = "font-size: 1em",
                      # authors
                      span("Created by "),
                      a("Gian Carlo Diluvi, Vittorio Romaniello, Sophia Li & Mallory Flynn",
                        href = "https://www.stat.ubc.ca"),
                      HTML("&bull;"),
                      # date
                      span("April 2020"),
                      HTML("&bull;"),
                      # Shiny app code link
                      span("Code"),
                      a("on GitHub", href = "insert_link"),
                    )
                ),
                br(),
                br(),
                
                # all content goes here, and is hidden initially until the page fully loads
                sidebarLayout(
                  sidebarPanel(
                    # tabsetPanel(
                    #   tabPanel("User Inputs",                
                    # Only show the following for assessment predictions:
                    # Use PIC?
                    checkboxInput("picInput", "Use PIC?", value = FALSE),
                    selectInput("typeInput", "Estimate Type",
                                c("Select", "Assessment Value", "Mill Rate"),
                                selected = "Select"),
                                     
                    # If using PIC:
                    conditionalPanel("input.picInput",
                                     textInput("identInput", "PIC:", placeholder = NULL)),
                                     
                    # If PIC is not available:
                    conditionalPanel("!input.picInput",
                                     # selectInput("typeInput", "Estimate Type",
                                     #             c("Select", "Assessment Value", "Mill Rate"),
                                     #             selected = "Select"),
                                     
                                     # for municipality
                                     selectInput("municipalityInput", "Municipality:", 
                                                 c("-",sort(unique(dat$municipality))),
                                                 selected = "-"),
                                                     
                                     # for Tax Class code
                                     selectInput("taxclassInput", "Tax Class Code:",
                                                 c("-",sort(unique(dat$tax.class))),
                                                 selected = "-"),
                                     
                                     #conditional input for estimate type
                                     conditionalPanel("input.typeInput == 'Assessment Value'",
                                                      numericInput("assessmentInput",
                                                                   "Current Assessment Value:",
                                                                   value = 500000, min = 0, 
                                                                   max = 6000000000))
                                     ),
                                    
                    
                    
                    # button to update the data
                    shiny::hr(),
                    actionButton("updateButton", "Update"),
                    # ),
                    # 
                    # tabPanel("Upload data",
                    #          br(),
                    #          # Input: Select a file ----
                    #          fileInput("csvfile", "Upload Data from .csv file",
                    #                    multiple = TRUE,
                    #                    accept = c("text/csv",
                    #                               "text/comma-separated-values,text/plain",
                    #                               ".csv")),
                    #          
                    #          # Horizontal line ----
                    #          tags$hr(),
                    #          
                    # )
                    # ),
                    
                    
                      
                    
                    # source of data as a footer - Altus Group image not loading
                    br(),
                    br(),
                    p("Generated using data from ",
                      a("the Altus Group Ltd.",
                        href = "https://www.altusgroup.com",
                        target = "_blank")),
                    a(img(src = "altusgroupimg.png", alt = "Altus Group",
                          height = 63, width = 150),
                      href = "https://www.altusgroup.com",
                      target = "_blank"),
                    br(),
                    br(),
                    br(),
                    br(),
                    bookmarkButton()
                    ),
                
                 
                  # main panel with Estimate tab and plot tab with mill rates
                  # or assessment values over time
                  mainPanel(h4(textOutput("resultsText")),
                            tabsetPanel(
                              tabPanel("Estimate", 
                                       br(),
                                       verbatimTextOutput("results")),
                               tabPanel("Plot", 
                                        br(),
                                        plotOutput("coolplot"))
                              )
                  )
                )
                )

                
