# Load required packages -------------------------------
require(shiny)
require(shinythemes)
require(DT)
require(bslib)
require(shinyWidgets)
require(plotly)
library(renv)
library(scales)
library(tidyverse)
library(hms)
library(lubridate)
library(magrittr)
library(vroom)
library(tidyselect)
library(janitor)
library(readxl)
library(tidytable)
library(data.table)
library(here)
library(stringdist)

# Extra packages not needed
# library(ggtext)
# library(extrafont)
# library(naniar)
# library(ggplot2)
# library(zoo)
# require(thematic)

#Package Coverage Test
    # library(rstudioapi)
    # library(NCmisc)
    # list.functions.in.file(rstudioapi::getSourceEditorContext()$path, alphabetic = TRUE)


#RENV-------------------------------
# renv::init()
# renv::status()
# renv::snapshot()

# Load Helper files -------------------------------
source("figure_helper.R")
source("table_helper.R")
source("data_processor.R")
#source("weather_processor.R")

i_am("app.R")

#FONTS
# font_import()
# loadfonts(device = "win")

#Load data and split out cancelled dates
ctta_results <- read_xlsx(here("Data/Master Results", "tt_results_master.xlsx")) %>% clean_names() %>% remove_empty(which = c("rows", "cols"))
ctta_roster <- read_csv(here("Data/Master Results", "ctta_roster.csv"))

tt_results <- process_raw_tt_data(results = ctta_results , roster = ctta_roster )

tt_cancelled_dates <- tt_results %>% filter(rider_name == "Cancelled")

tt_results <- tt_results %>% filter(rider_name != "Cancelled") %>% filter(rider_name_2 != "DNF")

weather <- readr::read_csv(here("Data/Master Results","tai_tapu_weather2010-2022.csv")) %>% dplyr::mutate(date = dmy(date))

#initial selection variables for event page picker, selects the latest date
event_picker_inital_selection <- tt_results %>% 
                                  dplyr::select(date, event, season) %>% 
                                  dplyr::arrange(desc(date)) %>% 
                                  dplyr::slice_head( n = 1 ) %>% 
                                  dplyr::mutate(date_event = paste0(date," - ",event))

#thematic_shiny(font = "auto") 

#   Create user interface -------------------------------
ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "cerulean", font_scale = 1.05, `enable-rounded` = TRUE, primary = "#C71C1C"),
  inverse = F,
  id = "CTTA",
  windowTitle = "CTTA Results Explorer",
  # App Title
  tags$div(tags$img(src='uci_tt_logo.png', width = 100, height = 100, style="float:left; margin-left: 5px; margin-right: 5px; margin-top: 5px")),
  # Player Pages --------------------------------------------------------
  
  tabPanel("Event Records",
           fluidRow( 
             column(width = 2, pickerInput(
               inputId = "season",
               label = "Selected Season", 
               choices = sort(unique(tt_results$season), decreasing = T),
               selected = event_picker_inital_selection$season
             )),
             column(width = 3, pickerInput(
               inputId = "dates",
               label = "Event", 
               choices = event_picker_inital_selection$date_event,
               selected = event_picker_inital_selection$date_event
             )),
            column(width = 7,
                   wellPanel(DTOutput("weather_table" , height = 100 , width = "auto")))),    

           fluidRow(
             column(5,wellPanel( DTOutput("event_results_table", height = 850, width = "auto") )), 
             column(7,
                    wellPanel( plotOutput("event_fig_male", height = 400) ),
                    wellPanel( plotOutput("event_fig_female", height = 400) ))
                    ), 
           
          ),

#   tabPanel("Athlete Comparison",
#            fluidRow( 
#                column(8, align = "left",
#                       selectizeInput(inputId = "athlete_name", label = NULL,
#                                      choices = sort(unique(tt_results$rider_name)),
#                                      selected = sample(c("Jake Marryatt", "Gary Ferguson", "David Roche", "Michael Vink", "Sharon Prutton",
#                                                          "Mary Jones", "Reon Nolan", "Darran Humpheson"), 1),
#                                      multiple = F,
#                                      options = list(maxOptions = 1500)
# ))),
#                 column(5,wellPanel( DTOutput("event_results_table", height = 600, width = "auto") ), 
# ),
#    
#            
#            fluidRow(
#                column(5,wellPanel( DTOutput("event_results_table", height = 600, width = "auto") )), 
#                column(7,wellPanel( plotOutput("event_fig", height = 600, width = "auto") ),
# )), 
#            
# ),
  
  tabPanel("Athlete Records",
           fluidRow(column(4,
                    fluidRow(column(12,
                                    wellPanel(fluidRow(style = 'margin-top: 10px',
                                                       column(4, align = 'right', p("Select an Athlete:")),
                                                       column(8, align = "left",
                                                              selectizeInput(inputId = "athlete_name", label = NULL,
                                                                             choices = sort(unique(tt_results$rider_name)),
                                                                             selected = sample(c("Jake Marryatt", "Gary Ferguson", "David Roche", "Michael Vink", "Sharon Prutton",
                                                                                                 "Mary Jones", "Reon Nolan", "Darran Humpheson"), 1),
                                                                             multiple = F,
                                                                             options = list(maxOptions = 1500)
                                                                              ))),
                                              fluidRow(column(6, p(tags$b('Tai Tapu 16km TT Results:'), style = 'text-align:center; margin-bottom: 5px;')),
                                                       column(6, p(tags$b('All Race Results:'), style = 'text-align:center; margin-bottom: 5px;'))),
                                              fluidRow(column(6, align = 'center', htmlOutput('athlete_career_L')),
                                                       column(6, align = 'center', htmlOutput('athlete_career_R')))))),
                    fluidRow(column(12, 
                             wellPanel(DTOutput("athlete_results_table"))))),
                     column(8,
                    # Error Message Appearance
                    tags$head(tags$style(HTML(".shiny-output-error-validation"))),
                    wellPanel(fluidRow(column(12, plotlyOutput("athlete_fig", height = 800))))))),
      
tabPanel("Leaderboards",
         fluidRow(
           column(5, 
                  p(tags$b('Male Leaderboard:'), style = 'text-align:center; margin-bottom: 5px; font-size:140%;'), 
                  wellPanel(DTOutput("leaderboard_table_male" , height = 800 , width = "auto") )),
           
           # column(4,wellPanel(DTOutput("leaderboard_table_female" , height = 800 , width = "auto") )),
           column(5, 
                  p(tags$b('Female Leaderboard:'), style = 'text-align:center; margin-bottom: 5px; font-size:140%;'),
                  wellPanel(DTOutput("leaderboard_table_female" , height = 800 , width = "auto") )),
           
           column(2, fluidRow( pickerInput(
                     inputId = "leaderboard_date_filter",
                     label = "Selected Season", 
                     choices = c( "All" , sort(unique(tt_results$season), decreasing = T) ),
                     selected = "All"
                         ))  , 
                  
                  
                  fluidRow( radioButtons( inputId = "leaderboard_type" , 
                                          label = "Leaderboard Type" , 
                                          choices = c("Fastest Riders" = "riders" , "Fastest Times - Top 500" = "times"), 
                                          selected = "riders" )),
                  fluidRow( radioButtons( inputId = "bike_type" , 
                                          label = "Bike Type (in_dev_no_data)" , 
                                          choices = c("Time Trial" = "TT" , "Road" = "road"), 
                                          selected = "TT" )),
                  fluidRow( radioButtons( inputId = "age_group" , 
                                          label = "Age Group (in_dev_no_data)" , 
                                          choices = c("All" = "all" , 
                                                      "Elite-Senior" = "Elite-Senior", 
                                                      "U15" = "u15", 
                                                      "U17" = "u17" , 
                                                      "U19" = "u19", 
                                                      "U23" = "u23" , 
                                                      "Masters 1" = "Masters 1", 
                                                      "Masters 2" = "Masters 2", 
                                                      "Masters 3" = "Masters 3", 
                                                      "Masters 4" = "Masters 4", 
                                                      "Masters 5" = "Masters 5", 
                                                      "Masters 6" = "Masters 6", 
                                                      "Masters 7" = "Masters 7", 
                                                      "Masters 8" = "Masters 8",
                                                      "Masters 9" = "Masters 9",
                                                      "Masters 10" = "Masters 10"),
                                          selected = "all" ))
                  ),
           
         )),


  # About Tab ------------------------------------------------
    tabPanel("About", icon = icon("bars"),
                     fluidRow( column(12, wellPanel(includeHTML("about_ctta.html")))))
  )

# Server logic -----------------------------------

server <- function(input, output, session) {
  #bs_themer()
  
  ### Player Pages Tab -----------------------------------
  
  # Error Messagesv
  plyr_err_msg1 <- "Waiting for athlete..."
  plyr_err_msg2 <- ""
  
  # Present the Player's Tournaments Table
  athlete_trns <- reactive({ athlete_results(tt_results,
                                             athlete = input$athlete_name) })
  
  output$athlete_results_table <- renderDT({ datatable(athlete_trns(), 
                                                     options = list(info = F,
                                                                    paging = F,
                                                                    searching = F,
                                                                    stripeClasses = F, 
                                                                    lengthChange = F,
                                                                    scrollY = '460px',
                                                                    scrollCollapse = T,
                                                                    columnDefs = list(list(className = 'dt-center', 
                                                                                           targets = "_all"))),
                                                     rownames = F,
                                                     selection = 'none') })
  # Player career information
  output$athlete_career_L <- renderPrint({ 
    
    validate(need(input$athlete_name, plyr_err_msg2))
    athlete_career_str_L <- function(results) {
      
      details <- if(nrow(results) == 0) {
        p(tags$b("Races: "), br(),
          tags$b("Dist (km) "), br(),
          tags$b("Fastest Time: "), br(),
          tags$b("Best Avg Speed: "), br(),
          ) 
        }
      else if(nrow(results) >= 1) {
        p(tags$b("Races: "),         tags$em( nrow(results) ), br(),
          tags$b("Dist (km) "),      tags$em( sum(results$dist_km) ), br(),
          tags$b("Fastest Time: "),  tags$em( as_hms(min( results$time ))) , br(),   
          tags$b("Best Avg Speed: "),tags$em( max( results$speed )), br(), 
          )
        } 
      else { NA }
      
      return(details)
    }
    
    athlete_career_str_L(results = tt_results %>% filter(rider_name == input$athlete_name) %>% filter(dist_km == 16 & location == "Tai Tapu" ) )
  })
  
  output$athlete_career_R <- renderPrint({ 
    
    validate(need(input$athlete_name, plyr_err_msg2))
    athlete_career_str_R <- function(results) {
      
      details <- if(nrow(results) == 0) {
        p(tags$b("Races: "), br(),
          tags$b("Dist (km) "), br(),
          tags$b("Fastest Speed: "), br(),
          tags$b("First Race: "), br(),
          tags$b("Last Race: "), br(),
        ) 
        }
      else if(nrow(results) >= 1) {
        p(tags$b("Races: "),         tags$em( nrow(results)), br(),  
          tags$b("Dist (km) "),      tags$em( sum(results$dist_km) ), br(),  
          tags$b("First Race: "),    tags$em( min( results$date )) , br(), 
          tags$b("Last Race: "),     tags$em( max( results$date ) ), br(),
        ) 
        }  
      else { NA }
      
      return(details)
    }
    
    athlete_career_str_R(results = tt_results %>% filter(rider_name == input$athlete_name) )
    
  })
  
  # Plot the Athlete Figure
  output$athlete_fig <- renderPlotly({          
    validate(need(input$athlete_name, plyr_err_msg1))
    athlete_results_plot(tt_results, athlete = input$athlete_name) })

  # Plot the Event Figure for male
  output$event_fig_male <- renderPlot({
    event_results_plot(results = tt_results  , date = input$dates , gender_filter = "male" , dist = 16)
  }) 
  
  # Plot the Event Figure for female
  output$event_fig_female <- renderPlot({
    event_results_plot(results = tt_results  , date = input$dates , gender_filter = "female" , dist = 16 )
  }) 

  # Plot the Event Figure for 8km
  output$event_fig_8km <- renderPlot({
    event_results_plot(results = tt_results  , date = input$dates , gender_filter = "both" , dist = 8 )
  }) 
  
  
#Events Page Date Auto Update----------------
  observeEvent(input$season, {
    {
    filtered_season <- tt_results %>%  filter(season == input$season) %>% dplyr::select(date, event) %>% unique() %>% dplyr::arrange(desc(date)) %>% mutate(date_event = paste0(date," - ",event))
    }
    updatePickerInput(
      session,
      inputId = "dates",
      choices = filtered_season$date_event,  #need to convert it back when filtering tables and plots
      )})
  
  event_trns <- reactive({ event_results(tt_results, date = input$dates) })
  
  output$event_results_table <- renderDT({ datatable(event_trns(), 
                                                       options = list(info = F,
                                                                      paging = F,
                                                                      searching = F,
                                                                      stripeClasses = F, 
                                                                      lengthChange = F,
                                                                      scrollY = '800px',
                                                                      scrollCollapse = F,
                                                                      columnDefs = list(list(className = 'dt-center', 
                                                                                  targets = "_all"))),
                                                       rownames = F,
                                                       selection = 'none') })
  
  event_trns_male <- reactive({ leaderboard_table(tt_results, gender_filter = "male" , dist_filter = 16, bike_type_filter = input$bike_type, leaderboard_type = input$leaderboard_type , ag_filter = input$age_group, date_filter = input$leaderboard_date_filter) })
  
  output$leaderboard_table_male <- renderDT({ datatable(event_trns_male(), 
                                                     options = list(info = F,
                                                                    paging = F,
                                                                    searching = F,
                                                                    stripeClasses = F, 
                                                                    lengthChange = F,
                                                                    scrollY = '750px',
                                                                    scrollCollapse = F,
                                                                    columnDefs = list(list(className = 'dt-center', 
                                                                                           targets = "_all"))),
                                                     rownames = F,
                                                     selection = 'none') })  
  
  event_trns_female <- reactive({ leaderboard_table(tt_results, "female" , dist_filter = 16, bike_type_filter = input$bike_type, leaderboard_type = input$leaderboard_type , ag_filter = input$age_group, date_filter = input$leaderboard_date_filter) })
  
  output$leaderboard_table_female <- renderDT({ datatable(event_trns_female(), 
                                                        options = list(info = F,
                                                                       paging = F,
                                                                       searching = F,
                                                                       stripeClasses = F, 
                                                                       lengthChange = F,
                                                                       scrollY = '750px',
                                                                       scrollCollapse = F,
                                                                       columnDefs = list(list(className = 'dt-center', 
                                                                                              targets = "_all"))),
                                                        rownames = F,
                                                        selection = 'none') })
  
  
  
  weather_trns <- reactive({ weather_results(weather, date = input$dates) })
  
  output$weather_table <- renderDT({ datatable(weather_trns(), 
                                                     options = list(info = F,
                                                                    paging = F,
                                                                    searching = F,
                                                                    stripeClasses = F, 
                                                                    lengthChange = F,
                                                                    scrollY = '440px',
                                                                    scrollCollapse = F,
                                                                    columnDefs = list(list(className = 'dt-center', 
                                                                                           targets = "_all"))),
                                                     rownames = F,
                                                     selection = 'none') })
  
  
}

# Run app -------------------------------
shinyApp(ui = ui, server = server)
