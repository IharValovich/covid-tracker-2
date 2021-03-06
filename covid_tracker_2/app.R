
####### NEW CODE
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(utils)
library(httr)

library(ggplot2)
library(plotly)
library(ggthemes)
library(colourpicker)
library(readr)
library(lubridate)
library(dplyr)
library(DT)
library(shinydashboard)

#download the dataset from the ECDC website to a local temporary file
#GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_memory())
#write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", stringsAsFactors = TRUE)

data$dateRep   <- as.character(data$dateRep)   
data <- data %>%
    mutate(days = dmy(data$dateRep)) %>%
    filter(cases>=0 & deaths >= 0)
total_cases <- data %>% summarise(sum(cases))
total_deaths <- data %>%  summarise(sum(deaths))

# current date
# current_data <- data %>%
#         filter(days == Sys.Date())

# add coordinate to current date dataframe
countries <- read.csv("https://raw.githubusercontent.com/IharValovich/covid-tracker-2/master/covid_tracker_2/Raw-Data/countries.csv")
data_current_data <- inner_join(data, countries,by = c("geoId" ="country" ))
data_current_data <- filter(data_current_data, cases != 0)
data_current_data <- filter(data_current_data, days == Sys.Date())

      #create geom_segment plot cases/continentExp
# ggplot(data_current_data, aes(x = cases, y =  continentExp)) +
#     geom_point(size = 4) +
#     geom_segment(aes(xend = 120, yend =  continentExp), size = 1)

# Define UI for application that draws a histogram
ui <- fluidPage(#theme = "bootstrap.css",
    titlePanel(h1(strong("COVID-19 Dashboard"), img(src = "covid-19-logo.jpg", height = 132, width = 132, align = "right"),align = "center", style = "color:#205d87")),
    
    sidebarLayout(
        sidebarPanel(
            
            selectInput("continents", "Country",
                        choices = levels(data$countriesAndTerritories),
                        multiple = FALSE,
                        selected = "Poland"),
            numericInput("size", "Point size", 1, 1, 5),
            checkboxInput("fit", "Add line of best fit", FALSE),
            colourInput("color", "Point color", value = "red"),
            
            img(src = "logo-ecdc.png"),
            h5("The downloadable data file from European Centre for Disease Prevention and Control is updated daily and contains the latest available public data on COVID-19", style = "color:#205d87")
            
        ),
        
        mainPanel(
            
            tabsetPanel(
                tabPanel("Map", leafletOutput("mymap")),
                tabPanel("Deaths", plotlyOutput("plot")), 
                tabPanel("Cases", plotlyOutput("plot_cases")),
                tabPanel("Table", DT::dataTableOutput("table"))
            ),
            # plotlyOutput("plot"),
            # plotlyOutput("plot_cases")
            #leafletOutput("mymap")
            # valueBox(total_deaths, "subtitle", icon = NULL, color = "aqua", width = 4,
                     # href = NULL)
        )
    )
)

# Define the server logic
server <- function(input, output) {
    # Replace the `renderPlot()` with the plotly version
    
    # Replace the `renderPlot()` with the plotly version
    output$plot_cases <- renderPlotly({
        # Convert the existing ggplot2 to a plotly plot  -new cases
        ggplotly({
            data <- subset(data,
                           countriesAndTerritories %in% input$continents) # &
            #year >= input$years[1] & year <= input$years[2])
            
            p_cases <- ggplot(data, aes(days, cases)) +
                
                geom_point(size = input$size, col = input$color, alpha = 0.7) + geom_smooth(se =FALSE) +
                labs(x = "Date", y = "Number of cases") +
                ggtitle(input$continents) +
                theme_calc()      #theme(rect = element_rect(fill = "grey"))
            
            if (input$fit) {
                p_cases <- p_cases + geom_smooth(method = "lm")
            }
            p_cases
        })
    })
    
    output$plot <- renderPlotly({
        # Convert the existing ggplot2 to a plotly plot
        ggplotly({
            data <- subset(data,
                           countriesAndTerritories %in% input$continents) # &
            #year >= input$years[1] & year <= input$years[2])
            
            p <- ggplot(data, aes(days, deaths)) +
                
                geom_point(size = input$size, col = input$color, alpha = 0.7) + geom_smooth(se =FALSE) +
                labs(x = "Date", y = "Number of deaths") +
                ggtitle(input$continents) +
                theme_fivethirtyeight()                                        #theme(rect = element_rect(fill = "grey"))
                
            
            if (input$fit) {
                p <- p + geom_smooth(method = "lm")
            }
            p
        })
    })
    #MAP-------------------------------------  
    output$mymap <- renderLeaflet({
        # old map   ---- 
        #     m <- leaflet() %>%
        # addTiles() %>% # Add default OpenStreetMap map tiles
        # setView( lng = 19.145136, lat = 51.919438, zoom = 4 ) %>% 
        # addProviderTiles("Esri.WorldImagery")
        # old map ----   
        
        # new map  ==============================
        
        m <- leaflet(data_current_data) %>% addTiles() %>%
            setView( lng = 35.243322, lat = 38.963745, zoom = 1.5)%>%
            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       radius = ~ sqrt(cases) *4000 ,  stroke = FALSE, color ="red", opacity = 1,   
                       fillOpacity = 1
            )
        
        
        # new map   ==============================
        
        m  # Print the map
    })
    
    #End Map________-----------------------------------------------
    
    output$table <- DT::renderDataTable({
        
        data <- subset(data,
                       countriesAndTerritories %in% input$continents)
        data_for_table <- data %>% select(-2, -3, -4, -8, -9, -12)
        
        DT::datatable(data_for_table, rownames = FALSE, 
        colnames = c('date'= 1, 'country' = 4, 'population'= 5, 'continent' = 6))
    })
}

shinyApp(ui = ui, server = server)















# #  ######  OLD CODE    OLD CODE    OLD CODE
# # This is a Shiny web application. You can run the application by clicking
# # the 'Run App' button above.
# #
# # Find out more about building applications with Shiny here:
# #
# #    http://shiny.rstudio.com/
# #
# 
# library(shiny)
# library(leaflet)
# library(utils)
# library(httr)
# 
# library(ggplot2)
# library(plotly)
# library(colourpicker)
# library(readr)
# library(lubridate)
# library(dplyr)
# 
# #download the dataset from the ECDC website to a local temporary file
# #GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_memory())
# #write_disk(tf <- tempfile(fileext = ".csv")))
# 
# #read the Dataset sheet into “R”. The dataset will be called "data".
# data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", stringsAsFactors = TRUE)
# 
# data$dateRep   <- as.character(data$dateRep)   
# data <- data %>%
#     mutate(days = dmy(data$dateRep)) %>% 
#     select(-1,-2,-3,-4,-8,-9,-10)
# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(#theme = "bootstrap.css",
#     titlePanel(h1(strong("COVID-19 Dashboard"), img(src = "covid-19-logo.jpg", height = 132, width = 132, align = "right"),align = "center", style = "color:#205d87")),
#     
#     sidebarLayout(
#         sidebarPanel(
#             #textInput("title", "Title", "GDP vs life exp"),
#             selectInput("continents", "Country",
#                         choices = levels(data$countriesAndTerritories),
#                         multiple = FALSE,
#                         selected = "Poland"),
#             numericInput("size", "Point size", 1, 1, 5),
#             checkboxInput("fit", "Add line of best fit", FALSE),
#             colourInput("color", "Point color", value = "blue"),
#             #out = h5("This is a static text"),
#             
#             
#             # sliderInput("years", "Years",
#             # min(gapminder$year), max(gapminder$year),
#             # value = c(1977, 2002))
#             img(src = "logo-ecdc.png"),
#             h5("The downloadable data file from European Centre for Disease Prevention and Control is updated daily and contains the latest available public data on COVID-19", style = "color:#205d87")
#         ),
#         mainPanel(
#             # Replace the `plotOutput()` with the plotly version
#             tabsetPanel(
#                 tabPanel("Deaths", plotlyOutput("plot")), 
#                 tabPanel("Cases", plotlyOutput("plot_cases")),
#                 tabPanel("Map", leafletOutput("mymap")),
# 				tabPanel("Table", DT::dataTableOutput("table"))
#             ),
#            # plotlyOutput("plot"),
#            # plotlyOutput("plot_cases")
#             
#             
#         )
#     )
# )
# 
# # Define the server logic
# server <- function(input, output) {
#     # Replace the `renderPlot()` with the plotly version
#     
#     # Replace the `renderPlot()` with the plotly version
#     output$plot_cases <- renderPlotly({
#         # Convert the existing ggplot2 to a plotly plot  -new cases
#         ggplotly({
#             data <- subset(data,
#                            countriesAndTerritories %in% input$continents) # &
#             #year >= input$years[1] & year <= input$years[2])
#             
#             p_cases <- ggplot(data, aes(days, cases)) +
#                 
#                 geom_point(size = input$size, col = input$color, alpha = 0.7) + geom_smooth()
#             #scale_x_log10() +
#             
#             ggtitle(input$continents)
#             
#             if (input$fit) {
#                 p_cases <- p_cases + geom_smooth(method = "lm")
#             }
#             p_cases
#         })
#     })
#     
#     output$plot <- renderPlotly({
#         # Convert the existing ggplot2 to a plotly plot
#         ggplotly({
#             data <- subset(data,
#                            countriesAndTerritories %in% input$continents) # &
#             #year >= input$years[1] & year <= input$years[2])
#             
#             p <- ggplot(data, aes(days, deaths)) +
#                 
#                 geom_point(size = input$size, col = input$color, alpha = 0.7) + geom_smooth() +
#                 #scale_x_log10() +
#                 
#                 ggtitle(input$continents)
#             
#             if (input$fit) {
#                 p <- p + geom_smooth(method = "lm")
#             }
#             p
#         })
#     })
#     
#     output$mymap <- renderLeaflet({
#         m <- leaflet() %>%
#             addTiles() %>% # Add default OpenStreetMap map tiles
#             setView( lng = 19.145136, lat = 51.919438, zoom = 4 ) %>% 
#             addProviderTiles("Esri.WorldImagery")
#         # addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
#         m  # Print the map
#     })
# 	
# 	output$table <- DT::renderDataTable({
#         data <- subset(data,
#                        countriesAndTerritories %in% input$continents)
#         data
#     })
# }
# 
# shinyApp(ui = ui, server = server)
