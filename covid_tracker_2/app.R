#
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
library(colourpicker)
library(readr)
library(lubridate)
library(dplyr)

#download the dataset from the ECDC website to a local temporary file
#GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_memory())
#write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", stringsAsFactors = TRUE)

data$dateRep   <- as.character(data$dateRep)   
data <- data %>%
    mutate(days = dmy(data$dateRep)) %>% 
    select(-1,-2,-3,-4,-8,-9,-10)

# Define UI for application that draws a histogram
ui <- fluidPage(#theme = "bootstrap.css",
    titlePanel(h1(strong("COVID-19 Dashboard"), align = "center", style = "color:#2E86C1")),
    
    sidebarLayout(
        sidebarPanel(
            #textInput("title", "Title", "GDP vs life exp"),
            selectInput("continents", "Country",
                        choices = levels(data$countriesAndTerritories),
                        multiple = FALSE,
                        selected = "Poland"),
            numericInput("size", "Point size", 1, 1, 5),
            checkboxInput("fit", "Add line of best fit", FALSE),
            colourInput("color", "Point color", value = "blue"),
            #out = h5("This is a static text"),
            
            
            # sliderInput("years", "Years",
            # min(gapminder$year), max(gapminder$year),
            # value = c(1977, 2002))
            img(src = "logo-ecdc.png"),
            h5("The downloadable data file from European Centre for Disease Prevention and Control is updated daily and contains the latest available public data on COVID-19")
        ),
        mainPanel(
            # Replace the `plotOutput()` with the plotly version
            tabsetPanel(
                tabPanel("Deaths", plotlyOutput("plot")), 
                tabPanel("Cases", plotlyOutput("plot_cases")),
                tabPanel("Map", leafletOutput("mymap")),
				tabPanel("Table", DT::dataTableOutput("table"))
            ),
           # plotlyOutput("plot"),
           # plotlyOutput("plot_cases")
            
            
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
                
                geom_point(size = input$size, col = input$color, alpha = 0.7) + geom_smooth()
            #scale_x_log10() +
            
            ggtitle(input$continents)
            
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
                
                geom_point(size = input$size, col = input$color, alpha = 0.7) + geom_smooth() +
                #scale_x_log10() +
                
                ggtitle(input$continents)
            
            if (input$fit) {
                p <- p + geom_smooth(method = "lm")
            }
            p
        })
    })
    
    output$mymap <- renderLeaflet({
        m <- leaflet() %>%
            addTiles() %>% # Add default OpenStreetMap map tiles
            setView( lng = 19.145136, lat = 51.919438, zoom = 4 ) %>% 
            addProviderTiles("Esri.WorldImagery")
        # addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
        m  # Print the map
    })
	
	output$table <- DT::renderDataTable({
        data <- subset(data,
                       countriesAndTerritories %in% input$continents)
        data
    })
}

shinyApp(ui = ui, server = server)
