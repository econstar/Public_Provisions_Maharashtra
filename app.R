#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(ggplot2)
library(dplyr)
library(sf)
library(viridis)
library(plotly)

#DATA----
new <- read.csv("data.csv")
MH <- st_read("india_Subdistrict_27.shp")

#UI--------
ui <- fluidPage(
    titlePanel(
        div(
            "Public Provisions in Maharashtra",
            style = "color: #2c3e50; font-weight: bold; font-size: 24px; margin-bottom: 20px;"
        )
    ),
    
    sidebarLayout(
        sidebarPanel(
            width = 3,
            style = "background-color: #ecf0f1; padding: 15px; border-radius: 5px;",
            helpText("Select a variable to visualize on the map."),
            
            
            selectInput("variable", "Choose a variable:",
                        choices = names(new)[sapply(new, is.numeric) & !(names(new) %in% c("lat", "long"))],
                        selected = "underweight"),
            
            hr(),
            p("Size and color represent the intensity of the selected variable",
              style = "font-size: 14px; color: #7f8c8d;")
        ),
        
        mainPanel(
            width = 9,
            fluidRow(
                column(
                    width = 12,
                    plotlyOutput("bubbleMap", height = "800px")  
                )
            ),
            style = "padding: 20px; background-color: #ffffff; border-radius: 10px;"
        )
    )
)

#SERVER
server <- function(input, output) {
    output$bubbleMap <- renderPlotly({
        
        
        size_var <- new[[input$variable]]
        size_scaled <- (size_var - min(size_var)) / (max(size_var) - min(size_var)) * 5  
        color_var <- new[[input$variable]]
        
        #PLOT-----
        plot_ly(data = new, 
                lat = ~lat, 
                lon = ~long, 
                size = size_scaled,  
                color = color_var, 
                colors = viridis::viridis(100), 
                type = "scattermapbox", 
                mode = "markers", 
                marker = list(sizemode = "diameter", opacity = 0.5),
                text = ~paste("subdistrict:", new$sdtname),   
                hoverinfo = "text"  
        ) %>%  
            layout(
                title = list(
                    text = paste("Distribution of", input$variable),
                    font = list(size = 15)  
                ),
                
                mapbox = list(
                    style = "carto-positron",
                    center = list(lon = mean(new$long), lat = mean(new$lat)),
                    zoom = 6
                ),
                sliderdefaults = list(
                    frames = 100, 
                    redraw = TRUE
                ),
                showlegend = FALSE
            ) %>%
            animation_opts(frame = 100, redraw = TRUE, mode = "immediate")
    })
}

#SHINY
shinyApp(ui = ui, server = server)

