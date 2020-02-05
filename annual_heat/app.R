#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(plotly)
library(rsconnect)


ambulatory_joined <-
    readr::read_rds("../annual_heat/data/joined_counts_scaled.rds") %>% 
    mutate(rate = (count / population) * 1000)
    

glmer_ambulatory <-
    readr::read_rds("../annual_heat/data/random_slope_bct_nest.rds")

index_choices <- glmer_ambulatory$index


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Annual Heat Stress Illness Models - US Army"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("View plots, tables, and models of annual indices of environmental heat and Heat Stress Illness encounters."),
            
            selectInput("type",
                        label = "Select encounter type",
                        choices = list("Out-patient", 
                                       "In-patient"),
                        selected = "Out-patient"),
            
            selectInput("index",
                        label = "Select index of heat", 
                        index_choices)
        ),
            

        # Show a scatterplot of exposure-response
        mainPanel(
           plotlyOutput("scatter_plot1"),
           br(),
           plotlyOutput("scatter_plot2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatter_plot1 <- renderPlotly({
        # generate plot based on input$index from ui.R
    
        gg_ambulatory_scatter <-
            ambulatory_joined  %>% 
                filter(index %in% input$index) %>% 
                ggplot(aes(x = value, y = rate, color = installation, shape = installation, 
                           text = paste("installation:", installation, "<br>", 
                                        "year:", year, "<br>", 
                                        "index value:", round(value, digits = 2), "<br>", 
                                        "HSI rate:", round(rate, digits = 2)), 
                            group = installation)) +
                geom_point() +
            
                geom_smooth(method = lm, se = FALSE, size = 0.8) +
                labs(
                    title = "Army Heat Stress Illness Ambulatory Rates (1997-2018)",
                            x = input$index,
                            y = "HSI rate (per 1,000 persons per year)"
                        ) +
                scale_shape_manual(values = 0:11) +
                        theme_bw()
            
        ggplotly(gg_ambulatory_scatter, tooltip = c("text")) %>% 
            print
        }
    )
        
        
        output$scatter_plot2 <- renderPlotly({
            # generate plot based on input$index from ui.R
            
            gg_ambulatory_scatter_counts <-
                ambulatory_joined  %>% 
                filter(index %in% input$index) %>% 
                ggplot(aes(x = value, y = count, color = installation, shape = installation, 
                           text = paste("installation:", installation, "<br>", 
                                        "year:", year, "<br>", 
                                        "index value:", round(value, digits = 2), "<br>", 
                                        "HSI count:", round(count, digits = 2)), 
                           group = installation)) +
                geom_point() +
                
                geom_smooth(method = lm, se = FALSE, size = 0.8) +
                labs(
                    title = "Army Heat Stress Illness Ambulatory Counts (1997-2018)",
                    x = input$index,
                    y = "HSI count"
                ) +
                scale_shape_manual(values = 0:11) +
                theme_bw()
            
            ggplotly(gg_ambulatory_scatter_counts, tooltip = c("text")) %>% 
                print
        
    
    })
}

 
# Run the application 
shinyApp(ui = ui, server = server)
