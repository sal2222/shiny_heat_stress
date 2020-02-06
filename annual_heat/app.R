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

joined_hsi_hosp <- readr::read_rds("../annual_heat/data/joined_hsi_hosp.rds")
 
joined_hsi_rme <- readr::read_rds("../annual_heat/data/joined_hsi_rme.rds")    
    

glmer_ambulatory <-
    readr::read_rds("../annual_heat/data/random_slope_bct_nest.rds")

index_choices <- glmer_ambulatory$index

index_heat_key <- readr::read_rds("../annual_heat/data/index_heat_key.rds")    


# Define UI for application
ui <- navbarPage("Annual Heat Stress Illness Models - US Army",
        tabPanel("Ambulatory",
            fluidPage(

    # Sidebar with a select box input for index of heat 
                sidebarLayout(
                    sidebarPanel(
                     helpText("View interactive plots of annual indices of environmental heat and Heat Stress Illness encounters."),
  
                        selectInput("index1",
                        label = "Select index of heat", 
                        index_choices),
                     textOutput("description1"),
                     br(),
                     textOutput("type1")
        ),
            

        # Show a scatterplot of exposure-response
        mainPanel(
           plotlyOutput("scatter_plot1"),
           br(),
           plotlyOutput("scatter_plot2")
        )
       )
    )
),
        tabPanel("Hospitalization",
                 fluidPage(
                     
                     # Sidebar 
                     sidebarLayout(
                         sidebarPanel(
                             helpText("View interactive plots of annual indices of environmental heat and Heat Stress Illness encounters."),
                             
                             selectInput("index2",
                                         label = "Select index of heat", 
                                         index_choices),
                             textOutput("description2"),
                             br(),
                             textOutput("type2")
                         ),
                         
                         
                         # Show a scatterplot of exposure-response
                         mainPanel(
                             plotlyOutput("scatter_plot_hosp1"),
                             br(),
                             plotlyOutput("scatter_plot_hosp2")
                         )
                     )
                 )
        ),
        tabPanel("Reportable Events",
                 fluidPage(
                     
                     # Sidebar 
                     sidebarLayout(
                         sidebarPanel(
                             helpText("View interactive plots of annual indices of environmental heat and Heat Stress Illness encounters."),
                             
                             selectInput("index3",
                                         label = "Select index of heat", 
                                         index_choices),
                                         textOutput("description3"), 
                                         br(),
                                         textOutput("type3")
                         ),
                         
                         
                         # Show a scatterplot of exposure-response
                         mainPanel(
                             plotlyOutput("scatter_plot_rme1"),
                             br(),
                             plotlyOutput("scatter_plot_rme2")
                         )
                     )
                 )
        )
)


# Define server logic 
server <- function(input, output) {
    
    output$description1 <- renderText({ 
        paste("Index description:",
              (index_heat_key %>% filter(index %in% input$index1))$description)
    })
    
    output$description2 <- renderText({ 
        paste("Index description:",
              (index_heat_key %>% filter(index %in% input$index2))$description)
    })
    
    output$description3 <- renderText({ 
        paste("Index description:",
              (index_heat_key %>% filter(index %in% input$index3))$description)
    })
    
   
    output$type1 <- renderText({ 
        paste("Index measure type (Absolute or Relative):",
              (index_heat_key %>% filter(index %in% input$index1))$index_type)
    })
    
    output$type2 <- renderText({ 
        paste("Index measure type (Absolute or Relative):",
              (index_heat_key %>% filter(index %in% input$index2))$index_type)
    })
    
    output$type3 <- renderText({ 
        paste("Index measure type (Absolute or Relative):",
              (index_heat_key %>% filter(index %in% input$index3))$index_type)
    })
    
     

    output$scatter_plot1 <- renderPlotly({
        # generate plot based on input$index from ui.R
    
        gg_ambulatory_scatter <-
            ambulatory_joined  %>% 
                filter(index %in% input$index1) %>% 
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
                            x = input$index1,
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
                filter(index %in% input$index1) %>% 
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
                    x = input$index1,
                    y = "HSI count"
                ) +
                scale_shape_manual(values = 0:11) +
                theme_bw()
            
            ggplotly(gg_ambulatory_scatter_counts, tooltip = c("text")) %>% 
                print
        
    
    })


        output$scatter_plot_hosp1 <- renderPlotly({
            # generate plot based on input$index from ui.R
            
            gg_hosp_scatter_rates <-
                joined_hsi_hosp  %>% 
                filter(index %in% input$index2) %>% 
                ggplot(aes(x = value, y = rate, color = installation, shape = installation, 
                           text = paste("installation:", installation, "<br>", 
                                        "year:", year, "<br>", 
                                        "index value:", round(value, digits = 2), "<br>", 
                                        "HSI rate:", round(rate, digits = 2)), 
                           group = installation)) +
                geom_point() +
                
                geom_smooth(method = lm, se = FALSE, size = 0.8) +
                labs(
                    title = "Army Heat Stress Illness Hospitalization Rates (1990-2018)",
                    x = input$index2,
                    y = "HSI rate (per 1,000 persons per year)"
                ) +
                scale_shape_manual(values = 0:11) +
                theme_bw()
            
            ggplotly(gg_hosp_scatter_rates, tooltip = c("text")) %>% 
                print
        }
        )

        output$scatter_plot_hosp2 <- renderPlotly({
            # generate plot based on input$index from ui.R
            
            gg_hosp_scatter_counts <-
                joined_hsi_hosp  %>% 
                filter(index %in% input$index2) %>% 
                ggplot(aes(x = value, y = count, color = installation, shape = installation, 
                           text = paste("installation:", installation, "<br>", 
                                        "year:", year, "<br>", 
                                        "index value:", round(value, digits = 2), "<br>", 
                                        "HSI count:", round(count, digits = 2)), 
                           group = installation)) +
                geom_point() +
                
                geom_smooth(method = lm, se = FALSE, size = 0.8) +
                labs(
                    title = "Army Heat Stress Illness Hospitalization Counts (1990-2018)",
                    x = input$index2,
                    y = "HSI count"
                ) +
                scale_shape_manual(values = 0:11) +
                theme_bw()
            
            ggplotly(gg_hosp_scatter_counts, tooltip = c("text")) %>% 
                print
            
            
        })

        output$scatter_plot_rme1 <- renderPlotly({
            # generate plot based on input$index from ui.R
            
            gg_rme_scatter_rates <-
                joined_hsi_hosp  %>% 
                filter(index %in% input$index3) %>% 
                ggplot(aes(x = value, y = rate, color = installation, shape = installation, 
                           text = paste("installation:", installation, "<br>", 
                                        "year:", year, "<br>", 
                                        "index value:", round(value, digits = 2), "<br>", 
                                        "HSI rate:", round(rate, digits = 2)), 
                           group = installation)) +
                geom_point() +
                
                geom_smooth(method = lm, se = FALSE, size = 0.8) +
                labs(
                    title = "Army Heat Stress Illness Reportable Event Rates (1995-2018)",
                    x = input$index3,
                    y = "HSI rate (per 1,000 persons per year)"
                ) +
                scale_shape_manual(values = 0:11) +
                theme_bw()
            
            ggplotly(gg_rme_scatter_rates, tooltip = c("text")) %>% 
                print
        }
        )
        
        output$scatter_plot_rme2 <- renderPlotly({
            # generate plot based on input$index from ui.R
            
            gg_rme_scatter_counts <-
                joined_hsi_hosp  %>% 
                filter(index %in% input$index3) %>% 
                ggplot(aes(x = value, y = count, color = installation, shape = installation, 
                           text = paste("installation:", installation, "<br>", 
                                        "year:", year, "<br>", 
                                        "index value:", round(value, digits = 2), "<br>", 
                                        "HSI count:", round(count, digits = 2)), 
                           group = installation)) +
                geom_point() +
                
                geom_smooth(method = lm, se = FALSE, size = 0.8) +
                labs(
                    title = "Army Heat Stress Illness Reportable Event Counts (1995-2018)",
                    x = input$index3,
                    y = "HSI count"
                ) +
                scale_shape_manual(values = 0:11) +
                theme_bw()
            
            ggplotly(gg_rme_scatter_counts, tooltip = c("text")) %>% 
                print
            
            
        })
}


 
# Run the application 
shinyApp(ui = ui, server = server)
