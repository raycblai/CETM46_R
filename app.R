library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)
library(caret)
library(rpart)
library(tidyr)
library(tidyverse)
library(googleVis)
library(leaflet)
library(maps)
library(jpeg)
library(plotly)
library(DT)


wine1 <- as.data.frame(read.csv('winemag-data_first150k.csv'))
wine2_raw <- as.data.frame(read.csv('winemag-data-130k-v2.csv'))
wine2 <- subset(wine2_raw, select=-c(taster_name, taster_twitter_handle,title))
winelist <- rbind(wine1,wine2) %>% as.tibble()
winelist <- na.omit(winelist)
str(winelist)
high_score <- 90
winelist$description <- NULL
winelist$high_rating <- as.factor(winelist$points > high_score)
head(winelist, 5)
set.seed(100)
indices = createDataPartition(winelist$high_rating, p = .2, list = F)
training = winelist[-indices, ]
testing = winelist[indices, ]
winemodel = rpart(high_rating~price+variety+winery, data=training)

winelist_world <- as.data.frame(winelist %>% group_by(country) %>%  summarise(qty = n())) 

winelist_variety <- winelist %>% group_by(variety) %>% mutate(v_count = n())
winelist_variety <- winelist_variety %>% filter(v_count > 1000)
sort(unique(winelist_variety$variety))

value = 0

server <- function(input, output) {
  
  output$myWineList = DT::renderDataTable({
    winelist
  })
  
  output$maxWorldPrice <- renderInfoBox({ 
    max_price <- winelist %>% filter(country == input$graph_country) %>% summarise(max_price = max(price)) %>% select(max_price)
    infoBox('Max Price', max_price, icon = icon('hand-point-up'), color = 'green') 
  }) 
  
  output$meanWorldPrice <- renderInfoBox({ 
    mean_price <- winelist %>% filter(country == input$graph_country) %>% summarise(mean_price = round(mean(price))) %>% select(mean_price)
    infoBox('Mean Price', mean_price, icon = icon('hand-point-right'), color = 'yellow') 
  }) 
  
  output$minWorldPrice <- renderInfoBox({ 
    min_price <- winelist %>% filter(country == input$graph_country) %>% summarise(min_price = min(price)) %>% select(min_price)
    infoBox('Min Price', min_price, icon = icon('hand-point-down'), color = 'red') 
  }) 
  
  output$maxWorldPoints <- renderInfoBox({ 
    max_points <- winelist %>% filter(country == input$graph_country) %>% summarise(max_points = max(points)) %>% select(max_points)
    infoBox('Max Points', max_points, icon = icon('hand-point-up'), color = 'green') 
  }) 
  
  output$meanWorldPoints <- renderInfoBox({ 
    mean_points <- winelist %>% filter(country == input$graph_country) %>% summarise(mean_points = round(mean(points))) %>% select(mean_points)
    infoBox('Mean Points', mean_points, icon = icon('hand-point-right'), color = 'yellow') 
  }) 
  
  output$minWorldPoints <- renderInfoBox({ 
    min_points <- winelist %>% filter(country == input$graph_country) %>% summarise(min_points = min(points)) %>% select(min_points)
    infoBox('Min Points', min_points, icon = icon('hand-point-down'), color = 'red') 
  }) 
  
  ##
  
  output$maxVarPrice <- renderInfoBox({ 
    max_price <- winelist_variety %>% filter(variety == input$graph_variety) %>% group_by(variety) %>% filter(!is.na(price)) %>% summarise(max_price = max(price)) %>% select(max_price)
    # max_price <- winelist_variety %>% filter(variety == input$graph_variety) %>% summarise(max_price = max(price)) %>% select(max_price)
    infoBox('Max Price', max_price, icon = icon('hand-point-up'), color = 'green') 
  }) 
  
  output$meanVarPrice <- renderInfoBox({ 
    mean_price <- winelist_variety %>% filter(variety == input$graph_variety) %>% group_by(variety) %>% filter(!is.na(price)) %>% summarise(mean_price = round(mean(price))) %>% select(mean_price)
    infoBox('Mean Price', mean_price, icon = icon('hand-point-right'), color = 'yellow') 
  }) 
  
  output$minVarPrice <- renderInfoBox({ 
    min_price <- winelist_variety %>% filter(variety == input$graph_variety) %>% group_by(variety) %>% filter(!is.na(price)) %>% summarise(min_price = min(price)) %>% select(min_price)
    infoBox('Min Price', min_price, icon = icon('hand-point-down'), color = 'red') 
  }) 
  
  output$maxVarPoints <- renderInfoBox({ 
    max_points <- winelist_variety %>% filter(variety == input$graph_variety) %>% group_by(variety) %>% filter(!is.na(points)) %>% summarise(max_points = max(points)) %>% select(max_points)
    infoBox('Max Points', max_points, icon = icon('hand-point-up'), color = 'green') 
  }) 
  
  output$meanVarPoints <- renderInfoBox({ 
    mean_points <- winelist_variety %>% filter(variety == input$graph_variety) %>% group_by(variety) %>% filter(!is.na(points)) %>% summarise(mean_points = round(mean(points))) %>% select(mean_points)
    infoBox('Mean Points', mean_points, icon = icon('hand-point-right'), color = 'yellow') 
  }) 
  
  output$minVarPoints <- renderInfoBox({ 
    min_points <- winelist_variety %>% filter(variety == input$graph_variety) %>% group_by(variety) %>% filter(!is.na(points)) %>% summarise(min_points = min(points)) %>% select(min_points)
    infoBox('Min Points', min_points, icon = icon('hand-point-down'), color = 'red') 
  })
  
  output$scatterByVariety <- renderPlotly({
    scatter_graph_Variety <- winelist_variety %>% filter(variety == input$graph_variety) %>% arrange(desc(points)) %>% head(100) %>% ggplot(aes(x = price, y = points)) + geom_point(aes(color = country), alpha = 0.4) +  xlab('Price per Bottle (in USD)')+ ylab('Points') + ggtitle('Price vs. Points: Selected Variety Top 100 Wines') + theme(
      plot.title=element_text(face='bold')) + theme(legend.position='none')
  })
  
  ##
  
  
  output$scatterByCountry <- renderPlotly({
    scatter_graph_country <- winelist %>% filter(country == input$graph_country) %>% arrange(desc(points)) %>% head(100) %>% ggplot(aes(x = price, y = points)) + geom_point(aes(color = winery, shape = "21"), alpha = 0.4) + xlab('Price per Bottle (in USD)') + ylab('Points') + ggtitle(paste('Price vs. Points: Top 100 Wines from ',input$graph_country)) + theme(
      plot.title=element_text(face='bold')) +theme(legend.position='none')
    
  })
  
  
  output$world_map <- renderGvis({
    world_map <- gvisGeoChart(winelist_world, 'country', 'qty', options = list(width = 600, height = 400, displayMode = 'regions', colorAxis = "{colors:[ '#EADB9F', 'blue', 'purple', '#800020']}", title = 'Number of Wines per Country'))
  })
  
  
  
  winedata <- reactive({
    req(input$myVariety)
    req(input$myWinery)
    data.frame(X=as.integer(10000),
               country="US",
               designation="Reserve",
               points=as.integer(90),
               price=as.double(input$myPrice),
               province="California",
               region_1="Napa Valley",
               region_2="Napa",
               variety=input$myVariety,
               winery=input$myWinery
               
    )
  })
  
  wpred <- reactive({
    DEF <- predict(winemodel, winedata())
    recommendResult  = ifelse(DEF[1,1] < DEF[1,2], "GOOD", "BAD")
    recommendResult
  })
  
  output$prediction <- renderText({
    ABC <- predict(winemodel, winedata())
    str(ABC)
    endResult  = ifelse(ABC[1,1] < ABC[1,2], "GOOD", "BAD")
    endResult
  })
  
#  sentence <- reactive({
#    return(paste("This is : ",renderText({wpred()})))
#  })
  
  output$myResult <- renderValueBox({
    valueBox(
#      sentence(),
      renderText(wpred()),
      color = ifelse(wpred() == "GOOD","blue","red"),
      subtitle = "Recommendation",
      icon = icon("wine-glass", class = NULL, lib = "font-awesome")
    )
  })
  
}

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "intro", icon = icon("list-alt")),
    menuItem("Predict", tabName = "myPrediction", icon = icon("robot", class = NULL, lib = "font-awesome")),
    menuItem("Graphs", tabName = "graphs", icon = icon("chart-line",class = NULL, lib = "font-awesome")),
    menuItem("Map", tabName = "myMap", icon = icon("map", class = NULL, lib = "font-awesome")),
    menuItem("Data Set", tabName = "myDataset", icon = icon("database", class = NULL, lib = "font-awesome"))
    
  )
)


body <- dashboardBody(
  tabItems(
    #Zero tabItem
    
    tabItem(tabName = 'intro',  #intro 
            fluidRow(
              box(
                background = 'light-blue',
                h2('Wine Recommendation Platform'),
                
                tags$p("This is the wine application for the CETM46 Assignment Two written by Raymond Lai."), 
                tags$p(""), 
                tags$p("Submited on 27th Jan 2020"),
                tags$p(),
                tags$p("The dataset for this product was retrieved from Kaggle. The original data was scraped from WineEnthisiast in June 2017."), 
                tags$p ("The dataset contains country, description, designation, points, region, variety and winery."),
                tags$p("The dataset includes 280,000 records. For details, please refer to the URL below:"), 
                tags$a(href = "https://www.kaggle.com/zynicide/wine-reviews", style="color:yellow", "Wine-review Dataset."),
                tags$p(), 
                tags$p("Thank you."),
                tags$p("---"),
                tags$p("Raymond"),
                width = 48))
    ),
    
    
    # First tabItem 
    tabItem(tabName = "myPrediction",
            fluidRow(
              
              
              box(
                title = "Controls",
                sliderInput("myPrice", "Price:", value = 100,
                            min = min(winelist$price, na.rm = TRUE), 
                            max = max(winelist$price, na.rm = TRUE) 
                            )
              ),
              valueBoxOutput("myResult")
            ),
            fluidRow(
              box(
                title = "Variety",
                selectInput(inputId='myVariety', label='Variety', unique(winelist$variety))
              ),
              # h3("Predicted Result: ", textOutput('prediction'))
            ),
            fluidRow(
              box(
                title = "Winery",
                selectInput(inputId='myWinery', label='Winery', unique(winelist$winery))
              )
            )
    ),
    
    # Second tabItem 
    tabItem(tabName = "graphs",
            navlistPanel('Data Analytics', 
   #                      'Interactive: Geographic', 
                         tabPanel('By Country',  
                                  
                                  selectizeInput(
                                    "graph_country",
                                    'Select a country:', 
                                    choices = sort(unique(winelist$country)), 
                                    multiple = F, 
                                    selected = 'France'
                                  ),
                                  
                                  fluidRow(
                                    infoBoxOutput("maxWorldPrice"),
                                    infoBoxOutput("maxWorldPoints")
                                  ),
                                  fluidRow(
                                    infoBoxOutput("meanWorldPrice"),
                                    infoBoxOutput("meanWorldPoints")
                                  ), 
                                  
                                  fluidRow(
                                    infoBoxOutput("minWorldPrice"),
                                    infoBoxOutput("minWorldPoints")
                                  ), 
                                  
                                  fluidRow(
                                    plotlyOutput("scatterByCountry")
                                  )
                          ),

   ##
                          tabPanel('By Variety',  
            
                              selectizeInput(
                                "graph_variety",
                                'Select a variety:', 
                                choices = sort(unique(winelist_variety$variety)), 
                                multiple = F
                              ),
            
                              fluidRow(
                                infoBoxOutput("maxVarPrice"),
                                infoBoxOutput("maxVarPoints")
                              ),
                              fluidRow(
                                infoBoxOutput("meanVarPrice"),
                                infoBoxOutput("meanVarPoints")
                              ), 
            
                              fluidRow(
                                infoBoxOutput("minVarPrice"),
                                infoBoxOutput("minVarPoints")
                              ), 
            
                              fluidRow(
                                plotlyOutput("scatterByVariety")
                              )
                          )

##
                        
            )
    ),
   # Third tabItem 
    tabItem(tabName = "myMap", 
            fluidRow(
              valueBox(
                value = nrow(winelist),
                subtitle = "Wine Distribution (Worldwide)",
                icon = icon("wine")
              )
            ),
            fluidRow(box(width = 16, height = "80%", htmlOutput("world_map"))
                     
            )
            
    ),
   # Fourth tabItem 
    tabItem(tabName = "myDataset", h2("The winelist data"),
            DT::dataTableOutput("myWineList")
    
    )
  )
)

ui <- dashboardPage(header = dashboardHeader(title = "Raymond Lai - Wine App", titleWidth = 350),
                    sidebar = sidebar,
                    body = body, skin = "yellow"
)

shinyApp(ui, server)
