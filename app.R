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



value = 0

server <- function(input, output) {
  
  output$myWineList = DT::renderDataTable({
    winelist
  })
  
  output$maxWorldPrice <- renderInfoBox({ 
    max_price <- winelist %>% filter(country == input$graph_country) %>% summarise(max_price = max(price)) %>% select(max_price)
    infoBox('Max Price', max_price, icon = icon('hand-o-up'), color = 'green') 
  }) 
  
  output$scatterByCountry <- renderPlotly({
    scatter_graph_country <- winelist %>% filter(country == input$graph_country) %>% arrange(desc(points)) %>% head(100) %>% ggplot(aes(x = price, y = points)) + geom_point(aes(color = winery, shape = "21"), alpha = 0.4) + xlab('Price per Bottle (in USD)') + ylab('Points') + ggtitle(paste('Price vs. Points: Top 100 Wines from ',input$graph_country)) + theme(
      plot.title=element_text(face='bold')) +theme(legend.position='none')
    
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
      icon = icon("globe", class = NULL, lib = "font-awesome")
    )
  })
  
}

sidebar <- dashboardSidebar(
  sidebarMenu(
    #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Graphs", tabName = "graphs", icon = icon("chart-line",class = NULL, lib = "font-awesome")),
    menuItem("Predict", tabName = "myPrediction", icon = icon("robot", class = NULL, lib = "font-awesome")),
    menuItem("Data Set", tabName = "myDataset", icon = icon("robot", class = NULL, lib = "font-awesome"))
    
  )
)


body <- dashboardBody(
  tabItems(
    # First tab content
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
              h3("Predicted Result: ", textOutput('prediction'))
            ),
            fluidRow(
              box(
                title = "Winery",
                selectInput(inputId='myWinery', label='Winery', unique(winelist$winery))
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "graphs",
            navlistPanel('Graphs and Charts', 
                         'Interactive: Geographic', 
                         tabPanel('By Country',  
                                  
                                  selectizeInput(
                                    "graph_country",
                                    'Select a country:', 
                                    choices = sort(unique(winelist$country)), 
                                    multiple = F, 
                                    selected = 'Argentina'
                                  ),
                                  
                                  fluidRow(
                                    infoBoxOutput("maxWorldPrice")
                                  ),
                                  fluidRow(
                                    plotlyOutput("scatterByCountry")
                                  )
                         )
            )
    ),
    tabItem(tabName = "myDataset", h2("The winelist data"),
            DT::dataTableOutput("myWineList")
    
    )
  )
)

ui <- dashboardPage(header = dashboardHeader(),
                    sidebar = sidebar,
                    body = body
)

shinyApp(ui, server)
