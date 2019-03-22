#The data is found at https://data.ny.gov/Transportation/NYC-Transit-Subway-Entrance-And-Exit-Data/i9wp-a4ja. 
#This data file provides a variety of information on subway station entrances and exits.
#Through my shiny app, the New York government can invest more money on training workers to repair certain entrance type whose number is the most. 
#Besides, when workers want to repair facility, they know where to go. 

options(shiny.maxRequestSize = 30*2014^2)
library(shiny)
library(data.table)
library(leaflet)
library(dplyr)
library(sqldf)
library(shinythemes)
library(ggplot2)
#define UI for applications that analyzes the operation of subway system

ui <- shinyUI(fluidPage(
  
  theme = shinytheme("flatly"),
  
  #application title
  headerPanel("NYC Transit Subway Entrance And Exit Data"),
  
  #sidebar for uploading the file
  sidebarPanel(
    
    #create a file input
    fileInput("file","Choose a csv file",
              multiple = TRUE,
              accept = c("csv",
                         "text/csv",
                         "text/comma-separated-values,text/plain",".csv")),
    
   
    checkboxGroupInput("Division",
                       "Division",
                       choices = list("Brooklyn-Manhattan_Transit" = "BMT",
                                      "Independent_Subway_System "="IND",
                                      "Interborough_Rapid_Transit" = "IRT")),
    
    hr(),
    helpText("Please Select Division"),
    helpText("You Can Choose More Than One"),
    
    #create a multiple checkbox for manufacture or sale of alcoholic beverages.
    checkboxGroupInput("corner",
                       "Corner",
                       choices = list("Northeast" = "NE",
                                      "North "="N",
                                      "Northwest" = "NW",
                                      "South" = "S",
                                      "Southeast" = "SE",
                                      "Southwest" = "sw",
                                      "West" = "W"),
    helpText("Please Select the Type You Want To analyze")),
    helpText("You Can Choose More Than One"),
    
    selectInput("color","Choose Bar Color",c("yellow","orange","blue","red","pink")),
    # make the sidebar on the right of the webpage
    position = "right",
    fluid = TRUE
  ),
  #show the map in the main panel
  mainPanel(
    hr(),
    tabsetPanel(type = "tabs",
                
                #add a tab for problem description
                tabPanel("Problem Description", textOutput("text")),
                
                #Add a tab for decriptive table
                tabPanel("Descriptive Analysis",
                         
                         #Add two subtabs
                         tabsetPanel(
                           tabPanel("Bar chart",plotOutput("table1")),
                           tabPanel("Summary information",verbatimTextOutput("table2"))
                         )
                         ),
                tabPanel("map",leafletOutput("map",height=630))
                
                )
  )
  
))

#define Server for applications that analyzes manufacture and sale of alcoholic beverages.

server <- function(input,output,session){

  #create an output variable for problem description
    output$text <- renderText({
      "This project uses the dataset 'NYC_Transit_Subway_Entrance_And_Exit'. The dataset contains information for subway information, including Division, line, Station Name,latitude, longitude, route information,Entrance,Entry,Exit Only, Vending, Staffing, Staff Hours, Corners and etc. Question: Do all the subway station have many types of entrance? Do some division have more entrance type while others not?  Is there a correlation between corner and entrance type? To answer this question, we analyze the types of entrance, division , corner and create a map for visualization. "
    })
    
    output$table1 <- renderPlot({
    infile <- input$file
    if(is.null(infile))
      return(NULL)
    mydata1 <- read.csv(infile$datapath)
    attach(mydata1)
    
    mydata1 <- as.data.frame(mydata1)
 
    target1 <- c(input$Division)
    target2 <- c(input$corner)
    mydata2 <- filter(mydata1,Division %in% target1 & Corner %in% target2)
    
    
    if (input$color != 'None')
      #p <- p + aes_string(color=input$color)
      g <- ggplot(mydata2,aes(x=Entrance_Type,label=Entrance_Type))+geom_bar(fill=input$color)
      g
   
  })
  
    output$table2 <- renderPrint({
      infile <- input$file
      if(is.null(infile))
        return(NULL)
      mydata1 <- read.csv(infile$datapath)
      attach(mydata1)
      
      mydata1 <- as.data.frame(mydata1)
      
      target1 <- c(input$Division)
      target2 <- c(input$corner)
      mydata2 <- filter(mydata1,Division %in% target1 & Corner %in% target2)
      
      #g <- ggplot(mydata2,aes(x=Entrance_Type))+geom_bar()
      #g
      table(mydata2$Entrance_Type)
      
      #barplot(table(mydata2$Entrance_Type),xlab = "Entrance_Type",ylab = "Frequency")
      
    })
    
    
  output$map <- renderLeaflet({

    infile <- input$file
    if(is.null(infile))
      return(NULL)
    mydata1 <- read.csv(infile$datapath)
    attach(mydata1)


    mydata1 <- as.data.frame(mydata1)
    
    target1 <- c(input$Division)
    target2 <- c(input$corner)
    

    mydata2 <- filter(mydata1,Division %in% target1 & Corner %in% target2)
    color <- colorFactor(rainbow(9),mydata2$Entrance_Type)

    leaflet(mydata2)%>%
        setView(lng = -74.003549, lat = 40.655144,zoom = 12) %>%
        addProviderTiles("CartoDB.Positron",
                         options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        radius = 5,
        lng= mydata2$Longitude,
        lat= mydata2$Latitude,
        stroke= TRUE,
        fillOpacity=1,
        color=color(Entrance_Type)
      ) %>%
      # Add legends for different types of crime
      addLegend(
        "bottomright",
        pal=color,
        values=Entrance_Type,
        opacity=0.5,
        title = " Entrance Type"

      )



    })

    
}   

    

  


shinyApp(ui = ui,server = server)

