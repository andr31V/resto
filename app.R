library(RMySQL)
library(tidyverse)
library(shiny)
library(shinyjs)
library(DT)


names <- c("Jon","Luca","Munir")
boss <- "Jon"

ui <- 

fluidPage(
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=Dancing+Script&display=swap');
      
      h1 {
        font-family: 'Dancing Script', cursive;
        font-weight: 700;
        line-height: 1.1;
        color: #FBDD63;
      }
      
      .box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#FBDD63
                    }

.box.box-solid.box-primary{
border-bottom-color:#FBDD63;
border-left-color:#FBDD63;
border-right-color:#FBDD63;
border-top-color:#FBDD63;
}

    "))
  ),
  headerPanel(
    h1("Fabro")
    ),
  sidebarLayout( 
    sidebarPanel(width=4,
      selectInput("name","Select Name",choices=names,multiple=TRUE),
      fluidRow(column(12,h4(HTML("<b>Schedule Horizon:</b>")))),
      fluidRow(
        column(6,dateInput("date1","Start")),
        column(6,dateInput("date2","End"))
        ),
      uiOutput("day"),
      fluidRow(column(12,h5(HTML("<b>Select Shift Details:</b>")))),
      fluidRow(
        column(6,numericInput("start","Start",12,min=0,max=23,step=.5)),
        column(6,numericInput("end","End",12,min=0,max=23,step=.5))
      ),
      uiOutput("boss")
      
    ),
    mainPanel(
      #textOutput("test"),
      column(12,h1("Schedule")),
      DTOutput(outputId = "schedule"),
      DTOutput(outputId = "details")
      
    )
    
  ),
  
  fluidRow(
    )
  )
  


server <- function(input, output, session) {
  
  values <- reactiveValues()
  values$df <- data.frame()
  
  output$day <- renderUI({
    d1 <- input$date1
    d2 <- input$date2 
    days <- seq(d1,d2,1)
    selectInput("date","Select Working Dates",choices=days,multiple=TRUE)
  })
  
  output$boss <- renderUI({
    if(is.null(input$name)) {
      column(12,h6(""))
    }
    else if(input$name==boss) {
    actionButton("submit","Submit")
    }

  })
  
  output$schedule <- renderDT({
    d1 <- input$date1
    d2 <- input$date2 
    days <- seq(d1,d2,1)
    sched <- data.frame(
      id="",#c(1:length(days)),
      days=days
    ) %>% 
      spread(days,id) %>%
      mutate(id=1)
   staff <- data.frame(id=1,Staff=names)
   values$df <- staff %>% 
     inner_join(sched,by="id") %>% 
      select(-id)
    values$df 
  })
  
  output$test<- renderText({
    class(values$df$'2019-10-30')
  })
  
  
  
}


shinyApp(ui, server)