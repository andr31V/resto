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
      selectInput("user","Select User",choices=names),
      fluidRow(column(12,h4(HTML("<b>Schedule Horizon:</b>")))),
      selectInput("name","Select Staff",choices=names,multiple=TRUE),
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
      textOutput("test"),
      column(12,h1("Schedule")),
      DTOutput(outputId = "schedule"),
      column(12,h1("Changes")),
      DTOutput(outputId = "details"),
      fluidRow(column(1,actionButton("sell","   *Sell*   ")),
               column(1,actionButton("buy","   *Buy*   "))
               )
      
    )
    
  ),
  
  fluidRow(
    )
  )
  


server <- function(input, output, session) {
  
  values <- reactiveValues()
  
  observe({
    
  d1 <- input$date1
  d2 <- input$date2 
  days <- seq(d1,d2,1)
  sched <- data.frame(
    id="",
    days=days,
    stringsAsFactors = FALSE
  ) %>% 
    spread(days,id) %>%
    mutate(id=1)
  staff <- data.frame(id=1,Staff=names,stringsAsFactors = FALSE)
  
  values$df <- staff %>% 
    inner_join(sched,by="id") %>% 
    select(-id)

  values$df2 <- data.frame(
   Shift_Date=character(),
   Shift_Time=character(),
   Seller=character(),
   Seller_Comments=character(),
   Buyer=character(), #this should be a comma separated list
   #Buyer_Comments=character(),
   #Manager_Comments=character(),
   stringsAsFactors = FALSE)
  
  
  
  })
  
  output$day <- renderUI({
    d1 <- input$date1
    d2 <- input$date2 
    days <- seq(d1,d2,1)
    selectInput("date","Select Working Dates",choices=days,multiple=TRUE)
  })
  
  output$boss <- renderUI({
    if(is.null(input$user)) {
      column(12,h6(""))
    }
    else if(input$user==boss) {
    actionButton("submit","Submit")
    }

  })
  
  output$schedule <- renderDT({
    var <- if_else(input$user==boss,TRUE,FALSE)
   datatable(values$df, editable=var,
             selection=list(mode="single", target="cell")) 
  })
  
  observeEvent(input$submit,{
    if(!is.null(input$date)) {
      shift <- paste(input$start,input$end,sep='-') %>% 
        str_replace(".5", ':30')
      values$df[values$df$Staff %in% input$name,input$date] <-shift
    }
     })
  
  
  ###data.frame(x="12-13|13-14") %>%  separate(x,c("a","b"),sep = "([\\|])")
  
  output$details <- renderDT(
    datatable(values$df2, editable=TRUE,selection=list(mode="single")
    )
  )
  
  observeEvent(input$sell,{
    
    if(!is.null(input$schedule_cells_selected)) {
      
      r <- input$schedule_cells_selected[1,1]
      c <- input$schedule_cells_selected[1,2]
      
      if(input$user==values$df[r,1]) #only allow ppl to sell their OWN shifts - col 1 is name
      {
      add_rows <- data.frame(Shift_Date=colnames(values$df[c]),
                             Shift_Time=values$df[r,c],
                             Seller=input$user,
                             Seller_Comments="",
                             Buyer=""
                             #Buyer_Comments="",
                             #Manager_Comments=""
                             )
      
      values$df2 <- values$df2 %>% 
        bind_rows(add_rows) 
      }
      
      #values$selected<- NULL
    }
  })
  
  observeEvent(input$buy,{
    
    if(!is.null(input$details_rows_selected)) {
      
      #r <- input$schedule_cells_selected[1,1]
      #c <- input$schedule_cells_selected[1,2]
        
        values$df2 <- values$df2 %>% 
          mutate(Buyer=if_else(
            #Shift_Date==colnames(values$df[c]) & Shift_Time==values$df[r,c],
            row_number()==input$details_rows_selected,
            if_else(Buyer=="",input$user,paste(Buyer,input$user,sep=",")),
            Buyer
          )
          )

      
      #values$selected<- NULL
    }
  })
  
  observeEvent(input$details_cell_edit, {
    values$df2[input$details_cell_edit$row,input$details_cell_edit$col] <<- paste(
      input$details_cell_edit$value,paste0("[",input$user,"]")
    )
  })
  
  output$test<- renderText({
    row <- input$schedule_cells_selected[1,1]
    col <- input$schedule_cells_selected[1,2]
    colnames(values$df[col])
    input$details_rows_selected
  })
  
  
  
}


shinyApp(ui, server)