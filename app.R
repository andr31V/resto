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
        color: #1A4C64; 
      }
      
      .box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#1A4C64
                    }

.box.box-solid.box-primary{
border-bottom-color:#1A4C64;
border-left-color:#1A4C64;
border-right-color:#1A4C64;
border-top-color:#1A4C64;
}

body { 
            background-color: pink;
            color:black
            
            
}


#buy{background-color:yellow}
#sell{background-color:red}

.dataTables_filter, .dataTables_info, .dataTables_paginate, .dataTables_length   {
color: black !important;
}

<!-- 
above block is used for data table navigation text
-->
    "))
  ),
  headerPanel(
    h1("Fabro")
    ),
  sidebarLayout( 
    sidebarPanel(width=4,
      selectInput("user","Select User",choices=c("Enter Your Name"="",names), selected=boss),
      fluidRow(column(12,h4(HTML("<b>Schedule Horizon:</b>")))),
      selectInput("name","Select Staff",choices=names,selected=boss,multiple=TRUE),
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
      uiOutput("boss"),
      tags$img(src='pasta.png', align = "center", width="100%"
                                  #style="opacity: 0.2;"
               )
      
                      
      
    ),
    mainPanel(
      column(12,h1("Schedule")),
      DTOutput(outputId = "schedule"),
      column(12,h1("Changes")),
      DTOutput(outputId = "details"),
      fluidRow(column(1,actionButton("sell","   Sell   ")),
               column(1,actionButton("buy","   Buy   "))
               ),
      fluidRow(tableOutput("test")),
      fluidRow(tableOutput("test2"))
      
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
    select(-id) %>% 
    unite(Hours,-Staff,sep="|",remove=FALSE) %>% 
    mutate(Hours=if_else(str_replace_all(Hours,"[\\|]","")=="","0",Hours)) %>% 
    mutate(Hours=str_replace_all(Hours, "[\\|]", "+")) %>% 
    rowwise() %>% 
    mutate(Hours=-1*eval(parse(text=Hours)))

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
    
    validate(need(input$date1<=input$date2 ,"Please select a valid time frame"))
    
    d1 <- input$date1
    d2 <- input$date2 
    days <- seq(d1,d2,1)
    selectInput("date","Select Working Dates",choices=days,multiple=TRUE)
  })
  
  output$boss <- renderUI({
    
    validate(need(input$user==boss , paste("Only",boss,"can submit shifts")))
    validate(need(!is.null(input$name) ,"Please select staff to assign"))
    validate(need(!is.null(input$date), "Please select a date to assign"))
    validate(need(input$start<input$end ,"Please select a valid time frame"))
    
    actionButton("submit","Submit")

  })
  
  output$schedule <- renderDT({
    var <- if_else(input$user==boss,TRUE,FALSE)
   datatable(values$df, editable=var,
             selection=list(mode="single", target="cell")) 
  })
  
  observeEvent(input$submit,{
    
      #step1: create concatenated shift field in case of multiple shifts per day (add-on instead of erasing)
      shift <- paste(input$start,input$end,sep='-') 
      old_shift <- values$df[values$df$Staff %in% input$name,input$date][[1]]
      if(grepl(str_replace_all(shift,c(":30"=".5","[//|]"="")),str_replace_all(old_shift,c(":30"=".5","[//|]"="")))==FALSE) #prevent duplicates!
      {
      values$df[values$df$Staff %in% input$name,input$date] <- #str_replace( 
        as.character(
        paste(
        old_shift,
        shift,
        sep="|")
      )
      #, "[//|]", "")
      values$df <- values$df %>% 
        select(-Hours) %>% #remove the current hours calculation
        unite(Hours,-Staff,sep="|",remove=FALSE) %>%  #create a combined column of all shift strings
        mutate(Hours=if_else(str_replace_all(Hours,"[\\|]","")=="","0",Hours)) %>%  #if there are no shifts, set hours=0. otherwise...
        mutate(Hours=str_replace_all(Hours, "[\\|]", "+")) %>% #replace the shift separator - | - into the + sign to automatically calculate
        mutate(Hours=str_replace_all(Hours,":30", '.5')) %>%  #ensure formatted data read in correctly for mathematical calculation
        rowwise() %>%  #calculate row by row
        mutate(Hours=-1*eval(parse(text=Hours))) %>% #convert numeric and mutiply by -1 since the shift formatter (-) sums up negatively 
        mutate_at(vars(-Staff,-Hours),funs(str_replace_all(.,"[//.]5", ':30'))) #reformat the shifts for output
    }
     })
  
  observeEvent(input$schedule_cell_edit, {
    cell <- input$schedule_cell_edit
    values$df[cell$row, cell$col] <- cell$value
    values$df <- values$df %>% 
      select(-Hours) %>% 
      mutate_all(funs(str_replace_all(.,":30", '.5'))) %>% 
      unite(Hours,-Staff,sep="|",remove=FALSE) %>% 
      mutate(Hours=if_else(str_replace_all(Hours,"[\\|]","")=="","0",Hours)) %>% 
      mutate(Hours=str_replace_all(Hours, "[\\|]", "+")) %>% 
      rowwise() %>% 
      mutate(Hours=-1*eval(parse(text=Hours))) %>% 
      mutate_at(vars(-Staff,-Hours),funs(str_replace_all(.,"[//.]5", ':30')))
  })
  
  
  #data.frame(x="12-13|13-14") %>%  separate(x,c("a","b"),sep = "([\\|])") %>% 
  #mutate(test=eval(parse(text=a)))
  
  output$details <- renderDT({
    #only allow sellers to edit their own data
    if (length(input$schedule_cells_selected) > 0) {
    r <- input$schedule_cells_selected[1,1]
    seller <- values$df[r,]$Staff
    var <- if_else(input$user==seller,TRUE,FALSE)
    }
    else {
    var <- FALSE
    }
    datatable(values$df2, editable=var,selection=list(mode="single"))
  })
  
  observeEvent(input$sell,{
    
  validate(need(length(input$schedule_cells_selected) > 0 ,"Select shift desired for sale"))
      
      r <- input$schedule_cells_selected[1,1]
      c <- input$schedule_cells_selected[1,2]
      
      validate(need(input$user==values$df[r,1] ,"Access denied: this is not your shift!"))
      
      add_rows <- data.frame(Shift_Date=colnames(values$df[c]),
                             Shift_Time=as.character(values$df[r,c]),
                             Seller=input$user,
                             Seller_Comments="",
                             Buyer=""
                             #Buyer_Comments="",
                             #Manager_Comments=""
                             )
      
      values$df2 <- values$df2 %>% 
        bind_rows(add_rows) 

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
  
  output$test<- renderTable({
    test <- values$df %>% 
      select(-Hours) %>% 
      gather(Date,Shift,-Staff) %>% 
      filter(Shift!=""&is.null(Shift)==FALSE)
    test
  })
  
  output$test2 <- renderTable({
    values$df2
  })
  
  
  
}


shinyApp(ui, server)