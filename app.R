library(RMySQL)
library(tidyverse)
library(shiny)
library(shinyjs)
library(DT)


names <- c("Jon","Luca","Munir")
boss <- "Jon"

Sys.setenv(TZ='CET') #copenhagen

options(mysql = list(
  "host" = "remotemysql.com",
  "port" = 3306,
  "user" = "T0oPZcxIgs",
  "password" = "ZM2LECu1kV"
))
databaseName <- "T0oPZcxIgs"


dbup <- function(dat,qry_up,qry_dup) {
#upload
  vals <- dat %>% 
    rowwise() %>% 
    mutate_all(funs(paste0("'",.,"'"))) %>% 
    unite(tmpvar,sep=",") %>% 
    mutate_all(funs(paste0("(",.,")"))) %>% 
    unlist() %>%  
    paste(.,collapse=",") %>% 
    as.character()
  query <- paste(qry_up,vals)
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  dbGetQuery(db, query)
#dedupe
  dbGetQuery(db,qry_dup)
  dbDisconnect(db)
  
} 

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
#del{background-color:blue}

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
      selectInput("user","Select User",choices=c("Enter Your Name"="",names)),
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
      uiOutput("boss"),
      tags$img(src='pasta.png', align = "center", width="100%"
                                  #style="opacity: 0.2;"
               )
      
                      
      
    ),
    mainPanel(
      column(12,h1("Schedule")),
      DTOutput(outputId = "schedule"),
      column(12,actionButton("upd_sched","Update")),
      column(12,h1("Changes")),
      DTOutput(outputId = "details"),
      column(6,actionButton("upd_deets","Update")),
      fluidRow(column(1,actionButton("sell","   Sell   ")),
               column(1,actionButton("buy","   Buy   ")),
               column(1,actionButton("del","  Delete  "))
               ),
      fluidRow(tableOutput("test")),
      fluidRow(tableOutput("test2")),
      textOutput("test3")
      
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
    
    db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    
  datexist <- dbGetQuery(db,"select * from fabro_fixd") %>% 
    select(-timestamp) %>% 
    filter(as.Date(date)<=d2 & as.Date(date)>=d1)
    
  sched <- data.frame(
    staff="",
    date=as.character(days),
    stringsAsFactors = FALSE
  ) %>% 
    bind_rows(datexist) %>% 
    replace(., is.na(.), "") %>% 
    spread(date,shift) %>% 
    right_join(data.frame(staff=names,stringsAsFactors = FALSE),by="staff")
  
  values$df <- sched %>% 
    rename("Staff"=staff) %>% 
    replace(., is.na(.), "") %>% 
    unite(Hours,-Staff,sep="|",remove=FALSE) %>% 
    mutate(Hours=if_else(str_replace_all(Hours,"[\\|]","")=="","0",Hours)) %>% 
    mutate(Hours=str_replace_all(Hours, "[\\|]", "+")) %>% 
    mutate(Hours=paste(Hours, "0", sep="+")) %>% #in case there's a leftover + sign 
    rowwise() %>% 
    mutate(Hours=-1*eval(parse(text=Hours)))

  values$df2 <- dbGetQuery(db,"select * from fabro_temp") %>% 
    rename(
      "Shift_Date"=shift_date,
      "Shift_Time"=shift_time,
      "Seller"=seller,
      "Seller_Comments"=seller_comments,
      "Buyer"=buyer,
    ) %>% 
    select(-timestamp) 
   #data.frame(
   #Shift_Date=character(),
   #Shift_Time=character(),
   #Seller=character(),
   #Seller_Comments=character(),
   #Buyer=character(), #this should be a comma separated list
   #stringsAsFactors = FALSE)
  
  
  dbDisconnect(db)
  
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
  
  observeEvent(input$upd_sched,{
    
    sched <- values$df %>% 
      select(-Hours) %>% 
      gather(Date,Shift,-Staff) %>% 
      filter(Shift!=""&is.null(Shift)==FALSE)
    now <- as.POSIXlt(Sys.time())
    now.str <- format(now,'%Y-%m-%d %H:%M:%S')
    if(nrow(sched)>0) {
    sched$timestamp <- now.str
    
    dbup(sched,"insert into fabro_fixd values","delete b from fabro_fixd a inner join fabro_fixd b on a.staff=b.staff and a.date=b.date and a.timestamp > b.timestamp")
    
    showNotification("Update successful!")
    }
    else {
      showNotification("No data to update!")
    }
    
  })
  
  
  observeEvent(input$upd_deets,{
    
    now <- as.POSIXlt(Sys.time())
    now.str <- format(now,'%Y-%m-%d %H:%M:%S')
    
    deets <- values$df2
    
    if(nrow(deets)>0) {
      deets$timestamp <- now.str
      
      dbup(deets,"insert into fabro_temp values","delete b from fabro_temp a inner join fabro_temp b on a.shift_date=b.shift_date and a.seller=b.seller and a.timestamp > b.timestamp")
      
      showNotification("Update successful!")
    }
    else {
      showNotification("No data to update!")
    }
    
  })
  
  observeEvent(input$del,{
    
    validate(need(input$user==boss,"You are not authorized!"))
    
    db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    
    dbGetQuery(db,"delete from fabro_temp")
    
    dbDisconnect(db)
    
    showNotification("Delete successful - reload page")
    
  })

  
  output$test<- renderTable({
   # test <- values$df %>% 
   #   select(-Hours) %>% 
   #   gather(Date,Shift,-Staff) %>% 
   #   filter(Shift!=""&is.null(Shift)==FALSE)
  })
  
  output$test2 <- renderTable({
    
   # query <- "create table test_t(staff varchar(5) DEFAULT 'VLAD', shift varchar(20), timestamp datetime)"
   # query <- "create table fabro_fixd (staff varchar(10), date varchar(10), shift varchar(20), timestamp datetime)"
   # query <- "create table fabro_temp (shift_date varchar(10),	shift_time varchar(20),	seller varchar(10),	seller_comments varchar(50),	buyer varchar(10), timestamp datetime)"
   # query    <-"(select *, row_number() over (partition by staff, shift order by timestamp desc) as rnk from test_t where rnk=1"
   # query    <-"delete b from test_t a inner join test_t b on a.staff=b.staff and a.shift=b.shift and a.timestamp > b.timestamp"
   # query    <- "select * from fabro_fixd"
   # 
   # db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
   #                 port = options()$mysql$port, user = options()$mysql$user, 
   #                 password = options()$mysql$password) 
   # data <- dbGetQuery(db, query)
   # dbDisconnect(db)
   # 
   # data
  })
  
  output$test3 <- renderText({

  })
  
  
  
}


shinyApp(ui, server)