#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      radioButtons("p","select x-axis",list("wday_travel"='a1', "month_travel"='b1',
                                            "hour_travel"='c1',"minute_travel"='d1',"status" = 'e1',"num_seats_booked"= 'f1',"charged_free_rides"='g1'
                                            ,"booking_type"= 'h1',"commuter_id" = 'i1',"gender" = 'j1',"trip_type" = 'k1', "busid"  = 'l1')),
      
      radioButtons("q","select y-axis",list("wday_travel"='a2', "month_travel"='b2',
                                            "hour_travel"='c2',"minute_travel"='d2',"status" = 'e2',"num_seats_booked"= 'f2',"charged_free_rides"='g2'
                                            ,"booking_type"= 'h2',"commuter_id" = 'i2',"gender" = 'j2',"trip_type" = 'k2', "busid"  = 'l2')),
      
      
      radioButtons("r","select color varaible",list("trip_type" ='a3',"gender" = 'b3', "status" = 'c3'
                                                    ,"wday_travel"='d3',"month_travel"='e3')),
      
      
      
      radioButtons("s","select grid variable ",list("trip_type" ='a4',"gender" = 'b4', "status" = 'c4'
                                                    ,"wday_travel"='d4',"month_travel"='e4',"minute_travel"='f4',"hour_travel"='g4')),
      
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE)
      
    ),

    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel(title = "Data", tableOutput("contents")),
                  tabPanel(title = "summary", verbatimTextOutput("summ")),
                  tabPanel(title = "Plot",plotOutput("plot")),
                  tabPanel(title = "plot2", plotOutput("plot2")),
                  tabPanel(title = "plot3", plotOutput("plot3")),
                  tabPanel(title = "plot4", plotOutput("plot4")),
                  tabPanel(title = "plot5", plotOutput("plot5")),
                  tabPanel(title = "plot6", plotOutput("plot6"))
                  
                  
      )
    )
  ))












# Define server logic to read selected file ----


server <- function(input, output) {
  
  
  
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    c =  read.csv(file =file1$datapath, header = input$header)
    c
    ###
    
    
    
    
    
  })
  
  
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    if(is.null(data())){return ()}
    else{
      head(read.csv( file = input$file$datapath))
      
    }
  })
  
  
  
  
  output$summ<- renderPrint({
    if(is.null(data())){return ()}
    else{ 
      
      summary(data()) }
    
  })
  
  
  
  
  output$plot <- renderPlot({
    
    if(is.null(data())){return ()}
    else
    {
      
      if(input$p=='a1'){ i<-1}
      
      if(input$p=='b1'){ i<-2 }
      
      if(input$p=='c1'){i<-3}
      
      if(input$p=='d1'){i<-4}
      
      
      if(input$p=='e1'){ i<-7}
      
      if(input$p=='f1'){ i<-8 }
      
      if(input$p=='g1'){i<-9}
      
      if(input$p=='h1'){i<-10}
      
      if(input$p=='i1'){i<-11}
      
      if(input$p=='j1'){ i<-12}
      
      if(input$p=='k1'){ i<-13}
      
      if(input$p=='l1'){i<-14}
      
      
      
      
      if(input$q=='a2'){ j<-1}
      
      if(input$q=='b2'){ j<-2 }
      
      if(input$q=='c2'){j<-3}
      
      if(input$q=='d2'){j<-4}
      
      
      if(input$q=='e2'){ j<-7}
      
      if(input$q=='f2'){ j<-8 }
      
      if(input$q=='g2'){j<-9}
      
      if(input$q=='h2'){j<-10}
      
      if(input$r=='i2'){j<-11}
      
      if(input$q=='j2'){ j<-12}
      
      if(input$q=='k2'){ j<-13}
      
      if(input$q=='l2'){j<-14}
      
      
      
      if(input$r=='a3'){ z<-13}
      
      if(input$r=='b3'){ z<-12}
      
      if(input$r=='c3'){z<-7}
      
      if(input$r=='d3'){z<-1}
      
      if(input$r=='e3'){z<-2}
      
      
      
      
      df = data()
      library(ggplot2)
      ggplot(data =df,mapping =aes(x =df[,i],y =df[,j], color =  as.factor(df[,z])))+
        geom_jitter()+
        labs(x = "x-axis", y = "y-axis")
      
    }
    
  })
  
  
  output$plot2 = renderPlot({
    
    if(is.null(data())){return ()}
    else
    {
      
      if(input$p=='a1'){ i<-1}
      
      if(input$p=='b1'){ i<-2 }
      
      if(input$p=='c1'){i<-3}
      
      if(input$p=='d1'){i<-4}
      
      
      if(input$p=='e1'){ i<-7}
      
      if(input$p=='f1'){ i<-8 }
      
      if(input$p=='g1'){i<-9}
      
      if(input$p=='h1'){i<-10}
      
      if(input$p=='i1'){i<-11}
      
      if(input$p=='j1'){ i<-12}
      
      if(input$p=='k1'){ i<-13}
      
      if(input$p=='l1'){i<-14}
      
      
      
      
      if(input$q=='a2'){ j<-1}
      
      if(input$q=='b2'){ j<-2 }
      
      if(input$q=='c2'){j<-3}
      
      if(input$q=='d2'){j<-4}
      
      
      if(input$q=='e2'){ j<-7}
      
      if(input$q=='f2'){ j<-8 }
      
      if(input$q=='g2'){j<-9}
      
      if(input$q=='h2'){j<-10}
      
      if(input$r=='i2'){j<-11}
      
      if(input$q=='j2'){ j<-12}
      
      if(input$q=='k2'){ j<-13}
      
      if(input$q=='l2'){j<-14}
      
      
      
      if(input$r=='a3'){ z<-13}
      
      if(input$r=='b3'){ z<-12}
      
      if(input$r=='c3'){z<-7}
      
      
      
      
      
      
      
      
      
      
      df = data()
      
      library(ggplot2)
      
      
      
      hist(df[,i],col = "blue",xlab= "x-axis", main = "Histogram - median(red),mean(green)")
      rug(iris$Sepal.Length)
      abline(v = median(df[,i]), col = "red", lwd = 2)
      abline(v = mean(df[,i]), col = "green", lwd = 2)
      
      
      
      
      
      
      
      
      
      
    } 
    
  })
  
  
  output$plot3 = renderPlot({
    
    if(is.null(data())){return ()}
    else
    {
      
      if(input$p=='a1'){ i<-1}
      
      if(input$p=='b1'){ i<-2 }
      
      if(input$p=='c1'){i<-3}
      
      if(input$p=='d1'){i<-4}
      
      
      if(input$p=='e1'){ i<-7}
      
      if(input$p=='f1'){ i<-8 }
      
      if(input$p=='g1'){i<-9}
      
      if(input$p=='h1'){i<-10}
      
      if(input$p=='i1'){i<-11}
      
      if(input$p=='j1'){ i<-12}
      
      if(input$p=='k1'){ i<-13}
      
      if(input$p=='l1'){i<-14}
      
      
      
      
      if(input$q=='a2'){ j<-1}
      
      if(input$q=='b2'){ j<-2 }
      
      if(input$q=='c2'){j<-3}
      
      if(input$q=='d2'){j<-4}
      
      
      if(input$q=='e2'){ j<-7}
      
      if(input$q=='f2'){ j<-8 }
      
      if(input$q=='g2'){j<-9}
      
      if(input$q=='h2'){j<-10}
      
      if(input$r=='i2'){j<-11}
      
      if(input$q=='j2'){ j<-12}
      
      if(input$q=='k2'){ j<-13}
      
      if(input$q=='l2'){j<-14}
      
      
      
      if(input$r=='a3'){ z<-13}
      
      if(input$r=='b3'){ z<-12}
      
      if(input$r=='c3'){z<-7}
      
      if(input$r=='d3'){z<-1}
      
      if(input$r=='e3'){z<-2}
      
      df = data()
      
      library(ggplot2)
      
      
      
      ggplot(data(),mapping =aes(x = df[,i], fill =  as.factor(df[,z])))+
        geom_bar(position = "dodge")+
        labs(x = "x-axis")
    } 
    
  })
  
  output$plot4 = renderPlot({
    
    if(is.null(data())){return ()}
    else
    {
      if(input$p=='a1'){ i<-1}
      
      if(input$p=='b1'){ i<-2 }
      
      if(input$p=='c1'){i<-3}
      
      if(input$p=='d1'){i<-4}
      
      
      if(input$p=='e1'){ i<-7}
      
      if(input$p=='f1'){ i<-8 }
      
      if(input$p=='g1'){i<-9}
      
      if(input$p=='h1'){i<-10}
      
      if(input$p=='i1'){i<-11}
      
      if(input$p=='j1'){ i<-12}
      
      if(input$p=='k1'){ i<-13}
      
      if(input$p=='l1'){i<-14}
      
      
      
      
      if(input$q=='a2'){ j<-1}
      
      if(input$q=='b2'){ j<-2 }
      
      if(input$q=='c2'){j<-3}
      
      if(input$q=='d2'){j<-4}
      
      
      if(input$q=='e2'){ j<-7}
      
      if(input$q=='f2'){ j<-8 }
      
      if(input$q=='g2'){j<-9}
      
      if(input$q=='h2'){j<-10}
      
      if(input$r=='i2'){j<-11}
      
      if(input$q=='j2'){ j<-12}
      
      if(input$q=='k2'){ j<-13}
      
      if(input$q=='l2'){j<-14}
      
      
      
      if(input$r=='a3'){ z<-13}
      
      if(input$r=='b3'){ z<-12}
      
      if(input$r=='c3'){z<-7}
      
      if(input$r=='d3'){z<-1}
      
      if(input$r=='e3'){z<-2}
      
      
      
      
      
      
      
      
      
      df = data()
      
      library(ggplot2)
      
      ggplot( data(),mapping =aes(x = df[,i] ,y =df[,j],color =as.factor(df[,z])))+
        geom_jitter(size = 2 )+
        labs(x = "x-axis", y = "y-axis")
    } 
    
  })
  
  
  output$plot5 = renderPlot({
    
    if(is.null(data())){return ()}
    else
    {
      
      if(input$p=='a1'){ i<-1}
      
      if(input$p=='b1'){ i<-2 }
      
      if(input$p=='c1'){i<-3}
      
      if(input$p=='d1'){i<-4}
      
      
      if(input$p=='e1'){ i<-7}
      
      if(input$p=='f1'){ i<-8 }
      
      if(input$p=='g1'){i<-9}
      
      if(input$p=='h1'){i<-10}
      
      if(input$p=='i1'){i<-11}
      
      if(input$p=='j1'){ i<-12}
      
      if(input$p=='k1'){ i<-13}
      
      if(input$p=='l1'){i<-14}
      
      
      
      
      if(input$q=='a2'){ j<-1}
      
      if(input$q=='b2'){ j<-2 }
      
      if(input$q=='c2'){j<-3}
      
      if(input$q=='d2'){j<-4}
      
      
      if(input$q=='e2'){ j<-7}
      
      if(input$q=='f2'){ j<-8 }
      
      if(input$q=='g2'){j<-9}
      
      if(input$q=='h2'){j<-10}
      
      if(input$r=='i2'){j<-11}
      
      if(input$q=='j2'){ j<-12}
      
      if(input$q=='k2'){ j<-13}
      
      if(input$q=='l2'){j<-14}
      
      
      
      if(input$r=='a3'){ z<-13}
      
      if(input$r=='b3'){ z<-12}
      
      if(input$r=='c3'){z<-7}
      
      if(input$r=='d3'){z<-1}
      
      if(input$r=='e3'){z<-2}
      
      
      
      
      
      
      
      
      
      
      df = data()
      
      library(ggplot2)
      
      
      
      
      ggplot(data(),mapping =aes(x = df[,i], fill = df[,z]))+
        geom_density()+
        labs(x = "x-axis", y = "y-axis")
    } 
    
  })
  
  
  output$plot6 = renderPlot ({
    
    if(is.null(data())){return ()}
    else
    {
      
      if(input$p=='a1'){ i<-1}
      
      if(input$p=='b1'){ i<-2 }
      
      if(input$p=='c1'){i<-3}
      
      if(input$p=='d1'){i<-4}
      
      
      if(input$p=='e1'){ i<-7}
      
      if(input$p=='f1'){ i<-8 }
      
      if(input$p=='g1'){i<-9}
      
      if(input$p=='h1'){i<-10}
      
      if(input$p=='i1'){i<-11}
      
      if(input$p=='j1'){ i<-12}
      
      if(input$p=='k1'){ i<-13}
      
      if(input$p=='l1'){i<-14}
      
      
      
      
      if(input$q=='a2'){ j<-1}
      
      if(input$q=='b2'){ j<-2 }
      
      if(input$q=='c2'){j<-3}
      
      if(input$q=='d2'){j<-4}
      
      
      if(input$q=='e2'){ j<-7}
      
      if(input$q=='f2'){ j<-8 }
      
      if(input$q=='g2'){j<-9}
      
      if(input$q=='h2'){j<-10}
      
      if(input$r=='i2'){j<-11}
      
      if(input$q=='j2'){ j<-12}
      
      if(input$q=='k2'){ j<-13}
      
      if(input$q=='l2'){j<-14}
      
      
      
      if(input$r=='a3'){ z<-13}
      
      if(input$r=='b3'){ z<-12}
      
      if(input$r=='c3'){z<-7}
      
      if(input$r=='d3'){z<-1}
      
      if(input$r=='e3'){z<-2}
      
      
      
      if(input$s=='a4'){ x<-13}
      
      if(input$s=='b4'){ x<-12}
      
      if(input$s=='c4'){x<-7}
      
      if(input$s=='d4'){x<-1}
      
      if(input$s=='e4'){x<-2}
      
      
      if(input$s=='f4'){x<-4}
      
      
      
      if(input$s=='g4'){x<-3}
      
      
      
      
      
      
      
      
      
      df = data()
      
      library(ggplot2)
      
      
      
      
      ggplot(data(),mapping =aes(x = df[,i], fill = as.factor(df[,z])))+
        geom_bar()+
        facet_grid(~df[,x])+
        labs(x = "x-axis", y = "y-axis", title = "BAR PLOT -Facetting")
      
      
    } 
    
  })
  
  
  
  
}


library(rsconnect)

#rsconnect::setAccountInfo(name='jatinpal',
                         # token='384522A89B2ABC0B4DF767B1EF17B594',
                          #secret='e9kCt91AwZmNdlFo8bCVnCijaZFhz30WA9qIg92n')

#sconnect::setAccountInfo(name='jatinpal',
                          #oken='518686D160EDA47C0414010BE65145DA',
                          #ecret='ZDh6B9WWfak6yuShpZjnOl4Oc1aLnSfGVEdvhTQT')



rsconnect::setAccountInfo(name='jatinpal',
                          token='384522A89B2ABC0B4DF767B1EF17B594',
                          secret='e9kCt91AwZmNdlFo8bCVnCijaZFhz30WA9qIg92n')

deployApp('jatinapp')
#runApp('jatinapp')
# Run the application 
shinyApp(ui = ui, server = server)
###

rsconnect::AppDeploy('jatinapp')


quit()
