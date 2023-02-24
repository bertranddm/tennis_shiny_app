
library(shiny)
library(DT)

ui <- fluidPage(
   titlePanel("Tennishelpdata"),
   
    verticalLayout(
      splitLayout(
     
       sidebarLayout(
           sidebarPanel(selectInput("Player1",selected = "", label = "Player 1 chosen", choices = IDtbl$name, selectize = T)),
                   
             mainPanel(dataTableOutput("playerhist1"))
                  
    ),
       sidebarLayout(
           sidebarPanel(selectInput("Player2",selected = "", label = "Player 2 chosen", choices = IDtbl$name, selectize = T)),
  
              mainPanel(dataTableOutput("playerhist2"))
       )
   ),
   mainPanel(verbatimTextOutput(("probwin")),
             verbatimTextOutput("ID")
   )
))



server <- function(input, output) {
   
   output$playerhist1 <- DT::renderDataTable({(historytable(findID(input$Player1)))
   })
   output$playerhist2 <- DT::renderDataTable({(historytable(findID(input$Player2)))
   })
   output$probwin <- renderPrint(paste(input$Player1,":",probwin1(findID(input$Player1),findID(input$Player2)),"% chances of winning against",input$Player2,sep=" "))

   
}


shinyApp(ui = ui, server = server)

