library(shiny)
library(xlsx)
library(rhandsontable)

ui <- fluidPage(  
    titlePanel("Scores"),  
    sidebarLayout(    
        sidebarPanel(      
            fileInput("file", "Upload the file"),      
            br(),     
            downloadButton('downloadData', 'Save as excel')   
        ),    
        mainPanel(uiOutput("op"))  
      )
    )

server <- function(input, output, session) {
    data <- reactive({  
        file1 <- input$file   
        if (is.null(file1)) {
                  return()   
        }
          read.csv(file = file1$datapath) 
    })  

    fun1 <- function(x) {
            mydf <- data()
            DF <- mydf[(mydf$Grade == x), c(1:3)]
            table <- renderRHandsontable({
                  newtable<- rhandsontable(DF, rowHeaders = NULL)
            })
           tabPanel(x, table)
    }

    output$op <- renderUI({
            if (is.null(data()))
                 helpText("File not uploaded!")
            else{
                mydf <- data()
                Tabs <- lapply((unique(mydf$Grade)), fun1)
                do.call(tabsetPanel, c(id = "tabs", Tabs))
        }
    })

# Need to make changes in the following section so as to download the edited tables
    output$downloadData <- downloadHandler(
       filename = function() {
          'Edited table.xls'
       },
        # what should go in place of editedTable
       content = function(file) {
           write.xlsx(editedTable, file)
        }
     )  
   }

shinyApp(ui, server)
