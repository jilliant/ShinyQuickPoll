library(shiny)
library(plotly)
library(dplyr)

# Global ----

# which fields get saved 
fields <- c("activity")

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = TRUE)
}

# load all responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}

# directory where responses get stored
responsesDir <- file.path("response")

# App Start ----

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),

    title = "Pick an activity",
    div(id = "header",
        h1("Pick an activity")
    ),
    
    fluidRow(
      column(12,
             div(
               id = "form",
               
               selectInput("activity", "Choose an activity",
                           c("",  "Axe Throwing", "Mini Golf", "Laser Tag")),
               actionButton("submit", "Submit", class = "btn-primary"),
               
               shinyjs::hidden(
                 span(id = "submit_msg", "Submitting..."),
                 div(id = "error",
                     div(br(), tags$b("Error: "), span(id = "error_msg"))
                 )
               )
             ),
             
             shinyjs::hidden(
               div(
                 id = "thankyou_msg",
                 h3("Thanks, your response was submitted successfully!"),
                 actionLink("submit_another", "Submit another response")
               )
             )
      ),
      column(12,
             uiOutput("resultPanelContainer")
      )
    )
  ),
  server = function(input, output, session) {
    
    # Gather all the form inputs (and add timestamp)
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })    
    
    # When the Submit button is clicked, submit the response
    observeEvent(input$submit, {
      
      # User-experience stuff
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      # Save the data (show an error message in case of error)
      tryCatch({
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })
    
    # submit another response
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
    
    # render the result panel
     output$resultPanelContainer <- renderUI({
 
      div(
        id = "responsesPanel",
        h2("Previous responses"),
        plotlyOutput("responsesChart"),
        
         downloadButton("downloadBtn", "Download responses"), br(), br()
        #,
        # DT::dataTableOutput("responsesTable")
        )
    })
    
    # Show the responses in the admin table
    output$responsesTable <- DT::renderDataTable({
      data <- loadData()
      data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")
      DT::datatable(
        data,
        rownames = FALSE,
        options = list(searching = FALSE, lengthChange = FALSE)
      )
    })
    
    # Show the responses in the chart
    output$responsesChart <- renderPlotly({
      data <- loadData()
      data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")
    
      plotdata <- data %>%
        count(activity)
      
      p <- plot_ly(
        x = plotdata$activity,
        y = plotdata$n,
        color = plotdata$activity, 
        name = "Results",
        type = "bar"
      )
    })
    
    
   # Allow user to download responses
    output$downloadBtn <- downloadHandler(
      filename = function() {
        sprintf("results_%s.csv", humanTime())
      },
      content = function(file) {
        write.csv(loadData(), file, row.names = FALSE)
      }
    )
  }
)