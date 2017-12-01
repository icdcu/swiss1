library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("SWISS1: EHR navigation pattern"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
      # Sidebar panel for inputs ----
      sidebarPanel(
      
         # Input: Selector for choosing dataset ----
         selectInput(inputId = "pid",
                  label = "Physician:",
                  choices = c("all", "P1", "P2", "P3","P4", "P5", "P6", 
                              "P7", "P8", "P9", "P10", "P11", "P12", 
                              "P13", "P14", "P15", "P16", "P17", 
                              "P18", "P19", "P20", "P21", "P22", 
                              "P23", "P24" , "P25", "P26", "P27", 
                              "P28", "P29", "P30"),selected = "all"),
         selectInput(inputId = "case_id",
                  label = "Case id:",
                  choices = c("all", "MF", "AM", "JL", "DP", "AL", "JA",
                              "ZS", "MR"),selected = "all"),
         selectInput(inputId = "obsv_seq",
                  label = "Observation sequence:",
                  choices = c("all", "1", "2", "3", "4", "5", "6"), selected = "all")
      #####ctrl + shift + c (to select and unselect for "#")
      # selectInput(inputId = "catg_nodup_seq",
      #             label = "Choose an Event:",
      #             choices = c("A", "B", "C", "D", "E", "F",
      #                         "G", "H", "I", "J", "K", "L", "M",
      #                         "N", "O")),
      
      # Input: Numeric entry for number of obs to view ----
      #numericInput(inputId = " ? ",
      #            label = "Number of observations to view:",
      #           value = 10)
    ),
    
 # Main panel for displaying outputs ----
 mainPanel(
      
      # Output: Verbatim text for data summary ----
      #verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")),
      
 position = "left"
  )
)

# Define server logic to summarize and view selected dataset ----



##??linking BASE to the reactive output

server <- function(input, output) {

  BASE <- read.csv("base.csv")

  Test <- reactiveValues(Active = BASE)
  
  output$view <- renderTable({ Test$Active
    
    } )
  
  ##uaing subset function (check 2nd line below)
  # observeEvent(input$pid, {
  #   Test$Active = BASE[BASE$pid==input$pid,c(3,4,6,8)]
  #   # Test$Active = subset(BASE, pid == input$pid)
  #   print(input$pid)
  # })
  
### observeEvent(c(input$pid, input$case_id, input$obsv_seq), {
  #   Test$Active = BASE[BASE$pid==input$pid & 
  #                        BASE$case_id==input$case_id &
  #                        BASE$obsv_seq==input$obsv_seq,
  #                      c(4,5,7,9)]
  
  observeEvent(c(input$pid, input$case_id, input$obsv_seq), {
    Test$Active = BASE[eval(parse(text=ifelse(input$pid == "all", "TRUE", "BASE$pid==input$pid")))
      &
        eval(parse(text=ifelse(input$case_id == "all", "TRUE", "BASE$case_id==input$case_id")))
      &
        eval(parse(text=ifelse(input$obsv_seq == "all", "TRUE", "BASE$obsv_seq==input$obsv_seq"))),
      c(4,5,7,9)]
  })
  
  
  # # Return the requested dataset ----
  # datasetInput <- reactive({
  #   switch(input$pid,
  #          "P1" = P1,
  #          "P2" = P2,
  #          "P3" = P3
  #          "P4" = P4)
  # })
  # 
  # # Generate a summary of the dataset ----
  # output$summary <- renderPrint({
  #   dataset <- datasetInput()
  #   summary(dataset)
  # })
  # 
  # # Show the first "n" observations ----
  # output$view <- renderTable({datasetInput()
  # })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)