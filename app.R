ui <- fluidPage(
  
  selectInput("cats1", "Category 1 level", unique(category_level_1_sl$category_full_slug)),
  numericInput("nrows", "Enter the number of rows to display:", 10),
  numericInput("proc", "Наценка:", 1.3),
  numericInput("rows", "Count items", 1),
  actionButton("do", "Upload items"),
  textOutput("numb"),
  tableOutput("tbl"),
  tableOutput("tbl2")
  
)

server <- function(input, output, session) {
  
  output$tbl <- renderTable({
    
    DT <- data.table(
      
      
      
    )
    
    return(DT)
    
  })
  
  output$tbl2 <- renderTable({
    
    data.table(
      
      dbGetQuery(pool, paste0('select sid, name, stock_0, stock_1, price from "',
                              input$cats1,
                              '" limit ',
                              input$nrows,
                              ';'))
      
    )
    
  })
  
  uploadItems <- eventReactive(input$do, {
    
    pool <- dbPool(PostgreSQL(), 
                   user = "postgres", 
                   password = NULL, 
                   dbname = "catalog_Flando_(For_Kirill_Odincov)", 
                   host = "localhost",
                   maxSize = 1,
                   idleTimeout = 1,
                   validationInterval = 0) # Connect to PostgreSQL
    
    on.exit(poolClose(pool), add = T)
    
    lst <- map(1:input$rows, \(j) {
      
      DT <- data.table(dbReadTable(pool, input$cats1))
      
      n <- which(unique(category_level_1_sl$category_full_slug) == input$cats1)
      
      create_item_is(i = j, tb_nms = n, tbl = DT, val = input$proc)
      
    })
    
    print("Upload files completed")
    
  })
  
  output$numb <- renderText(uploadItems())
  
}

shinyApp(ui = ui, server = server)