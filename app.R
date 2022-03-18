library(shiny)
library(truncnorm)
library(dplyr)
library(purrr)
library(readr)
blarg <- read_csv("Tradition_Lottery.csv") %>% 
  rename(names = 1, weight = 2)
  
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("Chapel Hill Country Club Tradition Tournament Draw"),
  
  sidebarLayout(position = "left",
                sidebarPanel(
                  numericInput("year", h3("Enter Year"), value = 2018),
                  numericInput("slots",h3("Available Places"),value = 40),
                  
                  actionButton("goButton", "Go!")
                  
                             ),
                mainPanel(
                  tableOutput("tabs")
                )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  blarg_a_func <- eventReactive(input$goButton,
    {blarg %>% 
        mutate(row_n = row_number()) %>% 
        split(.$row_n)})
      
      
  output$tabs <- renderTable({ 
    
    seeding <- isolate(input$year)
    isolate(input$slots)
    
    ## Set seed taken from user input 
    set.seed(seeding)
    
    blarg_a <- blarg_a_func()
    
    blarg2 <- map(blarg_a, ~ {
      rep(.x$names, .x$weight)
    }) %>% unname() %>% unlist()
    
    
    # Take a random draw
    set.seed(seeding)
    blarg3 <- sample(blarg2)
    
    pick_selection_order <- unique(blarg3)
    
    data.frame(Name = pick_selection_order[1:isolate(input$slots)]) %>% 
      mutate(row_n = row_number())
    
    })
}

shinyApp(ui = ui, server = server)