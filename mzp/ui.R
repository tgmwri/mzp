library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
    
  # Application title
  headerPanel(""),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(
    radioButtons("kat", "Kategorie povodí:",
                 c("I" = 1,
                   "II" = 2,
                   "III" = 3,
                   "IV" = 4)),
  
    
      radioButtons("rok", "Období:",
                   c("Léto" = 11,
                     "Zima" = 22)),
    
    
    numericInput('n', 'Hodnota Q330:', 1,
                 min = 0, max = 1000),
    
    
    numericInput('nn', 'Hodnota Q355:', 0.7,
                 min = 0, max = 1000),
    
    
    numericInput('nnn', 'Hodnota Q364:', 0.5,
                 min = 0, max = 1000)
    

  ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Hodnota MZP", verbatimTextOutput("summary")), 
      tabPanel("Graf", plotOutput("plot"))
    )
  )
))
