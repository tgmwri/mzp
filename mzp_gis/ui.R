library(shiny)

shinyUI(pageWithSidebar(
  headerPanel(""),
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
                 min = 0, max = 1000),
                 
    numericInput('nnnn', 'Hodnota Qa:', 3,
                 min = 0, max = 1000)
                 
    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Hodnota MZP", verbatimTextOutput("summary")), 
      tabPanel("Graf", plotOutput("plot")),
      tabPanel("Graf starý/nový", plotOutput("plot2")),
      tabPanel("Graf poměr", plotOutput("plot3")),
      tabPanel("GIS I", plotOutput("plot4")),
      tabPanel("GIS II", plotOutput("plot5")),
      tabPanel("GIS III", plotOutput("plot6")),
      tabPanel("GIS IV", plotOutput("plot7"))
    )
  )
))
