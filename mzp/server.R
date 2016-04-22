library(shiny)
library(data.table)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output functions defined 
  # below then all use the value computed from this expression
  
  data <- reactive({
    dist <- switch(input$kat,
                   jedna = 1,
                   dva = 2,
                   tri = 3,
                   ctyr = 4,
                   rnorm)
    
    dist(input$n)
  })
  
  data <- reactive({
    dist <- switch(input$rok,
                   leto = 11,
                   zima = 22,
                   rnorm)
    dist(input$n)
    
    })
  
#   data <- reactive({
#     dist <- switch(input$kat,
#                    norm = rnorm,
#                    unif = runif,
#                    lnorm = rlnorm,
#                    exp = rexp,
#                    rnorm)
#     
#     dist(input$n)
#   })
#   
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
    kat <- input$kat
    X330 = q330 <- input$n
    obd = input$rok
    X355<- input$nn
    X364<- input$nnn
    
    #X355=0.02
    #X330=0.5
    #X364=4
    
    old=data.table(X355)
    
    old[X355<0.05, MZPold:=X330]
    old[X355>0.05 & X355<0.5, MZPold:=(X330+X355)*0.5]
    old[X355>0.5 & X355<5, MZPold:=X355]
    old[X355>5, MZPold:=(X355+X364)*0.5]
    
    exponent=0.85
    
    if (q330<1) exponent1=1.09 else exponent1=1
    
    
    if (kat==1 & obd==11) mzp=((q330^exponent)^exponent1)*0.65
    if (kat==1 & obd==22) mzp=((q330^exponent)^exponent1)*0.85
    if (kat==2 & obd==11) mzp=((q330^exponent)^exponent1)*0.8
    if (kat==2 & obd==22) mzp=((q330^exponent)^exponent1)*1
    if (kat==3 & obd==11) mzp=((q330^exponent)^exponent1)*.85
    if (kat==3 & obd==22) mzp=((q330^exponent)^exponent1)*1
    if (kat==4 & obd==11) mzp=((q330^exponent)^exponent1)*.9
    if (kat==4 & obd==22) mzp=((q330^exponent)^exponent1)*1
    
    rada=seq((q330-0.9*q330),(q330+0.9*q330),by=q330/50)
    
    #length(rada)
    
    mzp1=rep(0,91)
    
    for (i in 1:91)
    {
    
      exponent=0.85
      
      if (rada[i]<1) exponent1=1.09 else exponent1=1
      
      
    if (kat==1 & obd==11) mzp1[i]=(((rada[i])^exponent)^exponent1)*0.65
    if (kat==1 & obd==22) mzp1[i]=(((rada[i])^exponent)^exponent1)*0.85
    if (kat==2 & obd==11) mzp1[i]=(((rada[i])^exponent)^exponent1)*0.8
    if (kat==2 & obd==22) mzp1[i]=(((rada[i])^exponent)^exponent1)*1
    if (kat==3 & obd==11) mzp1[i]=(((rada[i])^exponent)^exponent1)*.85
    if (kat==3 & obd==22) mzp1[i]=(((rada[i])^exponent)^exponent1)*1
    if (kat==4 & obd==11) mzp1[i]=(((rada[i])^exponent)^exponent1)*.9
    if (kat==4 & obd==22) mzp1[i]=(((rada[i])^exponent)^exponent1)*1
    }
    
    bod=old$MZPold
    
    plot(rada, mzp1, type='l', lty=2, lwd=2, xlab='Q330', ylab='MZP')
    abline(coef=c(mzp,0), col='red',lwd=1, lty=3)
    points(q330,mzp, col='red',lwd=2)
    points(q330,bod,col='dark blue', lwd=2)
    grid()
    #hist(data(), 
      #   main=paste('r', dist, '(', n, ')', sep=''))
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    
    kat <- input$kat
    X330=q330 <- input$n
    obd = input$rok
    exponent=0.85
    X355<- input$nn
    X364<- input$nnn
    
    #X355=0.02
    #X330=0.5
    #X364=4
    
    old=data.table(X355)
    
    old[X355<0.05, MZPold:=X330]
    old[X355>0.05 & X355<0.5, MZPold:=(X330+X355)*0.5]
    old[X355>0.5 & X355<5, MZPold:=X355]
    old[X355>5, MZPold:=(X355+X364)*0.5]
    
    if (q330<1) exponent1=1.09 else exponent1=1
    
    if (kat==1 & obd==11) mzp=((q330^exponent)^exponent1)*0.65
    if (kat==1 & obd==22) mzp=((q330^exponent)^exponent1)*0.85
    if (kat==2 & obd==11) mzp=((q330^exponent)^exponent1)*0.8
    if (kat==2 & obd==22) mzp=((q330^exponent)^exponent1)*1
    if (kat==3 & obd==11) mzp=((q330^exponent)^exponent1)*.85
    if (kat==3 & obd==22) mzp=((q330^exponent)^exponent1)*1
    if (kat==4 & obd==11) mzp=((q330^exponent)^exponent1)*.9
    if (kat==4 & obd==22) mzp=((q330^exponent)^exponent1)*1
    
    bod=old$MZPold
    print(c(mzp,bod))
  })
  
  # Generate an HTML table view of the data
#   output$table <- renderTable({
#     data.frame(x=data())
#   })
  
})