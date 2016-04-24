library(shiny)
library(data.table)

calc_mzp <- function(q330, kat, obd) {
    coeffs = list(leto = c(0.65, 0.8, 0.85, 0.9), zima = c(0.85, 1, 1, 1))
    season = "leto"
    if (obd == 22)
        season = "zima"
    exponent = 0.85
    if (q330<1) exponent2=1.09 else exponent2=1
    return(((q330^exponent)^exponent2)*coeffs[[season]][as.numeric(kat)])
}

shinyServer(function(input, output) {
new_mzp <- reactive({ calc_mzp(input$n, input$kat, input$rok) })
old_mzp <- reactive({
    X330 <- input$n
    X355 <- input$nn
    X364 <- input$nnn

    if (X355 < 0.05) return(X330)
    else if (X355 < 0.5) return((X330 + X355) * 0.5)
    else if (X355 < 5) return(X355)
    else return((X355 + X364) * 0.5)
})

  output$plot <- renderPlot({
    q330 <- input$n
    mzp = new_mzp()
    
    rada=seq((q330-0.9*q330),(q330+0.9*q330),by=q330/50)
    mzp1=rep(0,91)
    mzp1 = sapply(rada, calc_mzp, input$kat, input$rok)
    
    plot(rada, mzp1, type='l', lty=2, lwd=2, xlab='Q330', ylab='MZP')
    abline(coef=c(mzp,0), col='red',lwd=1, lty=3)
    points(q330,mzp, col='red',lwd=2)
    points(q330, old_mzp(), col='dark blue', lwd=2)
    grid()
})
  
output$summary <- renderPrint({
    print(c(new_mzp(), old_mzp()))
})
})
